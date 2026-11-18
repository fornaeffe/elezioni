/** Feasible forecast candidates. They never demand unsupported national totals. */
import { forecastRandom } from "./forecast-random";
import { betaTransition } from "./transition";
import { projectMargins } from "./projection";
import { sum, requireWorkspace } from "./types";
import { fitRandom } from "./random";
export const FIELD_MODELS = [
  "filing-strata-beta-v1",
  "bounded-pair-field-v1",
  "bounded-mass-pair-field-v2",
] as const;
export type FieldModel = (typeof FIELD_MODELS)[number];
export interface FieldInput {
  model: FieldModel;
  reference: readonly Float64Array[];
  cells: readonly {
    id: string;
    region: string;
    electors: number;
    municipality?: string;
  }[];
  politicalScales: ArrayLike<number>;
  localScales: ArrayLike<number>;
  localMultipliers?: readonly ArrayLike<number>[];
  participationScale: number;
  discrepancy: number;
  discrepancyScales?: ArrayLike<number>;
  horizon: number;
  seed: string;
  simulationId: number;
  chamber?: "camera" | "senato";
  correlation?: number;
  /** Frozen research sensitivity, not fitted from confirmation results. */
  scaleMultiplier?: number;
}
export function feasibleField(input: FieldInput) {
  const { reference, cells } = input,
    n = reference[0]?.length ?? 0,
    ch = input.chamber ?? "camera",
    rho = input.correlation ?? 0,
    m = input.scaleMultiplier ?? 1;
  if (
    !FIELD_MODELS.includes(input.model) ||
    !reference.length ||
    reference.length !== cells.length ||
    n < 2 ||
    input.politicalScales.length !== n - 1 ||
    input.localScales.length !== n ||
    (input.discrepancyScales !== undefined &&
      (input.discrepancyScales.length !== n - 1 ||
        Array.from(input.discrepancyScales).some(
          (x) => !Number.isFinite(x) || x < 0,
        ))) ||
    (input.localMultipliers !== undefined &&
      (input.localMultipliers.length !== cells.length ||
        input.localMultipliers.some(
          (row) =>
            row.length !== n ||
            Array.from(row).some((x) => !Number.isFinite(x) || x < 0),
        ))) ||
    !Number.isFinite(input.horizon) ||
    input.horizon < 0 ||
    !Number.isFinite(m) ||
    m < 0 ||
    [
      input.participationScale,
      input.discrepancy,
      ...Array.from(input.politicalScales),
      ...Array.from(input.localScales),
    ].some((x) => !Number.isFinite(x) || x < 0) ||
    reference.some(
      (p) =>
        p.length !== n ||
        p.some((x) => !Number.isFinite(x) || x < 0) ||
        Math.abs(sum(p) - 1) > 1e-10,
    ) ||
    cells.some((c) => !Number.isFinite(c.electors) || c.electors < 0)
  )
    throw new Error("INVALID_FEASIBLE_FIELD");
  requireWorkspace(reference.length * n * 8 * 5 + n * n * 32);
  const p = reference.map((x) => x.slice());
  let residual = 0,
    iterations = 0;
  if (input.model === "filing-strata-beta-v1") {
    const groups = new Map<string, number[]>();
    reference.forEach((row, i) => {
      const key = Array.from(row, (_, j) => (row[j] > 0 ? j : -1))
        .filter((j) => j >= 0)
        .join(",");
      const group = groups.get(key) ?? [];
      group.push(i);
      groups.set(key, group);
    });
    for (const indices of groups.values()) {
      const mass = sum(indices.map((i) => cells[i].electors));
      if (!mass) continue;
      const b = new Float64Array(n);
      for (const i of indices)
        for (let j = 0; j < n; j++)
          b[j] += (cells[i].electors / mass) * reference[i][j];
      const valid = 1 - b[0];
      if (!(valid > 0)) continue;
      let q: Float64Array = b.slice(1).map((x) => x / valid);
      q = betaTransition(
        q,
        Array.from(input.politicalScales, (x) => x * m),
        input.horizon,
        forecastRandom(
          input.seed,
          input.simulationId,
          "strata-political",
          ch,
          rho,
        ),
      );
      q = betaTransition(
        q,
        Float64Array.from(
          { length: n - 1 },
          (_, j) => (input.discrepancyScales?.[j] ?? input.discrepancy) * m,
        ),
        1,
        forecastRandom(
          input.seed,
          input.simulationId,
          "strata-discrepancy",
          ch,
          rho,
        ),
      );
      const a = betaTransition(
        [b[0], valid],
        [input.participationScale * m, input.participationScale * m],
        input.horizon,
        forecastRandom(
          input.seed,
          input.simulationId,
          "strata-nonvalid",
          ch,
          rho,
        ),
      );
      const target = Float64Array.from([a[0], ...q.map((x) => a[1] * x)]),
        region = new Map<string, Float64Array>();
      for (const i of indices)
        if (!region.has(cells[i].region)) {
          const rng = forecastRandom(
            input.seed,
            input.simulationId,
            "strata-region:" + cells[i].region,
            ch,
            rho,
          );
          region.set(
            cells[i].region,
            Float64Array.from({ length: n }, () => rng.normal()),
          );
        }
      const scores = indices.map((i) => {
        const rng = forecastRandom(
          input.seed,
          input.simulationId,
          "strata-local:" + cells[i].id,
          ch,
          rho,
        );
        return Float64Array.from({ length: n }, (_, j) => {
          const z = rng.normal();
          return target[j] > 0 && reference[i][j] > 0
            ? Math.log(target[j]) +
                Math.log(reference[i][j]) -
                Math.log(b[j]) +
                m *
                  input.localScales[j] *
                  (input.localMultipliers?.[i][j] ?? 1) *
                  Math.sqrt(input.horizon) *
                  (Math.sqrt(0.85) * z +
                    Math.sqrt(0.15) * region.get(cells[i].region)![j])
            : -Infinity;
        });
      });
      const projected = projectMargins(target, indices.length, (i) => ({
        weight: cells[indices[i]].electors,
        scores: scores[i],
      }));
      residual = Math.max(residual, projected.residual);
      iterations = Math.max(iterations, projected.iterations);
      indices.forEach((i, r) => p[i].set(projected.probabilities(r)));
    }
  } else {
    const weighted = input.model === "bounded-mass-pair-field-v2",
      patternIndex = new Map<string, number>(),
      patternRows: number[][] = [],
      patternMembers: number[][] = [],
      rowPattern = new Int32Array(p.length);
    for (let i = 0; i < p.length; i++) {
      const members = Array.from({ length: n - 1 }, (_, j) => j + 1).filter(
          (j) => reference[i][j] > 0,
        ),
        key = members.join(",");
      let group = patternIndex.get(key);
      if (group === undefined) {
        group = patternRows.length;
        patternIndex.set(key, group);
        patternRows.push([]);
        patternMembers.push(members);
      }
      patternRows[group].push(i);
      rowPattern[i] = group;
    }
    // Each disjoint pair receives a bounded symmetric transfer. Its conditional
    // expectation is zero, including at boundaries. Political and turnout
    // innovations have separate streams. A common component field couples places.
    const phases = [
      { name: "movement", exposure: input.horizon, discrepancy: false },
      { name: "discrepancy", exposure: 1, discrepancy: true },
    ];
    for (const phase of phases) {
      if (!phase.exposure) continue;
      const steps = Math.ceil(phase.exposure / 0.25),
        dt = phase.exposure / steps;
      for (let step = 0; step < steps; step++) {
        const address = "pair-field:" + phase.name + ":" + step,
          global = forecastRandom(
            input.seed,
            input.simulationId,
            address + ":global",
            ch,
            rho,
          ),
          common = Float64Array.from({ length: n }, () => global.normal()),
          order = Array.from({ length: n - 1 }, (_, j) => j + 1),
          permutation = forecastRandom(
            input.seed,
            input.simulationId,
            address + ":order",
          );
        for (let j = order.length - 1; j > 0; j--) {
          const k = permutation.integer(j + 1);
          [order[j], order[k]] = [order[k], order[j]];
        }
        const basePriorities = weighted
          ? Float64Array.from(
              { length: n },
              () => -Math.log(permutation.uniform()),
            )
          : undefined;
        const activePatterns = patternMembers.map((members, g) => {
          if (!weighted) return order.filter((j) => members.includes(j));
          const mass = new Float64Array(n);
          for (const i of patternRows[g])
            for (const j of members) mass[j] += cells[i].electors * p[i][j];
          const priorities = new Map(
            members.map((j) => [
              j,
              basePriorities![j] / Math.max(1e-300, mass[j]),
            ]),
          );
          return [...members].sort(
            (a, b) => priorities.get(a)! - priorities.get(b)! || a - b,
          );
        });
        const geographicActive =
            !phase.discrepancy &&
            Array.from(input.localScales).some((x) => x > 0),
          commonShocks = Float64Array.from(
            { length: n },
            (_, j) =>
              m *
              Math.sqrt(dt) *
              (j === 0
                ? input.participationScale
                : phase.discrepancy
                  ? (input.discrepancyScales?.[j - 1] ?? input.discrepancy)
                  : input.politicalScales[j - 1]) *
              common[j],
          ),
          regional = new Map<string, Float64Array>(),
          localShocks = new Float64Array(n),
          rootRng = weighted
            ? fitRandom(
                input.seed,
                "mass-pair:" + input.simulationId + ":" + address,
              )
            : undefined,
          localRoot = rootRng
            ? rootRng.integer(4294967296) + ":" + rootRng.integer(4294967296)
            : "";
        for (let i = 0; i < p.length; i++) {
          const row = p[i],
            cell = cells[i];
          let rz = regional.get(cell.region);
          if (geographicActive && !rz) {
            const rng = forecastRandom(
              input.seed,
              input.simulationId,
              address + ":region:" + cell.region,
              ch,
              rho,
            );
            rz = Float64Array.from({ length: n }, () => rng.normal());
            regional.set(cell.region, rz);
          }
          const commonLocal =
              weighted && geographicActive
                ? fitRandom(
                    localRoot,
                    "local:" + (cell.municipality ?? cell.id),
                  )
                : undefined,
            residualLocal =
              commonLocal && ch === "senato"
                ? fitRandom(
                    localRoot,
                    "residual:" + (cell.municipality ?? cell.id),
                  )
                : undefined;
          const normal = geographicActive
            ? commonLocal
              ? ch === "camera"
                ? commonLocal.normal
                : () => {
                    const z = commonLocal.normal();
                    return (
                      rho * z +
                      Math.sqrt(1 - rho * rho) * residualLocal!.normal()
                    );
                  }
              : forecastRandom(
                  input.seed,
                  input.simulationId,
                  address + ":local:" + cell.id,
                  ch,
                  rho,
                ).normal
            : undefined;
          if (geographicActive)
            for (let j = 0; j < n; j++) {
              const local = normal!(),
                national =
                  j === 0
                    ? input.participationScale
                    : phase.discrepancy
                      ? (input.discrepancyScales?.[j - 1] ?? input.discrepancy)
                      : input.politicalScales[j - 1],
                geographic = phase.discrepancy
                  ? 0
                  : input.localScales[j] *
                    (input.localMultipliers?.[i][j] ?? 1);
              localShocks[j] =
                m *
                Math.sqrt(dt) *
                (national * common[j] +
                  geographic *
                    (Math.sqrt(0.15) * rz![j] + Math.sqrt(0.85) * local));
            }
          const shocks = geographicActive ? localShocks : commonShocks,
            a = row[0],
            v = 1 - a;
          if (!(v > 0)) continue;
          const active = activePatterns[rowPattern[i]];
          for (let t = 0; t + 1 < active.length; t += 2) {
            const j = active[t],
              k = active[t + 1],
              total = row[j] + row[k];
            if (total > 0) {
              const delta =
                ((row[j] * row[k]) / total) *
                Math.tanh((shocks[j] - shocks[k]) / Math.SQRT2);
              row[j] += delta;
              row[k] -= delta;
            }
          }
          if (!phase.discrepancy) {
            const delta = a * v * Math.tanh(shocks[0]),
              next = a + delta,
              ratio = (1 - next) / v;
            row[0] = next;
            for (let j = 1; j < n; j++) row[j] *= ratio;
          }
        }
      }
    }
  }
  const total = sum(cells.map((c) => c.electors)),
    country = new Float64Array(n);
  for (let i = 0; i < p.length; i++)
    for (let j = 0; j < n; j++)
      country[j] += (cells[i].electors / total) * p[i][j];
  return {
    probabilities: p,
    country,
    projectionResidual: residual,
    projectionIterations: iterations,
    model: input.model,
  };
}
