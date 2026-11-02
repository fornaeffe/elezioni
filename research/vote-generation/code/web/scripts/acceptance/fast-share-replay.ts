/** Original-domain replay adapter; held-out votes are used only by scoring. */
import { fitReference } from "../../src/lib/politics/reform-2026/vote-share/information";
import { fitProfiles } from "../../src/lib/politics/reform-2026/vote-share/profiles";
import {
  betaTransition,
  movementScales,
} from "../../src/lib/politics/reform-2026/vote-share/transition";
import { forecastRandom } from "../../src/lib/politics/reform-2026/vote-share/forecast-random";
import { referenceNoise } from "../../src/lib/politics/reform-2026/vote-share/reference-noise";
import {
  projectMargins,
  ProjectionCapacityError,
} from "../../src/lib/politics/reform-2026/vote-share/projection";
import { removeAbsent } from "../../src/lib/politics/reform-2026/vote-share/interventions";
import { multinomial } from "../../src/lib/politics/reform-2026/generation-math";
import {
  feasibleField,
  type FieldModel,
} from "../../src/lib/politics/reform-2026/vote-share/feasible-field";
import {
  sum,
  years,
  type NationalElection,
  type InformationPoll,
  type FitContext,
} from "../../src/lib/politics/reform-2026/vote-share/types";
interface Truth {
  id: string;
  region: string;
  electors: number;
  nonValid: number;
  groups: string[][];
  counts: number[];
}
interface History extends Omit<NationalElection, "context"> {
  kind: FitContext;
}
export interface ReplaySource {
  components: string[];
  cities: string[];
  cutoff: string;
  targetDate: string;
  targetKind: FitContext;
  nationalHistory: History[];
  elections: { id: string; date: string; kind: FitContext; rows: Truth[] }[];
  truth: Truth[];
  nationalTruth: History;
  polls: (Omit<
    InformationPoll,
    "context" | "availableDate" | "fieldworkEnd" | "errorGroup"
  > & { kind: FitContext; available: string; end: string; group: string })[];
}
export function quantile(sorted: number[], p: number) {
  const at = (sorted.length - 1) * p,
    lo = Math.floor(at);
  return sorted[lo] + (sorted[Math.ceil(at)] - sorted[lo]) * (at - lo);
}
export function score(values: number[], truth: number) {
  if (
    !values.length ||
    !Number.isFinite(truth) ||
    values.some((x) => !Number.isFinite(x))
  )
    throw new Error("Invalid predictive score");
  const ordered = [...values].sort((a, b) => a - b),
    median = quantile(ordered, 0.5),
    mean = sum(values) / values.length;
  let wis = 0.5 * Math.abs(median - truth);
  for (const level of [0.5, 0.8, 0.9, 0.95]) {
    const alpha = 1 - level,
      lo = quantile(ordered, alpha / 2),
      hi = quantile(ordered, 1 - alpha / 2);
    wis +=
      (alpha / 2) *
      (hi -
        lo +
        (2 / alpha) * (Math.max(0, lo - truth) + Math.max(0, truth - hi)));
  }
  const lo = quantile(ordered, 0.05),
    hi = quantile(ordered, 0.95);
  return {
    mean,
    coverage90: Number(lo <= truth && truth <= hi),
    width90: hi - lo,
    meanMae: Math.abs(mean - truth),
    meanBias: mean - truth,
    wis: wis / 4.5,
  };
}
export function aggregate(rows: ReturnType<typeof score>[]) {
  return Object.fromEntries(
    ["coverage90", "width90", "meanMae", "meanBias", "wis"].map((key) => [
      key,
      sum(rows.map((r) => r[key as keyof typeof r])) / rows.length,
    ]),
  );
}
export function prepareReplay(source: ReplaySource, seed: string) {
  const national = {
    components: source.components,
    referenceDate: source.cutoff,
    elections: source.nationalHistory.map(({ kind, ...e }) => ({
      ...e,
      context: kind,
    })),
    polls: source.polls.map(({ kind, available, end, group, ...p }) => ({
      ...p,
      date: p.date.slice(0, 10),
      context: kind,
      availableDate: available.slice(0, 10),
      fieldworkEnd: end.slice(0, 10),
      errorGroup: group,
    })),
  };
  const fit = fitReference(national, seed + ":fit"),
    profiles = fitProfiles({
      components: source.components,
      referenceDate: source.cutoff,
      targetContext: source.targetKind,
      nationalElections: national.elections,
      elections: source.elections.map(({ kind, ...e }) => ({
        ...e,
        context: kind,
      })),
      targets: source.truth.map((t) => ({
        id: t.id,
        region: t.region,
        electors: t.electors,
      })),
    });
  return { fit, profiles };
}
export interface ReplayOptions {
  model?: FieldModel;
  scaleMultiplier?: number;
  prepared?: ReturnType<typeof prepareReplay>;
  aggregateDomain?: boolean;
  retainCounts?: boolean;
  referenceConstruction?: "conditional" | "country-calibrated";
  profileRegionCorrelation?: number;
  onProgress?: (draw: number, total: number) => void;
}
export function replay(
  source: ReplaySource,
  seed: string,
  availability: "reference" | "future" = "reference",
  draws = 512,
  options: ReplayOptions = {},
) {
  const { fit, profiles } = options.prepared ?? prepareReplay(source, seed);
  const ids = fit.componentIds,
    k = ids.length,
    n = k + 1,
    position = new Map(ids.map((id, j) => [id, j + 1])),
    truth = new Map(source.truth.map((t) => [t.id, t])),
    context = Number(source.targetKind === "european"),
    horizon = years(source.targetDate, source.cutoff);
  const offered = profiles.municipalityIds.map((id) => {
    const mask = new Array(n).fill(false);
    mask[0] = true;
    for (const group of truth.get(id)!.groups)
      for (const c of group) mask[position.get(c)!] = true;
    return mask;
  });
  const countryOffered = Array.from({ length: n }, (_, j) =>
      offered.some((mask) => mask[j]),
    ),
    nationalDraws: Float64Array[] = [],
    municipal: Uint32Array[] = [];
  let maximumResidual = 0,
    maximumIterations = 0,
    referenceConditionedDraws = 0;
  for (let s = 1; s <= draws; s++) {
    if (s % 64 === 0) options.onProgress?.(s, draws);
    const row = forecastRandom(seed, s, "posterior-political-row").integer(
        fit.political.chains * fit.political.drawsPerChain,
      ),
      row0 = forecastRandom(seed, s, "posterior-non-valid-row").integer(
        fit.participation.chains * fit.participation.drawsPerChain,
      );
    let q: Float64Array = fit.political.values.slice(
        row * fit.political.dimensions + context * k,
        row * fit.political.dimensions + (context + 1) * k,
      ),
      a = fit.participation.values[row0 * 4 + context * 2];
    if (availability === "reference") {
      const b = removeAbsent(
        Float64Array.from([a, ...q.map((x) => (1 - a) * x)]),
        countryOffered,
      );
      a = b[0];
      q = b.slice(1).map((x) => x / (1 - a));
    }
    const scale = movementScales(
      fit.audit.scales.political,
      fit.audit.scales.participation,
      forecastRandom(seed, s, "movement-scales"),
    );
    if (options.model) {
      const noiseField = referenceNoise(
        seed,
        s,
        n,
        0,
        options.profileRegionCorrelation ?? 0,
      );
      let reference = profiles.municipalityIds.map((id, c) => {
        const noise = noiseField("camera", id, profiles.regions[c]),
          scores = Float64Array.from({ length: n }, (_, j) => {
            const z = noise[j],
              b = j === 0 ? a : (1 - a) * q[j - 1];
            return offered[c][j] && b > 0
              ? Math.log(b) +
                  profiles.means[c * n + j] +
                  profiles.profileSd[c * n + j] * z
              : -Infinity;
          }),
          max = Math.max(...scores),
          p = scores.map((x) => Math.exp(x - max)),
          total = sum(p);
        return p.map((x) => x / total);
      });
      if (options.referenceConstruction === "country-calibrated") {
        const b = Float64Array.from([a, ...q.map((x) => (1 - a) * x)]);
        try {
          const projection = projectMargins(b, reference.length, (i) => ({
            weight: profiles.electors[i],
            scores: reference[i].map((x) => (x > 0 ? Math.log(x) : -Infinity)),
          }));
          reference = reference.map((_, i) => projection.probabilities(i));
        } catch (error) {
          if (!(error instanceof ProjectionCapacityError)) throw error;
          referenceConditionedDraws++;
        }
      }
      const generated = feasibleField({
        model: options.model,
        reference,
        cells: profiles.municipalityIds.map((id, i) => ({
          id,
          region: profiles.regions[i],
          electors: profiles.electors[i],
        })),
        politicalScales: new Float64Array(k).fill(scale.political),
        localScales: profiles.futureScales,
        participationScale: scale.nonValid,
        discrepancy: 1.5 * fit.audit.electionDiscrepancy,
        horizon,
        seed,
        simulationId: s,
        scaleMultiplier: options.scaleMultiplier,
      });
      nationalDraws.push(generated.country);
      maximumResidual = Math.max(maximumResidual, generated.projectionResidual);
      maximumIterations = Math.max(
        maximumIterations,
        generated.projectionIterations,
      );
      const counts = new Uint32Array(profiles.municipalityIds.length * n);
      for (let c = 0; c < profiles.municipalityIds.length; c++)
        counts.set(
          multinomial(
            profiles.electors[c],
            [...generated.probabilities[c]],
            forecastRandom(
              seed,
              s,
              "multinomial:" + profiles.municipalityIds[c],
            ).uniform,
          ),
          c * n,
        );
      municipal.push(counts);
      continue;
    }
    q = betaTransition(
      q,
      new Float64Array(k).fill(scale.political),
      horizon,
      forecastRandom(seed, s, "political-movement"),
    );
    q = betaTransition(
      q,
      new Float64Array(k).fill(1.5 * fit.audit.electionDiscrepancy),
      1,
      forecastRandom(seed, s, "election-discrepancy"),
    );
    const nv = betaTransition(
      [a, 1 - a],
      [scale.nonValid, scale.nonValid],
      horizon,
      forecastRandom(seed, s, "non-valid-movement"),
    );
    let target = Float64Array.from([nv[0], ...q.map((x) => nv[1] * x)]);
    if (availability === "future")
      target = removeAbsent(target, countryOffered);
    nationalDraws.push(target);
    const localScale = Math.exp(
        0.2 * forecastRandom(seed, s, "local-scale").normal(),
      ),
      regionNoise = new Map(
        [...new Set(profiles.regions)].map((region) => {
          const rng = forecastRandom(seed, s, "region:" + region);
          return [
            region,
            Float64Array.from({ length: n }, () => rng.normal()),
          ] as const;
        }),
      );
    const scores = profiles.municipalityIds.map((id, c) => {
      const p = forecastRandom(seed, s, "profile:" + id),
        l = forecastRandom(seed, s, "local:" + id);
      return Float64Array.from({ length: n }, (_, j) => {
        const pn = p.normal(),
          ln = l.normal();
        return !offered[c][j] || !target[j]
          ? -Infinity
          : Math.log(target[j]) +
              profiles.means[c * n + j] +
              profiles.profileSd[c * n + j] * pn +
              profiles.futureScales[j] *
                localScale *
                Math.sqrt(horizon) *
                (Math.sqrt(0.85) * ln +
                  Math.sqrt(0.15) * regionNoise.get(profiles.regions[c])![j]);
      });
    });
    const projected = projectMargins(
      target,
      profiles.municipalityIds.length,
      (i) => ({ weight: profiles.electors[i], scores: scores[i] }),
    );
    maximumResidual = Math.max(maximumResidual, projected.residual);
    maximumIterations = Math.max(maximumIterations, projected.iterations);
    const counts = new Uint32Array(profiles.municipalityIds.length * n);
    for (let c = 0; c < profiles.municipalityIds.length; c++) {
      const rng = forecastRandom(
        seed,
        s,
        "multinomial:" + profiles.municipalityIds[c],
      );
      counts.set(
        multinomial(
          profiles.electors[c],
          [...projected.probabilities(c)],
          rng.uniform,
        ),
        c * n,
      );
    }
    municipal.push(counts);
  }
  const nat: ReturnType<typeof score>[] = [],
    mun: ReturnType<typeof score>[] = [],
    natElector: ReturnType<typeof score>[] = [],
    munElector: ReturnType<typeof score>[] = [];
  const metrics: {
    scope: string;
    members: string[];
    truth: number;
    values: number[];
    municipality?: string;
  }[] = [];
  let nationalTruth = source.nationalTruth;
  if (options.aggregateDomain) {
    const counts = [
        sum(source.truth.map((t) => t.nonValid)),
        ...source.nationalTruth.groups.map((members) =>
          sum(
            source.truth.flatMap((t) =>
              t.groups.map((group, g) =>
                group.every((id) => members.includes(id)) ? t.counts[g] : 0,
              ),
            ),
          ),
        ),
      ],
      electors = sum(source.truth.map((t) => t.electors));
    if (sum(counts) !== electors)
      throw new Error("Sample-domain truth coarsening lost counts");
    nationalTruth = { ...source.nationalTruth, electors, counts };
    nationalDraws.splice(
      0,
      nationalDraws.length,
      ...municipal.map((draw) => {
        const b = new Float64Array(n);
        for (let c = 0; c < profiles.municipalityIds.length; c++)
          for (let j = 0; j < n; j++) b[j] += draw[c * n + j] / electors;
        return b;
      }),
    );
  }
  for (let g = 0; g < nationalTruth.groups.length; g++) {
    const members = nationalTruth.groups[g],
      indices = members.map((id) => position.get(id)!);
    const values = nationalDraws.map(
      (b) => sum(indices.map((j) => b[j])) / (1 - b[0]),
    );
    const actual =
      nationalTruth.counts[g + 1] / sum(nationalTruth.counts.slice(1));
    nat.push(score(values, actual));
    natElector.push(
      score(
        nationalDraws.map((b) => sum(indices.map((j) => b[j]))),
        nationalTruth.counts[g + 1] / nationalTruth.electors,
      ),
    );
    metrics.push({ scope: "national", members, truth: actual, values });
  }
  natElector.push(
    score(
      nationalDraws.map((b) => b[0]),
      nationalTruth.counts[0] / nationalTruth.electors,
    ),
  );
  profiles.municipalityIds.forEach((id, c) => {
    const t = truth.get(id)!;
    munElector.push(
      score(
        municipal.map((d) => d[c * n] / t.electors),
        t.nonValid / t.electors,
      ),
    );
    t.groups.forEach((members, g) => {
      const js = members.map((id) => position.get(id)!),
        values = municipal.map((d) => {
          const valid = t.electors - d[c * n];
          return valid ? sum(js.map((j) => d[c * n + j])) / valid : 0;
        }),
        actual = t.counts[g] / (t.electors - t.nonValid);
      mun.push(score(values, actual));
      munElector.push(
        score(
          municipal.map((d) => sum(js.map((j) => d[c * n + j])) / t.electors),
          t.counts[g] / t.electors,
        ),
      );
      metrics.push({
        scope: "municipal",
        municipality: id,
        members,
        truth: actual,
        values,
      });
    });
  });
  return {
    scores: {
      national: aggregate(nat),
      municipal: aggregate(mun),
      nationalElector: aggregate(natElector),
      municipalElector: aggregate(munElector),
    },
    metrics,
    maximumResidual,
    maximumIterations,
    referenceConditionedDraws,
    fitDiagnostics: {
      political: fit.political.diagnostics,
      participation: fit.participation.diagnostics,
    },
    scope:
      "Original-domain conditional geography and observable groups; no within-group truth and no legal carrier",
    availability,
    ...(options.retainCounts
      ? {
          municipalDraws: municipal,
          municipalityIds: profiles.municipalityIds,
          componentIds: ids,
        }
      : {}),
    forecastModel: options.model ?? "legacy-global-beta-v1",
  };
}
