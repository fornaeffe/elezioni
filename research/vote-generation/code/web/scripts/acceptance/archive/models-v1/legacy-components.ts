/** Revised legacy law: fixed preparation, shared poll errors, component shocks. */
import type { Chamber } from "../../../../src/lib/politics/reform-2026/input";
import type { ReformAssemblyContext } from "../../../../src/lib/politics/reform-2026/generation-types";
import type { FastGenerationScenario, PhysicalCell } from "../../../../src/lib/politics/reform-2026/vote-share/forecast-types";
import type { CompiledLegacy } from "./legacy-types";
import { normalizedComposition, pollChoiceId } from "./correspondences";
import { politicalIntervention } from "./interventions";
import { forecastRandom } from "../../../../src/lib/politics/reform-2026/vote-share/forecast-random";
import { sum, day } from "./types";
const logit = (p: number) => Math.log(p / (1 - p));
const logistic = (x: number) => 1 / (1 + Math.exp(-x));
const move = (p: number, z: number) =>
  p === 0 || p === 1 ? p : logistic(logit(p) + z);

export function legacySurfaces(
  s: FastGenerationScenario,
  compiled: CompiledLegacy,
  cells: PhysicalCell[],
  ch: Chamber,
  simulationId: number,
  central: boolean,
) {
  const table = new Map(
      compiled.rows[ch].map((r) => [r.code, r.probabilities]),
    ),
    rows = cells.map((c) => Float64Array.from(table.get(c.municipality)!)),
    index = new Map(compiled.componentIds.map((id, i) => [id, i + 1])),
    selected = cells.flatMap((c, i) =>
      c.electors > 0 && compiled.pollCollegeIds[ch].includes(c.collegeId)
        ? [i]
        : [],
    ),
    local = selected.map((i) => rows[i]),
    weights = selected.map((i) => cells[i].electors);
  const polls = compiled.polls
    .filter(
      (p) =>
        p.enabled &&
        p.context === "camera" &&
        p.date <= s.referenceDate &&
        p.available <= s.referenceDate &&
        p.end <= s.referenceDate,
    )
    .sort(
      (a, b) =>
        b.available.localeCompare(a.available) ||
        b.end.localeCompare(a.end) ||
        a.id.localeCompare(b.id),
    );
  const seen = new Set<string>();
  const updates = polls
    .flatMap((p) => {
      const parts = p.groups.map((g) => {
        const composition = compiled.compositions[pollChoiceId(p.id, g.label)];
        if (composition?.proxy)
          throw new Error(
            "Legacy poll composition cannot proxy an unmeasured identity",
          );
        return normalizedComposition(g.members, composition).map((part) => ({
          ...part,
          id: index.has(part.id)
            ? part.id
            : (compiled.componentMap[part.id] ?? part.id),
        }));
      });
      const offered = new Set(parts.flatMap((g) => g.map((part) => part.id))),
        chosen = new Set([...offered].filter((id) => !seen.has(id)));
      if ([...offered].some((id) => !index.has(id)))
        throw new Error("UNRESOLVED_POLL_IDENTITY:" + p.id);
      offered.forEach((id) => seen.add(id));
      return chosen.size ? [{ p, parts, offered, chosen }] : [];
    })
    .reverse();
  // Apply older retained points first. Each latest component keeps its original
  // question denominator; a complete smaller choice set is not a full-field
  // national margin. Unreported partial-question support remains a residual.
  for (const { p, parts, offered, chosen } of updates) {
    const centralTotal = sum(p.groups.map((g) => g.share));
    if (
      p.complete ? Math.abs(centralTotal - 1) > 1e-8 : centralTotal > 1 + 1e-12
    )
      throw new Error("INCOMPATIBLE_POLL_QUESTION_TOTAL:" + p.id);
    const shares = p.groups.map((g) => {
      if (!(g.share >= 0 && g.share <= 1) || !(p.sample > 0))
        throw new Error("INVALID_POLL_SUPPORT");
      const sigma = Math.hypot(
        Math.sqrt((g.share * (1 - g.share)) / p.sample) /
          Math.max(0.01, g.share * (1 - g.share)),
        0.08,
      );
      return move(
        g.share,
        central
          ? 0
          : sigma *
              forecastRandom(
                s.seed,
                simulationId,
                "legacy-poll-group:" + p.id + ":" + g.label,
              ).normal(),
      );
    });
    // The unreported residual has no independent measurement-error draw and no
    // invented component composition. Keeping it in the denominator preserves
    // partialness and ensures a coherent realization without clipping or retry.
    const denominator =
        sum(shares) + (p.complete ? 0 : Math.max(0, 1 - centralTotal)),
      points = new Map<string, number>();
    parts.forEach((group, g) =>
      group.forEach((part) => {
        if (chosen.has(part.id))
          points.set(
            part.id,
            (points.get(part.id) ?? 0) +
              (part.fraction * shares[g]) / denominator,
          );
      }),
    );
    const active = new Set(
        p.complete
          ? [...offered].map((id) => index.get(id)!)
          : [...index.values()],
      ),
      conditional = local.map((row) => {
        const mass = sum([...active].map((j) => row[j]));
        const out = row.map((v, j) =>
          j === 0 ? 1 - mass : active.has(j) ? v : 0,
        );
        return out;
      }),
      entries = [...points],
      full = entries.length === active.size,
      total = sum(entries.map(([, v]) => v));
    if (!p.complete && full && centralTotal < 1 - 1e-12)
      throw new Error("PARTIAL_POLL_RESIDUAL_IDENTITY_REQUIRED:" + p.id);
    const targets = entries.map(([, v]) => (full ? v / total : v));
    politicalIntervention(
      conditional,
      weights,
      entries.map(([id]) => [index.get(id)!]),
      targets,
      (_, j) => (active.has(j) ? 1 : 0),
    );
    local.forEach((row, i) => {
      for (const j of active) row[j] = conditional[i][j];
    });
  }

  return rows;
}

export function legacyMovement(
  s: FastGenerationScenario,
  compiled: CompiledLegacy,
  context: ReformAssemblyContext,
  cells: PhysicalCell[],
  reference: Float64Array[],
  ids: string[],
  ch: Chamber,
  simulationId: number,
) {
  const profile = context.models.chambers.profiles.find(
    (p) => p.id === s.chamberProfileId,
  )!;
  const rho =
      profile.sharedVariance +
      (1 - profile.sharedVariance) * profile.residualCorrelation,
    scales = context.models.chambers.sigmaPerSqrtDay,
    elapsed = Math.sqrt(day(s.electionDate) - day(s.referenceDate)),
    geographicElapsed = elapsed,
    n = ids.length + 1,
    total = sum(cells.map((c) => c.electors)),
    country = new Float64Array(n);
  reference.forEach((r, i) =>
    r.forEach((p, j) => (country[j] += (p * cells[i].electors) / total)),
  );
  const future = country.map((p, j) =>
    move(
      p,
      elapsed *
        scales.national *
        forecastRandom(
          s.seed,
          simulationId,
          "legacy-national:" + (ids[j - 1] ?? "non-valid"),
          ch,
          rho,
        ).normal(),
    ),
  );
  const rows = reference.map((r, i) => {
    const c = cells[i];
    const p = r.map((v, j) => {
      if (v === 0 || v === 1) return v;
      const id = ids[j - 1] ?? "non-valid",
        geographic =
          geographicElapsed *
          (scales.region *
            forecastRandom(
              s.seed,
              simulationId,
              "legacy-region:" + c.region + ":" + id,
              ch,
              rho,
            ).normal() +
            scales.local *
              forecastRandom(
                s.seed,
                simulationId,
                "legacy-local:" + c.municipality + ":" + id,
                ch,
                rho,
              ).normal());
      return logistic(
        logit(future[j]) + logit(v) - logit(country[j]) + geographic,
      );
    });
    const mass = sum(p);
    if (!(mass > 0)) throw new Error("EMPTY_LEGACY_SUPPORT");
    return p.map((v) => v / mass);
  });
  return rows;
}
