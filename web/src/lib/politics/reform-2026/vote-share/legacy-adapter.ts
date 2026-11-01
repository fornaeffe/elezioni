/** Component-based legacy preparation. No future ticket projection or vote relocation. */
import type { ReleasedScenario } from "./release";
import type { CompactSource } from "./source-types";
import type { FastVoteAssets } from "./forecast-types";
import type { ReformAssemblyContext } from "../generation-types";
import { decodeSourceRows, sourceKey, type AssetBytes } from "./source-codec";
import { legacyPollGroups } from "./legacy-observations";
import {
  normalizedComposition,
  type SourceComposition,
} from "./correspondences";
import type { LegacyPollObservation } from "./legacy-polls";
import {
  COMPONENT_POLICY,
  poolConditional,
  completeLocal,
  type ComponentObservation,
} from "./component-policy";
export interface CompiledLegacy {
  inputKey: string;
  policy: typeof COMPONENT_POLICY;
  componentIds: string[];
  componentMap: Record<string, string>;
  pollCollegeIds: Record<"camera" | "senato", string[]>;
  historicalDate: string;
  rows: Record<
    "camera" | "senato",
    { code: string; probabilities: number[]; original?: ComponentObservation }[]
  >;
  polls: LegacyPollObservation[];
  compositions: Record<string, SourceComposition>;
  requiredReferences: string[];
  audit: {
    baseline: { camera: string; senato: string };
    missingHistory: string[];
    policy: string;
    fallbackCounts: Record<string, number>;
  };
}
export const isLegacyGeneration = (s: Pick<ReleasedScenario, "generation">) =>
  s.generation?.model === "legacy-poll-informed-v1";
export function legacyInputKey(s: ReleasedScenario) {
  return sourceKey("unified-legacy-input-v2", {
    policy: COMPONENT_POLICY,
    scenario: {
      ...s,
      references: s.references.map(({ sdPercentagePoints, ...r }) => r),
      nonValidReferences: s.nonValidReferences.map(
        ({ sdPercentagePoints, ...r }) => r,
      ),
      fitSeed: "",
    },
  });
}
export function latestLegacyPair(source: CompactSource, cutoff: string) {
  const eligible = source.payload.events.filter(
    (e) => e.date <= cutoff && e.availableDate <= cutoff && e.minimumAge === 18,
  );
  const pairs = eligible
    .filter((e) => e.context === "camera")
    .flatMap((camera) =>
      eligible
        .filter((e) => e.context === "senato" && e.date === camera.date)
        .map((senato) => ({ camera, senato })),
    )
    .sort(
      (a, b) =>
        b.camera.date.localeCompare(a.camera.date) ||
        a.camera.id.localeCompare(b.camera.id),
    );
  if (!pairs.length)
    throw new Error(
      "No eligible common-18+ Camera/Senato pair at this reference date",
    );
  return pairs[0];
}
export async function compileLegacy(
  s: ReleasedScenario,
  source: CompactSource,
  read: (path: string) => Promise<AssetBytes>,
  assets: FastVoteAssets,
  context: ReformAssemblyContext,
): Promise<CompiledLegacy> {
  if (!s.generation) throw new Error("Unified generation settings required");
  const pair = latestLegacyPair(source, s.referenceDate),
    ids = s.components.map((c) => c.id).sort(),
    position = new Map(ids.map((id, i) => [id, i + 1])),
    width = ids.length + 1,
    choices = new Map(source.payload.choices.map((c) => [c.id, c])),
    units = new Map(source.payload.units.map((u) => [u.id, u])),
    required = new Set(
      s.components.filter((c) => c.kind === "new").map((c) => c.id),
    ),
    canonical = (id: string) =>
      position.has(id) ? id : (s.componentMap[id] ?? id);
  const result: CompiledLegacy = {
    inputKey: await legacyInputKey(s),
    policy: COMPONENT_POLICY,
    componentIds: ids,
    componentMap: s.componentMap,
    pollCollegeIds: {
      camera: context.seats.geography.colleges
        .filter((c) => c.chamber === "camera" && c.kind === "ordinary")
        .map((c) => c.id),
      senato: context.seats.geography.colleges
        .filter((c) => c.chamber === "senato" && c.kind === "ordinary")
        .map((c) => c.id),
    },
    historicalDate: pair.camera.date,
    rows: { camera: [], senato: [] },
    polls: [],
    compositions: s.generation.compositions,
    requiredReferences: [],
    audit: {
      baseline: { camera: pair.camera.id, senato: pair.senato.id },
      missingHistory: [],
      policy: COMPONENT_POLICY,
      fallbackCounts: {},
    },
  };
  for (const ch of ["camera", "senato"] as const) {
    const history: ComponentObservation[] = [],
      exact = new Map<string, ComponentObservation>();
    for (const row of await decodeSourceRows(
      source,
      pair[ch].id,
      await read(pair[ch].chunk.path),
    )) {
      const unit = units.get(row.unit)!;
      if (unit.match === "unlocated" || row.electors <= 0) continue;
      const counts = Array(width).fill(0) as number[],
        offered = Array(width).fill(false) as boolean[];
      offered[0] = true;
      counts[0] = row.electors - row.choices.reduce((a, c) => a + c.count, 0);
      const seen = new Set<string>();
      for (const c of row.choices) {
        const choice = choices.get(c.id)!,
          members = s.sourceMemberships[c.id]?.members ?? choice.members,
          composition = s.generation!.compositions[c.id];
        if (members.length > 1 && !composition)
          throw new Error("LEGACY_COMPOSITION_REQUIRED:" + c.id);
        if (composition?.proxy && !choice.opaque)
          throw new Error("Legacy proxy is reserved for opaque groups");
        for (const part of normalizedComposition(members, composition)) {
          const id = canonical(part.id),
            j = position.get(id);
          if (seen.has(part.id))
            throw new Error(
              "Component competes in multiple original source choices:" + id,
            );
          seen.add(part.id);
          // Components outside the future fitted basis remain explicit removed alternatives.
          if (j === undefined)
            throw new Error("UNRESOLVED_HISTORICAL_IDENTITY:" + id);
          offered[j] = true;
          counts[j] += c.count * part.fraction;
        }
      }
      const obs = {
        code: unit.targetCode ?? unit.id,
        region: unit.region,
        electors: row.electors,
        counts,
        offered,
      };
      history.push(obs);
      if (unit.match === "same-place" && unit.targetCode) {
        if (exact.has(unit.targetCode))
          throw new Error("Duplicate legacy geographic baseline");
        exact.set(unit.targetCode, obs);
      }
    }
    const national = poolConditional(history, width),
      regions = new Map<string, Float64Array>();
    ids.forEach((id, j) => {
      if (
        !national.observed[j + 1] ||
        national.probabilities[j + 1] === 0 ||
        national.unidentified[j + 1]
      )
        required.add(id);
    });
    for (const region of new Set(
      assets.geography.targets.map((t) => t.region),
    )) {
      const donors = history.filter((r) => r.region === region);
      regions.set(
        region,
        poolConditional(donors, width, national.probabilities).probabilities,
      );
    }
    for (const t of assets.geography.targets) {
      const row = exact.get(t.code),
        donor = regions.get(t.region)!;
      const q = completeLocal(row, donor),
        mass = q.reduce((a, v) => a + v, 0);
      if (!(mass > 0))
        throw new Error("EMPTY_HISTORICAL_SUPPORT:" + ch + "/" + t.code);
      const missing = row ? row.offered.filter((v) => !v).length : width;
      result.audit.fallbackCounts[ch] =
        (result.audit.fallbackCounts[ch] ?? 0) + missing;
      if (!row) result.audit.missingHistory.push(ch + "/" + t.code);
      result.rows[ch].push({
        code: t.code,
        probabilities: [...q.map((v) => v / mass)],
        original: row,
      });
    }
  }
  const added = new Map(s.polls.added.map((p) => [p.id, p]));
  for (const id of s.polls.selected) {
    const p = added.get(id) ?? source.payload.polls.find((p) => p.id === id);
    if (!p) throw new Error("Unknown selected legacy poll");
    result.polls.push({
      id: p.id,
      date: p.date,
      end: p.fieldworkEnd,
      available: p.availableDate,
      context: p.context,
      sample: p.effectiveSample,
      source: p.citation,
      enabled: p.status === "supported",
      complete: true,
      groups: legacyPollGroups(source, p),
    });
  }
  result.polls.push(...s.generation.supplementalPolls);
  for (const p of result.polls) {
    if (
      !p.enabled ||
      p.date > s.referenceDate ||
      p.available > s.referenceDate ||
      p.end > s.referenceDate
    )
      continue;
    const seen = new Set<string>();
    for (const g of p.groups) {
      if (g.members.some((id) => seen.has(id)))
        throw new Error("Component competes in multiple poll choices:" + p.id);
      g.members.forEach((id) => seen.add(id));
      if (
        g.members.length > 1 &&
        !result.compositions["poll:" + p.id + ":" + g.label]
      )
        throw new Error(
          "LEGACY_COMPOSITION_REQUIRED:poll:" + p.id + ":" + g.label,
        );
    }
  }
  result.requiredReferences = [...required].sort();
  return result;
}
