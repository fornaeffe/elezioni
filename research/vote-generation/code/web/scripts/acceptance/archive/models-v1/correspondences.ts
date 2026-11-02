/** Structural S_i is separate from legacy row composition W_i. */
export {generationModels} from '../../../../src/lib/politics/reform-2026/vote-share/model-registry';
import type {ModelId} from '../../../../src/lib/politics/reform-2026/vote-share/model-registry';
export interface SourceComposition {
  fractions: Record<string, number>;
  provenance: string;
  /** An explicit legacy proxy for an opaque source; never an identified fitted split. */
  proxy?: boolean;
}
export interface GenerationChoice {
  model: ModelId;
  modelVersion: string;
  compositions: Record<string, SourceComposition>;
  /** Compared partial named observations; unsupported as complete candidate polls. */
  supplementalPolls: import("./legacy-polls").LegacyPollObservation[];
}
export const pollChoiceId = (poll: string, label: string) =>
  "poll:" + poll + ":" + label;
export function normalizedComposition(
  members: readonly string[],
  composition?: SourceComposition,
) {
  const ids = composition?.proxy
    ? Object.keys(composition.fractions)
    : [...members];
  if (!ids.length || new Set(ids).size !== ids.length)
    throw new Error("Empty or repeated source membership");
  const weights = ids.map((id) =>
    composition ? (composition.fractions[id] ?? 0) : 1,
  );
  if (
    composition &&
    (!composition.provenance.trim() ||
      (!composition.proxy &&
        ids.some((id) => !Object.hasOwn(composition.fractions, id))) ||
      Object.keys(composition.fractions).some((id) => !ids.includes(id)))
  )
    throw new Error(
      "Composition must name exactly its source members and retain provenance",
    );
  if (
    weights.some((w) => !Number.isFinite(w) || w < 0) ||
    !weights.some((w) => w > 0)
  )
    throw new Error(
      "Composition needs finite nonnegative weights with positive total",
    );
  const ordinaryTotal = weights.reduce((n, w) => n + w, 0);
  if (Number.isFinite(ordinaryTotal))
    return ids.map((id, i) => ({ id, fraction: weights[i] / ordinaryTotal }));
  const maximum = Math.max(...weights),
    scaled = weights.map((w) => w / maximum),
    total = scaled.reduce((n, w) => n + w, 0);
  return ids.map((id, i) => ({ id, fraction: scaled[i] / total }));
}
/** C_i = W_i S_future^T. Future memberships are binary, never row-normalized. */
export function correspondenceRow(
  members: readonly string[],
  composition: SourceComposition | undefined,
  lists: readonly { id: string; componentIds: string[] }[],
  canonical: (id: string) => string = (id) => id,
) {
  const row = normalizedComposition(members, composition),
    result = new Map<string, number>();
  for (const { id, fraction } of row)
    for (const list of lists)
      if (list.componentIds.includes(canonical(id)))
        result.set(list.id, (result.get(list.id) ?? 0) + fraction);
  return [...result]
    .filter(([, factor]) => factor > 0)
    .map(([futureListId, factor]) => ({ futureListId, factor }));
}
export function equalComposition(
  members: readonly string[],
): SourceComposition {
  if (!members.length) throw new Error("Select source members first");
  return {
    fractions: Object.fromEntries(
      members.map((id) => [id, 1 / members.length]),
    ),
    provenance:
      "Explicit equal allocation action; assumed internal composition, not observed counts.",
  };
}
