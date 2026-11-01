/** Authored comparison assumptions, converted to editable scenario values. */
import { legacyPollGroups } from "../../src/lib/politics/reform-2026/vote-share/legacy-observations";
import type { ReleasedScenario } from "../../src/lib/politics/reform-2026/vote-share/release";
import type { CompactSource } from "../../src/lib/politics/reform-2026/vote-share/source-types";
import {
  equalComposition,
  type SourceComposition,
} from "../../src/lib/politics/reform-2026/vote-share/correspondences";
export function unifiedShareDefault(
  s: ReleasedScenario,
  source: CompactSource,
  policy: { jointWeights: Record<string, Record<string, number>> },
  review: {
    polls: {
      id: string;
      date: string;
      available: string;
      groups: string[][];
      shares: number[];
      sample?: number;
      source: string;
    }[];
    historicalTicketSplits: Record<string, Record<string, number>>;
  },
) {
  s.schemaVersion = 5;
  const compositions: Record<string, SourceComposition> = {};
  for (const c of source.payload.choices) {
    const members = s.sourceMemberships[c.id]?.members ?? c.members,
      weights =
        policy.jointWeights[c.eventId + ":" + c.label] ??
        policy.jointWeights[
          c.eventId.replace("senato", "camera") + ":" + c.label
        ];
    if (members.length > 1)
      compositions[c.id] = weights
        ? {
            fractions: Object.fromEntries(
              members.map((id) => [id, weights[id] ?? 0]),
            ),
            provenance:
              "Reviewed deterministic joint composition from legacy-comparison-policy.json; not observed internal counts.",
          }
        : equalComposition(members);
    const split =
      c.eventId === "senato-2022"
        ? review.historicalTicketSplits[c.label]
        : undefined;
    if (c.opaque && split) {
      const fractions: Record<string, number> = {};
      for (const [listId, share] of Object.entries(split)) {
        const list = s.lists.senato.find((l) => l.id === listId);
        if (!list)
          throw new Error("Missing comparison proxy destination " + listId);
        for (const id of list.componentIds)
          fractions[id] =
            (fractions[id] ?? 0) + share / list.componentIds.length;
      }
      compositions[c.id] = {
        fractions,
        proxy: true,
        provenance:
          "Reviewed named TAA ticket proxy from legacy-comparison-future-polls.json; candidate retains the opaque original observation.",
      };
    } else if (c.opaque && c.eventId === "senato-2022" && c.label === "TEAM K")
      compositions[c.id] = {
        fractions: { "team-k": 1 },
        proxy: true,
        provenance:
          "Exact TEAM K identity reviewed for legacy comparison; candidate retains its declared fallback.",
      };
  }
  s.generation = {
    model: "selected",
    compositions,
    supplementalPolls: review.polls.map((p) => ({
      id: p.id,
      date: p.date,
      end: p.date,
      available: p.available,
      context: "camera",
      source: p.source,
      sample: p.sample ?? 300,
      enabled: true,
      groups: p.groups.map((members, i) => ({
        label: members.join(" + "),
        members,
        share: p.shares[i],
      })),
    })),
  };
  // Make the default's assumed internal poll splits explicit and editable,
  // rather than letting the runtime invent equal weights for a missing row.
  const questions = [
    ...source.payload.polls.map((p) => ({
      id: p.id,
      groups: legacyPollGroups(source, p),
    })),
    ...s.generation.supplementalPolls,
  ];
  for (const p of questions)
    for (const g of p.groups)
      if (g.members.length > 1) {
        const matching = source.payload.choices
          .filter(
            (c) =>
              JSON.stringify([...c.members].sort()) ===
              JSON.stringify([...g.members].sort()),
          )
          .sort((a, b) => b.eventId.localeCompare(a.eventId))
          .find((c) => compositions[c.id] && !compositions[c.id].proxy);
        compositions["poll:" + p.id + ":" + g.label] = matching
          ? structuredClone(compositions[matching.id])
          : equalComposition(g.members);
        compositions["poll:" + p.id + ":" + g.label].provenance +=
          " Explicit default poll-group composition; shared group measurement error, not independent component observations.";
      }
  // The compared explicit FN TAA point is shared as an authored intervention,
  // not a nationwide poll silently applied to a special ballot.
  for (const ch of ["camera", "senato"] as const)
    if (
      !s.references.some(
        (r) =>
          r.scope.kind === "region" &&
          r.scope.code === "04" &&
          r.channel === ch &&
          r.subject.id ===
            s.lists[ch].find((l) => l.id === "futuro-nazionale")!
              .componentIds[0],
      )
    )
      s.references.push({
        id: "compared-taa-fn:" + ch,
        scope: { kind: "region", code: "04" },
        channel: ch,
        subject: {
          kind: "component",
          id: s.lists[ch].find((l) => l.id === "futuro-nazionale")!
            .componentIds[0],
        },
        mean: 0.072,
        sdPercentagePoints: 0,
      });
  return s;
}
