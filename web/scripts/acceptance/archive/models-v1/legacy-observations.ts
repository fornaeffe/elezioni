import type { CompactSource, SourcePoll } from "../../../../src/lib/politics/reform-2026/vote-share/source-types";
/** Original dated Other complement, before component pooling or future filing. */
export function legacyPollGroups(source: CompactSource, p: SourcePoll) {
  const named = new Set(
      p.normalized.filter((a) => !a.other).flatMap((a) => a.members),
    ),
    starts = new Map<string, string>(),
    dates = new Map(source.payload.events.map((e) => [e.id, e.date]));
  for (const choice of source.payload.choices)
    for (const id of choice.members) {
      const date = dates.get(choice.eventId)!;
      if (!starts.has(id) || date < starts.get(id)!) starts.set(id, date);
    }
  for (const q of source.payload.polls)
    for (const a of q.normalized.filter((a) => !a.other))
      for (const id of a.members)
        if (!starts.has(id) || q.date < starts.get(id)!) starts.set(id, q.date);
  return p.normalized.map((a) => ({
    label: a.label,
    share: a.share,
    members: a.other
      ? [...starts]
          .filter(([id, date]) => date <= p.date && !named.has(id))
          .map(([id]) => id)
          .sort()
      : a.members,
  }));
}
