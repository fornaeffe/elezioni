/** Maintained port of the exact latest-publication comparison procedure.
 * This is not the older 14-day-decay / one-wave-per-house polling estimator. */
import { preparePairedVoteReferences } from '../paired-vote-generation';
import type {
  ReformGenerationScenario,
  ReformGenerationSnapshot,
} from '../generation-types';
import type { SourceComposition } from './correspondences';
import { correspondenceRow, pollChoiceId } from './correspondences';
export interface LegacyPollObservation {
  id: string;
  date: string;
  available: string;
  end: string;
  context: 'camera' | 'european' | 'senato';
  groups: { label: string; members: string[]; share: number }[];
  sample: number;
  source: string;
  enabled: boolean;
  complete?: boolean;
}
export function latestLegacyPoints(
  polls: LegacyPollObservation[],
  cutoff: string,
  lists: { id: string; componentIds: string[] }[],
  compositions: Record<string, SourceComposition>,
  canonical: (id: string) => string,
) {
  const eligible = polls
    .filter(
      (p) =>
        p.enabled &&
        p.context === 'camera' &&
        p.available <= cutoff &&
        p.end <= cutoff &&
        p.date <= cutoff,
    )
    .sort(
      (a, b) =>
        b.available.localeCompare(a.available) ||
        b.end.localeCompare(a.end) ||
        a.id.localeCompare(b.id),
    );
  const points = new Map<
    string,
    {
      share: number;
      date: string;
      available: string;
      id: string;
      source: string;
      sample: number;
    }
  >();
  for (const p of eligible) {
    const local = new Map<string, number>(), members = new Set<string>(), labels = new Set<string>();
    for (const g of p.groups) {
      if (labels.has(g.label) || !g.members.length || g.members.some(id=>members.has(id)) || new Set(g.members).size!==g.members.length)
        throw new Error('Component competes in multiple poll choices: '+p.id);
      labels.add(g.label); for (const id of g.members) members.add(id);
      if (compositions[pollChoiceId(p.id,g.label)]?.proxy)
        throw new Error('Legacy poll composition cannot proxy an unmeasured identity: '+p.id);
      const targets = correspondenceRow(
          g.members,
          compositions[pollChoiceId(p.id, g.label)],
          lists,
          canonical,
        ),
        total = targets.reduce((n, t) => n + t.factor, 0);
      if (!total) continue; // Unnamed Other never becomes a measured new-party point.
      for (const t of targets)
        local.set(
          t.futureListId,
          (local.get(t.futureListId) ?? 0) + (g.share * t.factor) / total,
        );
    }
    for (const [id, share] of local)
      if (!points.has(id))
        points.set(id, {
          share,
          date: p.end,
          available: p.available,
          id: p.id,
          source: p.source,
          sample: p.sample,
        });
  }
  return { points, latest: eligible[0]?.available };
}
export function refreshLegacyPolls(
  s: ReformGenerationScenario,
  snapshot: ReformGenerationSnapshot,
  polls: LegacyPollObservation[],
  cutoff: string,
  lists: { id: string; componentIds: string[] }[],
  compositions: Record<string, SourceComposition>,
  canonical: (id: string) => string,
  chamberLists?: Record<'camera' | 'senato', { id: string; componentIds: string[] }[]>,
) {
  const active = s.renewal === 'joint' ? (['camera', 'senato'] as const) : [s.renewal],
    projections = active.map((ch) => {
      const future = chamberLists?.[ch] ?? lists,
        { points, latest } = latestLegacyPoints(polls, cutoff, future, compositions, canonical),
        fresh = future.filter((l) => !s.correspondences.some(
          (c) => c.chamber === ch && c.futureListId === l.id,
        ));
      return { ch, future, points, latest, fresh };
    }),
    audit = {
      policy: 'legacy-latest-publication-v1',
      points: projections.flatMap(({ ch, points }) => [...points].map(
        ([listId, p]) => ({ chamber: ch, listId, ...p }),
      )),
      unsupported: [] as string[],
      warnings: [] as string[],
    },
    freshRows = new Map<string, ReformGenerationScenario['shares'][number]>(),
    freshDates: string[] = [];
  for (const { ch, points, fresh } of projections) {
    for (const l of fresh) {
      const p = points.get(l.id);
      if (!p || p.share <= 0) {
        audit.unsupported.push(ch + '/' + l.id);
        continue;
      }
      const row = freshRows.get(l.id) ?? {
        scope: { kind: 'national' }, listId: l.id, common: null, camera: null, senato: null,
      };
      row[ch] = p.share;
      freshRows.set(l.id, row);
      freshDates.push(p.date);
    }
  }
  s.shares.push(...freshRows.values());
  s.projectionDate = freshDates.sort().at(-1) ?? snapshot.historicalDate;
  for (const { points, latest, fresh } of projections)
    for (const [id, p] of points)
      if (p.available !== latest && !fresh.some((l) => l.id === id)) points.delete(id);
  const baseline = preparePairedVoteReferences(s, snapshot).units,
    date = projections.flatMap(({ points }) => [...points.values()].map((p) => p.date))
      .sort().at(-1) ?? snapshot.historicalDate;
  const rows: NonNullable<ReformGenerationScenario['pollReferences']>['rows'] =
    [];
  for (const { ch, future: lists, points } of projections) {
    const ordinary = new Set(
        snapshot.seats.geography.colleges
          .filter((c) => c.chamber === ch && c.kind === 'ordinary')
          .map((c) => c.id),
      ),
      codes = new Set(
        snapshot.memberships
          .filter((m) => m.chamber === ch && ordinary.has(m.collegeId))
          .map((m) => m.municipalityCode),
      ),
      units = baseline.filter((u) => codes.has(u.municipality.code)),
      total = units.reduce(
        (n, u) => n + u.municipality[ch].electors * (1 - u[ch].abstention),
        0,
      ),
      base = new Map(
        lists.map((l) => [
          l.id,
          units.reduce(
            (n, u) =>
              n +
              u.municipality[ch].electors *
                (1 - u[ch].abstention) *
                (u[ch].shares.get(l.id) ?? 0),
            0,
          ) / (total || 1),
        ]),
      ),
      offered = new Set(
        s.coverage
          .filter((c) => c.chamber === ch && ordinary.has(c.collegeId))
          .flatMap((c) => c.listIds),
      ),
      ordinaryLists = lists.filter((l) => offered.has(l.id)),
      known = ordinaryLists.filter(
        (l) => points.has(l.id) && points.get(l.id)!.share > 0,
      ),
      mass = known.reduce((n, l) => n + points.get(l.id)!.share, 0),
      free = ordinaryLists.filter((l) => !known.includes(l)),
      remainder = free.reduce((n, l) => n + base.get(l.id)!, 0);
    if (!known.length) {
      audit.warnings.push('No named poll refresh for ' + ch);
      continue;
    }
    const scale = mass >= 1 ? (1 - remainder) / mass : 1,
      remaining = 1 - mass * scale;
    if (scale !== 1)
      audit.warnings.push(
        'Overcomplete mixed-wave references normalized; historical unreported mass retained: ' +
          ch +
          '/' +
          mass,
      );
    for (const l of ordinaryLists) {
      const p = points.get(l.id),
        share = p
          ? p.share * scale
          : remainder
            ? (base.get(l.id)! * remaining) / remainder
            : 0;
      rows.push({
        chamber: ch,
        listId: l.id,
        share,
        sigma: p
          ? Math.hypot(
              Math.sqrt((p.share * (1 - p.share)) / p.sample) /
                Math.max(0.01, p.share * (1 - p.share)),
              0.08,
            )
          : 0,
        donorHistoricalListIds: [],
      });
    }
    const r = rows.filter((r) => r.chamber === ch),
      sum = r.reduce((n, r) => n + r.share, 0);
    for (const row of r) row.share /= sum;
  }
  if (rows.length) {
    s.pollReferences = {
      version: 'legacy-latest-publication-v1',
      archiveVersion: 'bound-by-unified-input-key',
      policyVersion: 'poll-reference-v1',
      date,
      sourceQuestionIds: [...new Set(projections.flatMap(({ points }) => [...points.values()].map((p) => p.id)))],
      assumptions: [
        'Latest reviewed named observations; mixed waves and residual baseline allocation are user assumptions, not fitted estimates.',
      ],
      rows,
    };
    s.schemaVersion = 3;
    s.shares = [];
    s.projectionDate = date;
  }
  return audit;
}
