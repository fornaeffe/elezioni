import type { M01Settings, M01Data, CollegeCounts } from './types';
import type { GeneratedMunicipalVotes } from './m01';
import { apportion, generationRng, sum } from '../reform-2026/generation-math';
/** Controlled integer reconstruction of historical municipal support to the
 * reform geography. Census weights are an estimate of geographic voting shares,
 * not observed submunicipal turnout. Preserve municipal votes and elector totals. */
export function aggregateM01Votes(s: M01Settings, snapshot: M01Data, generated: readonly GeneratedMunicipalVotes[], simulationId: number): Map<string, CollegeCounts> {
  const result = new Map<string, CollegeCounts>();
  const colleges = new Map(snapshot.geography.colleges.map(c => [c.id, c]));
  const filing = new Map(s.coverage.map(c => [c.collegeId, new Set(c.listIds)]));
  const memberships = new Map<string, typeof snapshot.memberships[number][]>();
  for (const m of snapshot.memberships) if (colleges.get(m.collegeId)?.kind !== 'taa-proportional') {
    const k = `${m.chamber}:${m.municipalityCode}`, entries = memberships.get(k) ?? [];
    entries.push(m); memberships.set(k, entries);
  }
  for (const row of generated) {
    const units = new Map<string, number>();
    for (const m of memberships.get(`${row.chamber}:${row.municipalityCode}`) ?? []) units.set(m.collegeId, (units.get(m.collegeId) ?? 0) + m.population);
    const ids = [...units.keys()].sort(), weights = ids.map(id => units.get(id)!);
    if (!ids.length) throw new RangeError(`Missing ${row.chamber} municipality membership ${row.municipalityCode}`);
    const remaining = apportion(row.electors, weights, generationRng(s.seed, simulationId, 'geography-electors', row.chamber, row.municipalityCode));
    ids.forEach((id, i) => { const r = result.get(id) ?? { electors: 0, votes: new Map() }; r.electors += remaining[i]; result.set(id, r); });
    for (const vote of row.votes) {
      const eligible = ids.map(id => !!filing.get(id)?.has(vote.listId));
      let pending = vote.votes;
      const allocated = ids.map(() => 0);
      while (pending > 0) {
        const support = weights.map((w, i) => eligible[i] && remaining[i] ? w : 0);
        if (!sum(support)) throw new RangeError(`Municipal votes conflict with college filing/capacity ${row.municipalityCode}/${vote.listId}`);
        const proposal = apportion(pending, support, generationRng(s.seed, simulationId, 'geography-votes', row.chamber, row.municipalityCode, vote.listId, pending));
        const accepted = proposal.map((n, i) => Math.min(n, remaining[i]));
        accepted.forEach((n, i) => { remaining[i] -= n; allocated[i] += n; }); pending -= sum(accepted);
      }
      ids.forEach((id, i) => { const r = result.get(id)!; r.votes.set(vote.listId, (r.votes.get(vote.listId) ?? 0) + allocated[i]); });
    }
  }
  // Camera TAA is one ballot set viewed both at special and proportional level.
  for (const c of snapshot.geography.colleges.filter(c => c.kind === 'taa-proportional')) {
    const physical = snapshot.geography.colleges.filter(p => p.chamber === c.chamber && p.territoryId === c.territoryId && p.kind === 'special-uninominal');
    const r: CollegeCounts = { electors: 0, votes: new Map() };
    for (const p of physical) { const source = result.get(p.id); if (!source) continue; r.electors += source.electors; for (const [id, n] of source.votes) r.votes.set(id, (r.votes.get(id) ?? 0) + n); }
    result.set(c.id, r);
  }
  return result;
}

