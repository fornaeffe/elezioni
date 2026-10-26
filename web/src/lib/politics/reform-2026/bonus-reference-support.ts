import type { Chamber } from './input';
import type { ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import type { ProjectedMunicipality } from './paired-vote-generation';
import { sum } from './generation-math';

export const bonusKey = (...parts: (string | number)[]) => JSON.stringify(parts);

/** Fractional counterpart of college aggregation, before shocks and integer
 * sampling. Preserve reference turnout, census splits and filing zeros. This
 * consumes the existing estimates verbatim; it supplies no territorial prior.
 * Supplied election votes are deliberately not reference-date information. */
export function bonusReferenceSupport(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot,
  units: readonly ProjectedMunicipality[]): Map<string, number> {
  const totals = new Map<string, number>();
  const colleges = new Map(snapshot.seats.geography.colleges.map(c => [c.id, c]));
  const filing = new Map(s.coverage.map(c => [c.collegeId, new Set(c.listIds)]));
  const memberships = new Map<string, Map<string, number>>();
  for (const m of snapshot.memberships) if (colleges.get(m.collegeId)?.kind !== 'taa-proportional') {
    const k = bonusKey(m.chamber, m.municipalityCode), rows = memberships.get(k) ?? new Map<string, number>();
    rows.set(m.collegeId, (rows.get(m.collegeId) ?? 0) + m.population); memberships.set(k, rows);
  }
  const chambers: Chamber[] = s.renewal === 'joint' ? ['camera', 'senato'] : [s.renewal];
  for (const u of units) for (const ch of chambers) {
    const rows = [...(memberships.get(bonusKey(ch, u.municipality.code)) ?? [])].sort(([a], [b]) => a.localeCompare(b));
    const population = sum(rows.map(([, n]) => n));
    if (!population) throw new RangeError(`Missing reference geography ${ch}/${u.municipality.code}`);
    const remaining = rows.map(([, n]) => u.municipality[ch].electors * n / population);
    for (const [listId, share] of u[ch].shares) {
      let pending = u.municipality[ch].electors * (1 - u[ch].abstention) * share;
      // A saturated split can redirect votes to another filed college, exactly
      // as the runtime aggregation does, without introducing rounding noise.
      for (let round = 0; pending > 0 && round <= rows.length; round++) {
        const weights = rows.map(([id, n], i) => filing.get(id)?.has(listId) && remaining[i] > 0 ? n : 0);
        const mass = sum(weights);
        if (!mass) {
          if (pending > 1e-7) throw new RangeError(`Reference votes conflict with filing/capacity ${ch}/${u.municipality.code}/${listId}`);
          break;
        }
        const proposal = weights.map(w => pending * w / mass);
        const accepted = proposal.map((n, i) => Math.min(n, remaining[i]));
        for (const [i, [id]] of rows.entries()) {
          remaining[i] -= accepted[i];
          const college = colleges.get(id)!;
          if (college.kind !== 'ordinary') continue;
          const k = bonusKey(ch, college.territoryId, listId);
          totals.set(k, (totals.get(k) ?? 0) + accepted[i]);
        }
        pending = Math.max(0, pending - sum(accepted));
        if (accepted.every((n, i) => n === proposal[i])) break;
      }
    }
  }
  return totals;
}
