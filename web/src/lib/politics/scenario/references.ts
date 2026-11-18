import { calibrateReferences, type ReferenceMoment } from '../reform-2026/vote-share/reference-family';
import { type Chamber, type NonValidReference, type PoliticsScenario, type Reference, type ReferenceScope } from './schema';

export const REFERENCE_DEFAULT_POLICY = 'reference-defaults-v1';
export function effectiveUncertaintyMultipliers(s: PoliticsScenario, listId: string, chamber: Chamber) {
  const row = s.modelSettings.M02.uncertaintyMultipliers.find(r => r.listId === listId && r.chamber === chamber);
  return { national: row?.national ?? 1, local: row?.local ?? 1 };
}
export function defaultReferenceSd(mean: number, scope: ReferenceScope): number {
  if (!Number.isFinite(mean) || mean < 0 || mean > 1) throw new RangeError('Reference mean must be a fraction in [0,1]');
  return Math.min({ national: 1, region: 2, province: 3, municipality: 5 }[scope.kind], 50 * mean, 50 * (1 - mean));
}
export function withReferenceDefault<T extends Reference | NonValidReference>(row: T): T {
  if (row.uncertainty?.origin === 'explicit') return row;
  return { ...row, uncertainty: { sdPercentagePoints: defaultReferenceSd(row.mean, row.scope),
    origin: 'default', policyVersion: REFERENCE_DEFAULT_POLICY } };
}
/** Input is an already-resolved model family (same geography/channel and disjoint effective groups).
 * Adapters must preserve the model's precedence/filing semantics before calling this boundary.
 * Proposals never overwrite explicit values or silently clip/calibrate infeasible moments. */
export function resolveReferenceFamily(rows: readonly { readonly reference: Reference | NonValidReference;
  readonly members: readonly string[] }[], free: boolean) {
  const proposed = rows.map(({ reference, members }) => ({ reference: reference.uncertainty ? reference : withReferenceDefault(reference), members }));
  // Include an explicit zero when it is the only possible partner of a missing/default SD.
  // With fixed mass elsewhere, only the two remaining variable groups are complementary.
  const uncertain = proposed.filter(r => r.reference.uncertainty!.sdPercentagePoints > 0 || proposed.length === 2);
  const mass = proposed.reduce((n, r) => n + r.reference.mean, 0);
  if (Math.abs(mass - 1) < 1e-12 && uncertain.length === 2) {
    const [a, b] = uncertain;
    const explicitA = a.reference.uncertainty!.origin === 'explicit', explicitB = b.reference.uncertainty!.origin === 'explicit';
    const sd = explicitA ? a.reference.uncertainty!.sdPercentagePoints : explicitB ? b.reference.uncertainty!.sdPercentagePoints :
      Math.min(a.reference.uncertainty!.sdPercentagePoints, b.reference.uncertainty!.sdPercentagePoints);
    for (const row of uncertain) if (row.reference.uncertainty!.origin !== 'explicit')
      row.reference = { ...row.reference, uncertainty: { ...row.reference.uncertainty!, sdPercentagePoints: sd } };
  }
  const moments: ReferenceMoment[] = proposed.map(({ reference: r, members }) => ({ id: r.id, members: [...members],
    mean: r.mean, sdPercentagePoints: r.uncertainty!.sdPercentagePoints }));
  const family = calibrateReferences(moments, free);
  return { references: proposed.map(p => p.reference), family };
}
