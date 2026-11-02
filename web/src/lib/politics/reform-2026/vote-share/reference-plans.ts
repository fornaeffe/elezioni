import type { Chamber, ReferenceScope } from '../input';
import type { FastGenerationScenario, FastReference, PhysicalCell } from './forecast-types';
import { calibrateReferences, drawReference, type ReferenceFamily } from './reference-family';
import { forecastRandom } from './forecast-random';
export const scopeKey = (s: ReferenceScope) => JSON.stringify([s.kind, s.kind === 'national' ? '' : s.code]);
export const scopeMatches = (cell: PhysicalCell, s: ReferenceScope) =>
  s.kind === 'national' ||
  (s.kind === 'municipality' ? cell.municipality : s.kind === 'region' ? cell.region : cell.province) ===
    s.code;
type Channel = 'common' | Chamber;
/** List identity matters when one component files under different lists in
 * disjoint colleges. Only a sole-new-component list is a component alias. */
export function referenceSubjectKey(s: FastGenerationScenario, row: FastReference, chamber: Chamber) {
  if (row.subject.kind === 'component') return 'new:' + row.subject.id;
  const members = referenceMembers(s, row, chamber);
  return members.length === 1 && s.components.some((c) => c.id === members[0] && c.kind === 'new')
    ? 'new:' + members[0]
    : 'list:' + row.subject.id;
}
export interface ReferencePlan {
  scope: ReferenceScope;
  key: string;
  political: Partial<Record<Channel, { family: ReferenceFamily; rows: Map<string, FastReference> }>>;
  nonValid: Partial<Record<Channel, ReferenceFamily>>;
}
export function referenceMembers(s: FastGenerationScenario, row: FastReference, chamber: Chamber) {
  if (row.subject.kind === 'component') {
    if (!s.components.some((c) => c.id === row.subject.id && c.kind === 'new'))
      throw new Error('REFERENCE_SUBJECT: component references require a declared new component');
    return [row.subject.id];
  }
  const list = s.lists[chamber].find((l) => l.id === row.subject.id);
  if (!list) throw new Error('REFERENCE_SUBJECT: unknown ' + chamber + ' list ' + row.subject.id);
  return [...list.componentIds].sort();
}
export function referencePlans(
  s: FastGenerationScenario,
  cells: Record<Chamber, PhysicalCell[]>,
  chambers: Chamber[],
  filedList: (cell: PhysicalCell, component: string) => string | undefined
) {
  const all = [...s.references, ...s.nonValidReferences];
  if (new Set(all.map((r) => r.id)).size !== all.length) throw new Error('Duplicate reference ID');
  const scopes = [...new Map(all.map((r) => [scopeKey(r.scope), r.scope])).values()].sort(
    (a, b) =>
      ['national', 'region', 'province', 'municipality'].indexOf(a.kind) -
        ['national', 'region', 'province', 'municipality'].indexOf(b.kind) ||
      scopeKey(a).localeCompare(scopeKey(b))
  );
  const output: ReferencePlan[] = [];
  for (const scope of scopes) {
    if (!chambers.some((ch) => cells[ch].some((c) => scopeMatches(c, scope))))
      throw new Error('REFERENCE_SCOPE: unknown/empty geography');
    const plan: ReferencePlan = { scope, key: scopeKey(scope), political: {}, nonValid: {} };
    for (const channel of ['common', ...chambers] as Channel[]) {
      const relevant = channel === 'common' ? chambers : [channel];
      const selected = s.references
        .filter((r) => r.channel === channel && scopeKey(r.scope) === plan.key)
        .sort((a, b) => a.id.localeCompare(b.id));
      if (selected.length) {
        const unique = new Map<string, FastReference>();
        for (const row of selected) {
          const members = referenceMembers(s, row, relevant[0]),
            key = referenceSubjectKey(s, row, relevant[0]),
            old = unique.get(key);
          if (relevant.some((ch) => JSON.stringify(referenceMembers(s, row, ch)) !== JSON.stringify(members)))
            throw new Error('REFERENCE_CONFLICT: common list membership differs by chamber');
          if (old && (old.mean !== row.mean || old.sdPercentagePoints !== row.sdPercentagePoints))
            throw new Error('REFERENCE_CONFLICT: duplicate component/list target');
          if (!old) unique.set(key, row);
        }
        const rows = [...unique.values()],
          free = relevant.some((ch) =>
            cells[ch]
              .filter((c) => scopeMatches(c, scope))
              .some((c) =>
                s.components.some((component) => {
                  const list = filedList(c, component.id);
                  return (
                    list !== undefined &&
                    !rows.some(
                      (r) =>
                        referenceMembers(s, r, ch).includes(component.id) &&
                        (r.subject.kind === 'component' || r.subject.id === list)
                    )
                  );
                })
              )
          );
        const family = calibrateReferences(
          rows.map((r) => ({
            id: r.id,
            members: [referenceSubjectKey(s, r, relevant[0])],
            mean: r.mean,
            sdPercentagePoints: r.sdPercentagePoints
          })),
          free,
          !free
        );
        plan.political[channel] = { family, rows: new Map(rows.map((r) => [r.id, r])) };
      }
      const nv = s.nonValidReferences.filter((r) => r.channel === channel && scopeKey(r.scope) === plan.key);
      if (nv.length > 1) throw new Error('REFERENCE_CONFLICT: duplicate non-valid channel/scope');
      if (nv.length)
        plan.nonValid[channel] = calibrateReferences(
          [
            {
              id: nv[0].id,
              members: ['non-valid'],
              mean: nv[0].mean,
              sdPercentagePoints: nv[0].sdPercentagePoints
            }
          ],
          true
        );
    }
    if (
      s.references.some((r) => !['common', ...chambers].includes(r.channel)) ||
      s.nonValidReferences.some((r) => !['common', ...chambers].includes(r.channel))
    )
      throw new Error('Reference targets a non-renewed chamber');
    output.push(plan);
  }
  return output;
}
export function commonAnchor(value: number, camera: number, senato: number, gapMultiplier: number) {
  if (value === 0 || value === 1) return { camera: value, senato: value, fallback: false };
  const fallback = !(camera > 0 && camera < 1 && senato > 0 && senato < 1);
  const contrast = fallback
    ? 0
    : 0.5 *
      gapMultiplier *
      Math.max(-1, Math.min(1, Math.log(senato / (1 - senato)) - Math.log(camera / (1 - camera))));
  const odds = Math.log(value / (1 - value));
  return {
    camera: 1 / (1 + Math.exp(-(odds - contrast / 2))),
    senato: 1 / (1 + Math.exp(-(odds + contrast / 2))),
    fallback
  };
}
export function sampledPlan(plan: ReferencePlan, seed: string, simulationId: number, central: boolean) {
  const political: Partial<Record<Channel, ReturnType<typeof drawReference>>> = {},
    nonValid: Partial<Record<Channel, number>> = {};
  for (const channel of ['common', 'camera', 'senato'] as Channel[]) {
    const p = plan.political[channel];
    if (p)
      political[channel] = drawReference(
        p.family,
        forecastRandom(seed, simulationId, 'reference:' + plan.key + ':' + channel),
        central
      );
    const n = plan.nonValid[channel];
    if (n)
      nonValid[channel] = drawReference(
        n,
        forecastRandom(seed, simulationId, 'non-valid-reference:' + plan.key + ':' + channel),
        central
      ).values[0];
  }
  return { political, nonValid };
}
