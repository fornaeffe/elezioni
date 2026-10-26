import { generateReformSimulation, prepareReformGeneration } from './generation';
import type { ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import type { Chamber, ReferenceScope } from './input';

export interface PreviewSelection { chamber: Chamber; role: 'ordinary' | 'bonus' | 'special'; locationId: string; ownerId: string }
/** A single deterministic sample checks generation compatibility. It is not a
 * scrutiny result, nor proof that every future simulation is computable. */
export function previewReformScenario(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot, selection?: PreviewSelection) {
  const prepared = prepareReformGeneration(s, snapshot);
  const { input, trace } = generateReformSimulation(prepared, 1);
  const chambers = input.renewal.kind === 'joint' ? [input.renewal.camera, input.renewal.senato] : [input.renewal.chamber];
  const people = new Map(input.candidates.map(p => [p.id, p]));
  const placements: { position: number; candidateId: string; name: string; gender: string; birthDate: string; listId: string | null; ordinaryLocation: string | null; ordinaryPosition: number | null }[] = [];
  const ch = chambers.find(c => c.chamber === selection?.chamber);
  if (ch && selection) {
    const add = (id: string, position: number, listId: string | null) => {
      const p = people.get(id)!;
      const bonusTerritory = selection.role === 'bonus' ? selection.locationId : null;
      const bonusPeople = new Set(ch.bonusSlates.flatMap(b => b.candidates.map(c => c.candidateId)));
      const ordinary = ch.ordinarySlates.find(o => o.candidateIds.includes(id) && (listId === null || o.listId === listId) &&
        (selection.role !== 'ordinary' || o.collegeId === selection.locationId) &&
        (bonusTerritory === null || ch.colleges.find(c => c.id === o.collegeId)?.territoryId === bonusTerritory &&
          o.candidateIds.slice(0, o.candidateIds.indexOf(id)).every(id => bonusPeople.has(id))));
      placements.push({ position, candidateId: id, name: p.name, gender: p.gender, birthDate: p.birthDate, listId,
        ordinaryLocation: ordinary?.collegeId ?? null, ordinaryPosition: ordinary ? ordinary.candidateIds.indexOf(id) + 1 : null });
    };
    if (selection.role === 'ordinary') ch.ordinarySlates.find(o => o.collegeId === selection.locationId && o.listId === selection.ownerId)?.candidateIds.forEach((id, i) => add(id, i + 1, selection.ownerId));
    if (selection.role === 'bonus') ch.bonusSlates.find(b => b.territoryId === selection.locationId && b.subject.id === selection.ownerId)?.candidates.forEach((c, i) => add(c.candidateId, i + 1, c.affiliationListId));
    if (selection.role === 'special') ch.specialCandidatures.filter(c => c.collegeId === selection.locationId && c.linkedListIds.some(id => { const list = ch.lists.find(l => l.id === id)!; return (list.coalitionId ?? list.id) === selection.ownerId; })).forEach(c => add(c.candidateId, 1, c.affiliationListId));
  }
  const scopes = [...new Map([{ kind: 'national' } as ReferenceScope, ...s.abstention.map(r => r.scope)].map(scope => [JSON.stringify(scope), scope])).values()];
  const abstention = scopes.map(scope => {
    const group = prepared.references.units.filter(u => scope.kind === 'national' || (scope.kind === 'municipality' ? u.municipality.code : scope.kind === 'province' ? u.municipality.provinceId : u.municipality.regionId) === scope.code);
    const value = (ch: Chamber) => {
      if (s.renewal !== 'joint' && s.renewal !== ch) return null;
      const electors = group.reduce((n, u) => n + u.municipality[ch].electors, 0);
      return electors ? group.reduce((n, u) => n + u.municipality[ch].electors * u[ch].abstention, 0) / electors : null;
    };
    return { scope, camera: value('camera'), senato: value('senato') };
  });
  return { references: trace.references, abstention, warnings: trace.warnings, normalization: trace.normalization, placements,
    bonusSelection: trace.bonusSelection };
}
export type ReformEditorPreview = ReturnType<typeof previewReformScenario>;
