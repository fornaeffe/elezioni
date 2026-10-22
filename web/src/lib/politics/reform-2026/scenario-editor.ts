import type { Candidate, Chamber, PoliticalList, ReferenceScope, Subject } from './input';
import type { ReformCandidateTemplate, ReformGenerationScenario, ReformShareOverride } from './generation-types';
import { upgradeReformDraft, subjectColor, cloneReformValue } from './presentation-metadata';

export type EditingScope = 'common' | Chamber;
export const editingChambers = (scope: EditingScope): Chamber[] => scope === 'common' ? ['camera', 'senato'] : [scope];
export const referenceKey = (scope: ReferenceScope) => JSON.stringify([scope.kind, scope.kind === 'national' ? '' : scope.code]);
export const subjectKey = (s: Subject) => JSON.stringify([s.kind, s.id]);
export function reformSubjects(s: ReformGenerationScenario, ch: Chamber): Subject[] {
  return [...new Map(s.lists[ch].map(l => {
    const sub: Subject = { kind: l.coalitionId ? 'coalition' : 'list', id: l.coalitionId ?? l.id };
    return [subjectKey(sub), sub];
  })).values()];
}
export function subjectLabel(s: ReformGenerationScenario, sub: Subject, ch: Chamber): string {
  return sub.kind === 'coalition' ? s.presentation?.coalitions.find(c => c.id === sub.id)?.name ?? sub.id
    : s.lists[ch].find(l => l.id === sub.id)?.name ?? sub.id;
}
export function editableLists(s: ReformGenerationScenario, scope: EditingScope): PoliticalList[] {
  return [...new Map(editingChambers(scope).flatMap(ch => s.lists[ch]).map(l => [l.id, l])).values()];
}
function nextId(prefix: string, ids: string[]): string {
  let i = 1;
  while (ids.includes(`${prefix}-${i}`)) i++;
  return `${prefix}-${i}`;
}
function edit(s: ReformGenerationScenario, mutate: (next: ReformGenerationScenario) => void) {
  const next = upgradeReformDraft(s); mutate(next); return next;
}
/** Structural edits cannot silently rewrite adjudicated/supplied ballot data. */
function requireGeneratedData(s: ReformGenerationScenario) {
  if (s.suppliedVotes.length || s.suppliedPreferences.length) throw new Error('Questo scenario contiene voti o preferenze forniti. Riconcilia prima i dati nel JSON completo per modificare liste, coalizioni o depositi.');
}
function reconcile(s: ReformGenerationScenario) {
  for (const ch of ['camera', 'senato'] as const) {
    const ids = new Set(s.lists[ch].map(l => l.id)), subjects = new Set(reformSubjects(s, ch).map(subjectKey));
    s.coverage = s.coverage.map(c => c.chamber === ch ? { ...c, listIds: c.listIds.filter(id => ids.has(id)) } : c);
    s.correspondences = s.correspondences.filter(c => c.chamber !== ch || ids.has(c.futureListId));
    s.shares = s.shares.map(r => ids.has(r.listId) ? r : { ...r, [ch]: null, common: null });
    s.bonusLinks = s.bonusLinks.filter(l => subjects.has(subjectKey(l[ch])));
    s.candidateTemplates = s.candidateTemplates.filter(t => t.chamber !== ch || (t.role === 'ordinary'
      ? s.coverage.some(c => c.chamber === ch && c.collegeId === t.locationId && c.listIds.includes(t.ownerId))
      : subjects.has(subjectKey({ kind: s.lists[ch].some(l => l.id === t.ownerId && !l.coalitionId) ? 'list' : 'coalition', id: t.ownerId }))));
  }
  s.shares = s.shares.filter(r => r.common !== null || r.camera !== null || r.senato !== null);
  const ids = new Set([...s.lists.camera, ...s.lists.senato].map(l => l.id));
  s.presentation!.lists = s.presentation!.lists.filter(l => ids.has(l.id));
}
export function addReformList(s: ReformGenerationScenario, scope: EditingScope) {
  requireGeneratedData(s);
  return edit(s, n => {
    const id = nextId('lista', [...n.lists.camera, ...n.lists.senato].map(l => l.id));
    for (const ch of editingChambers(scope)) n.lists[ch].push({ id, name: `Nuova lista ${id.split('-').at(-1)}`, coalitionId: null, protectedMinority: null });
    n.presentation!.lists.push({ id, color: subjectColor(id) });
    // New lists start unfiled. Filing is an explicit future scenario assumption.
  });
}
export function updateReformList(s: ReformGenerationScenario, scope: EditingScope, id: string, patch: Partial<Omit<PoliticalList, 'id'>>) {
  if ('coalitionId' in patch) requireGeneratedData(s);
  return edit(s, n => {
    for (const ch of editingChambers(scope)) {
      const existing = n.lists[ch].find(l => l.id === id);
      if (!existing && scope === 'common') {
        const source = [...n.lists.camera, ...n.lists.senato].find(l => l.id === id);
        if (source) n.lists[ch].push({ ...cloneReformValue(source), ...patch });
      } else n.lists[ch] = n.lists[ch].map(l => l.id === id ? { ...l, ...patch } : l);
    }
    reconcile(n);
  });
}
export function removeReformList(s: ReformGenerationScenario, scope: EditingScope, id: string) {
  requireGeneratedData(s);
  return edit(s, n => { for (const ch of editingChambers(scope)) n.lists[ch] = n.lists[ch].filter(l => l.id !== id); reconcile(n); });
}
export function copyReformPoliticalConfiguration(s: ReformGenerationScenario, source: Chamber, target: Chamber) {
  requireGeneratedData(s);
  return edit(s, n => {
    n.lists[target] = cloneReformValue(n.lists[source]);
    n.correspondences = [...n.correspondences.filter(c => c.chamber !== target), ...n.correspondences.filter(c => c.chamber === source).map(c => ({ ...c, chamber: target }))];
    reconcile(n);
  });
}
export function addReformCoalition(s: ReformGenerationScenario) {
  return edit(s, n => { const id = nextId('coalizione', n.presentation!.coalitions.map(c => c.id));
    n.presentation!.coalitions.push({ id, name: `Nuova coalizione ${id.split('-').at(-1)}`, color: subjectColor(id) }); });
}
export function removeReformCoalition(s: ReformGenerationScenario, id: string) {
  requireGeneratedData(s);
  return edit(s, n => {
    for (const ch of ['camera', 'senato'] as const) n.lists[ch] = n.lists[ch].map(l => l.coalitionId === id ? { ...l, coalitionId: null } : l);
    n.presentation!.coalitions = n.presentation!.coalitions.filter(c => c.id !== id); reconcile(n);
  });
}
export function setReformCoverage(s: ReformGenerationScenario, ch: Chamber, colleges: string[], listId: string, filed: boolean) {
  requireGeneratedData(s);
  return edit(s, n => { n.coverage = n.coverage.map(c => c.chamber === ch && colleges.includes(c.collegeId)
    ? { ...c, listIds: filed ? [...new Set([...c.listIds, listId])] : c.listIds.filter(id => id !== listId) } : c); reconcile(n); });
}
export function setReformReference(s: ReformGenerationScenario, scope: ReferenceScope, listId: string | null, side: EditingScope, percent: number | undefined) {
  if (percent !== undefined && (!Number.isFinite(percent) || percent < 0 || percent > 100)) throw new Error('La percentuale deve essere compresa tra 0 e 100.');
  return edit(s, n => {
    const value = percent === undefined ? null : percent / 100;
    if (listId === null) {
      let row = n.abstention.find(r => referenceKey(r.scope) === referenceKey(scope));
      if (!row) { row = { scope, common: null, camera: null, senato: null }; n.abstention.push(row); }
      row[side] = value;
      n.abstention = n.abstention.filter(r => r.common !== null || r.camera !== null || r.senato !== null);
    } else {
      if (value !== null && editingChambers(side).some(ch => !n.lists[ch].some(l => l.id === listId))) throw new Error('La quota comune richiede la lista in entrambi i rami. Usa una quota per ramo oppure aggiungi la lista mancante.');
      let row = n.shares.find(r => referenceKey(r.scope) === referenceKey(scope) && r.listId === listId);
      if (!row) { row = { scope, listId, common: null, camera: null, senato: null }; n.shares.push(row); }
      row[side] = value;
      n.shares = n.shares.filter(r => r.common !== null || r.camera !== null || r.senato !== null);
    }
  });
}
export function setReformCorrespondence(s: ReformGenerationScenario, scope: EditingScope, pastListId: string, futureListId: string, factor: number | undefined) {
  if (factor !== undefined && (!Number.isFinite(factor) || factor < 0)) throw new Error('Il peso deve essere un numero non negativo.');
  return edit(s, n => {
    for (const ch of editingChambers(scope)) {
      if (factor !== undefined && !n.lists[ch].some(l => l.id === futureListId)) throw new Error(`Lista destinazione assente in ${ch}.`);
      n.correspondences = n.correspondences.filter(c => !(c.chamber === ch && c.pastListId === pastListId && c.futureListId === futureListId));
      if (factor !== undefined) n.correspondences.push({ chamber: ch, pastListId, futureListId, factor });
    }
  });
}
export function resetReformCorrespondences(s: ReformGenerationScenario, defaults: ReformGenerationScenario, scope: EditingScope, pastListId: string) {
  return edit(s, n => { for (const ch of editingChambers(scope)) {
    n.correspondences = n.correspondences.filter(c => c.chamber !== ch || c.pastListId !== pastListId);
    n.correspondences.push(...cloneReformValue(defaults.correspondences.filter(c => c.chamber === ch && c.pastListId === pastListId && n.lists[ch].some(l => l.id === c.futureListId))));
  } });
}
export function saveReformCandidate(s: ReformGenerationScenario, template: ReformCandidateTemplate) {
  return edit(s, n => {
    const sameSlot = (t: ReformCandidateTemplate) => t.chamber === template.chamber && t.role === template.role && t.locationId === template.locationId && t.ownerId === template.ownerId && t.position === template.position;
    n.candidateTemplates = n.candidateTemplates.filter(t => !sameSlot(t)).map(t => t.candidate.id === template.candidate.id ? { ...t, candidate: cloneReformValue(template.candidate) } : t);
    n.candidateTemplates.push(cloneReformValue(template));
  });
}
export function newReformCandidate(s: ReformGenerationScenario): Candidate {
  return { id: nextId('persona', s.candidateTemplates.map(t => t.candidate.id)), name: '', birthDate: '', gender: 'female', availability: { kind: 'available' }, externalCandidacies: [] };
}
export function referenceValue(rows: readonly (ReformShareOverride | ReformGenerationScenario['abstention'][number])[], scope: ReferenceScope, side: EditingScope, listId?: string): number | undefined {
  const row = rows.find(r => referenceKey(r.scope) === referenceKey(scope) && (!('listId' in r) || r.listId === listId));
  return row?.[side] === null || row?.[side] === undefined ? undefined : row[side]! * 100;
}
export function removedDependencies(before: ReformGenerationScenario, after: ReformGenerationScenario): string[] {
  return ([['corrispondenze', before.correspondences.length - after.correspondences.length],
    ['riferimenti', before.shares.length - after.shares.length], ['template candidati', before.candidateTemplates.length - after.candidateTemplates.length],
    ['collegamenti premio', before.bonusLinks.length - after.bonusLinks.length],
    ['depositi', before.coverage.reduce((n, c) => n + c.listIds.length, 0) - after.coverage.reduce((n, c) => n + c.listIds.length, 0)]] as [string, number][])
    .filter(([, count]) => count > 0).map(([label, count]) => `${count} ${label}`);
}
