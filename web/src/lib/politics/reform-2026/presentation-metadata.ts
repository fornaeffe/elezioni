import { OTHER_LIST_ID, OTHER_LIST_COLOR } from '../../core/other-list';
import type { ReformGenerationScenario, ReformPresentation } from './generation-types';
import type { ReleasedScenario } from './vote-share/release';

const palette = ['#2563eb', '#dc2626', '#16a34a', '#9333ea', '#ea580c', '#0891b2', '#be185d', '#64748b'];
/** Scenario values are JSON trees. Copying by traversal also accepts Svelte
 * read proxies, while retaining invalid numeric drafts for validation. */
export function cloneReformValue<T>(value: T): T {
  if (Array.isArray(value)) return value.map(v => cloneReformValue(v)) as T;
  if (value !== null && typeof value === 'object') return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, cloneReformValue(v)])) as T;
  return value;
}
/** Stable defaults depend on identity, never list order or the RNG streams. */
export function subjectColor(id: string): string {
  if (id === OTHER_LIST_ID) return OTHER_LIST_COLOR;
  let hash = 0;
  for (const char of id) hash = (hash * 31 + char.charCodeAt(0)) >>> 0;
  return palette[hash % palette.length];
}
export function reformPresentation(s: ReformGenerationScenario | ReleasedScenario): ReformPresentation {
  if(s.schemaVersion===4){
    const lists=[...new Map([...s.lists.camera,...s.lists.senato].map(l=>[l.id,l])).values()];
    return {name:s.presentation.name??'Scenario riforma 2026',lists:lists.map(l=>({id:l.id,color:s.presentation.lists.find(p=>p.id===l.id)?.color??subjectColor(l.id)})),
      coalitions:s.presentation.coalitions??[...new Set(lists.flatMap(l=>l.coalitionId?[l.coalitionId]:[]))].map(id=>({id,name:lists.filter(l=>l.coalitionId===id).map(l=>l.name).join(' + '),color:subjectColor(id)}))};
  }
  if (s.presentation) return s.presentation;
  const lists = [...new Map([...s.lists.camera, ...s.lists.senato].map(l => [l.id, l])).values()];
  const coalitions = [...new Set(lists.flatMap(l => l.coalitionId ? [l.coalitionId] : []))];
  return { name: 'Scenario riforma 2026', lists: lists.map(l => ({ id: l.id, color: subjectColor(l.id) })),
    coalitions: coalitions.map(id => ({ id, name: lists.filter(l => l.coalitionId === id).map(l => l.name).join(' + '), color: subjectColor(id) })) };
}
/** Explicit presentation-only upgrade for editable drafts. Parsers preserve v1
 * executed scenarios, including their archive fingerprint and original seed. */
export function upgradeReformDraft(s: ReformGenerationScenario): ReformGenerationScenario {
  return { ...cloneReformValue(s), schemaVersion: s.pollReferences ? 3 : 2, presentation: cloneReformValue(reformPresentation(s)) };
}
