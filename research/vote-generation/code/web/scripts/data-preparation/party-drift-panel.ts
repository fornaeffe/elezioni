/** Cutoff-specific, conservative research panels. Components with no explicit
 * cutoff evidence join an opaque political residual. Remaining unresolved
 * contrasts become atomic groups. This changes neither original counts nor
 * production ballots, and never fabricates internal allocations. */
import { componentResolution } from './party-drift-identifiability';

export function coarsenGroups(groups: string[][], values: number[], remap: (id: string) => string) {
  if (groups.length !== values.length) throw new Error('Group/value mismatch');
  const parent = groups.map((_, i) => i), find = (i: number): number => parent[i] === i ? i : parent[i] = find(parent[i]);
  const mapped = groups.map(g => [...new Set(g.map(remap))]);
  const first = new Map<string, number>();
  mapped.forEach((g, i) => g.forEach(id => { if (first.has(id)) parent[find(i)] = find(first.get(id)!); else first.set(id, i); }));
  const roots = [...new Set(parent.map((_, i) => find(i)))];
  return roots.map(root => ({ members: [...new Set(mapped.flatMap((g, i) => find(i) === root ? g : []))].sort(),
    value: values.reduce((s, v, i) => s + (find(i) === root ? v : 0), 0), originalIndices: parent.flatMap((_, i) => find(i) === root ? [i] : []) }));
}
export function makeAdmissiblePanel(basis: string[], information: { id: string; groups: string[][] }[], explicitEvidence: Set<string>, seeds = ['pd', 'm5s', 'lega', 'fi-pdl', 'fdi'], futureGroups: string[][] = []) {
  // Keep the joint-list closure of the main research parties; every remaining
  // source list is retained inside an explicitly opaque research residual.
  const relevant = new Set(seeds.filter(id => basis.includes(id))); let changed = true;
  while (changed) {
    changed = false;
    for (const g of [...information.flatMap(row=>row.groups),...futureGroups]) if (g.some(id => relevant.has(id))) for (const id of g)
      if (!relevant.has(id) && explicitEvidence.has(id)) { relevant.add(id); changed = true; }
  }
  // Never use an Other complement for closure: callers supply explicit groups
  // here and add the actual complement only in the information callback below.
  const parent = new Map(basis.map(id => [id, relevant.has(id) && explicitEvidence.has(id) ? id : 'research-other']));
  parent.set('research-other','research-other');
  const find = (id: string): string => { const p = parent.get(id); if (!p) throw new Error('Unknown panel component'); return p === id ? p : (parent.set(id,find(p)),find(p)); };
  let audit;
  for (let iteration = 0; iteration <= basis.length; iteration++) {
    const ids = [...new Set(basis.map(find))].sort();
    const rows = information.map(row => ({ id:row.id,groups:coarsenGroups(row.groups,row.groups.map(()=>0),find).map(g=>g.members) }));
    audit = componentResolution(ids,rows);
    if (audit.resolved) break;
    for (const direction of audit.nullDirections) {
      const ids = direction.map(x=>find(x.id));
      // An unobserved singleton is absorbed by the observed opaque residual;
      // it is not given a guessed party share or a pre-birth trajectory.
      const root = ids.includes('research-other') || ids.length === 1 ? 'research-other' : [...ids].sort()[0];
      ids.forEach(id=>parent.set(id,root));
    }
    if (new Set(basis.map(find)).size === ids.length)
      throw new Error('Unsupported research residual: no cutoff evidence distinguishes this support. Supply reference information or retain an observed residual group.');
    if (iteration === basis.length) throw new Error('Panel resolution did not stabilize');
  }
  const mapping = Object.fromEntries(basis.map(id=>[id,find(id)]));
  return { ids:[...new Set(Object.values(mapping))].sort(),mapping,audit,
    policy:'Main-party joint closure; unsupported/unrequested identities are opaque residual support; unresolved remaining contrasts become atomic before fitting' };
}
