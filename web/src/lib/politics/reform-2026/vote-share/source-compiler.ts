/** Explicit source selection/membership/date boundary for Node and future workers. */
import { day, FAST_SHARE_MODEL, type NationalSource, type NationalElection } from './types';
import { normalizeSource } from './information';
import type { ProfileSource } from './profiles';
import type { CompactSource, OriginalRow } from './source-types';
import { decodeSourceRows, rowFlags, sourceKey, validateSourceEnvelope, type AssetBytes } from './source-codec';

export interface SourceSelection {
  referenceDate: string; electionIds: string[]; pollIds: string[]; components: string[];
  /** Complete edits with provenance. Unedited choices retain the bundled correspondence. */
  sourceMemberships?: Record<string, { members: string[]; provenance: string }>;
  /** Explicit whole-component pooling, never fractional splitting. */
  componentMap?: Record<string, string>;
  /** Electorates are conditional scenario inputs, never held-out future counts. */
  targets: { code: string; electors: number }[]; targetContext: 'camera' | 'european';
}
/** Finest observable disjoint coarsening; this sums original groups, never allocates them. */
export function observableGroups(groups: string[][], counts: number[]) {
  if (groups.length !== counts.length || groups.some(g => !g.length || new Set(g).size !== g.length)) throw new Error('Invalid original groups');
  const parent = groups.map((_, i) => i), first = new Map<string, number>();
  const root = (i: number): number => parent[i] === i ? i : parent[i] = root(parent[i]);
  groups.forEach((g, i) => g.forEach(id => { const previous = first.get(id); if (previous !== undefined) parent[root(i)] = root(previous); else first.set(id, i); }));
  const result = new Map<number, { members: Set<string>; count: number }>();
  groups.forEach((g, i) => { const r = root(i), row = result.get(r) ?? { members: new Set<string>(), count: 0 }; g.forEach(id => row.members.add(id)); row.count += counts[i]; result.set(r, row); });
  return [...result.values()].map(r => ({ members: [...r.members].sort(), count: r.count })).sort((a, b) => JSON.stringify(a.members).localeCompare(JSON.stringify(b.members)));
}
export function resolvedBasis(components: string[], observations: string[][][]): boolean {
  // Contrast the original conditional offered sets at an interior reference.
  // A one-choice question is identically 1 and supplies NO distinguishing row.
  const pivots = new Map<number, number[]>(), index = new Map(components.map((id, i) => [id, i]));
  const rows = [components.map(() => 1)];
  for (const groups of observations) {
    const offered = groups.flat();
    if (!offered.length || new Set(offered).size !== offered.length) throw new Error('Invalid offered partition');
    for (const g of groups) {
      const row = Array(components.length).fill(0);
      for (const id of offered) { const j = index.get(id); if (j === undefined) throw new Error('Unknown component'); row[j] = -g.length / offered.length; }
      for (const id of g) row[index.get(id)!] += 1;
      rows.push(row);
    }
  }
  for (const row of rows) {
    for (let c = 0; c < row.length; c++) {
      if (Math.abs(row[c]) < 1e-10) continue;
      const p = pivots.get(c);
      if (p) { const scale = row[c]; for (let j = c; j < row.length; j++) row[j] -= scale * p[j]; }
      else { const scale = row[c]; for (let j = c; j < row.length; j++) row[j] /= scale; pivots.set(c, row); break; }
    }
    if (pivots.size === components.length) return true;
  }
  return false;
}
export async function compileSource(source: CompactSource, readChunk: (path: string) => Promise<AssetBytes>, selection: SourceSelection) {
  await validateSourceEnvelope(source);
  const p = source.payload, cutoff = day(selection.referenceDate), components = [...selection.components].sort(), known = new Set(components);
  if (cutoff < day(p.range.first) || components.length < 1 || components.length > 128 || known.size !== components.length || components.some(id => !id)) throw new Error('Unsupported source date/basis');
  const select = <T extends { id: string }>(ids: string[], all: T[]) => {
    if (new Set(ids).size !== ids.length) throw new Error('Duplicate source selection');
    return ids.map(id => { const row = all.find(r => r.id === id); if (!row) throw new Error('Unknown source ID: ' + id); return row; });
  };
  const selectedEvents = select(selection.electionIds, p.events).sort((a, b) => a.id.localeCompare(b.id));
  const selectedPolls = select(selection.pollIds, p.polls).sort((a, b) => a.id.localeCompare(b.id));
  if (!selectedEvents.length || selectedEvents.some(e => e.context === 'senato' || e.role !== 'reference' || e.minimumAge !== 18)) throw new Error('Senate requires FV-04 adaptation; 25+ observations are audit-only');
  if (selectedEvents.some(e => day(e.date) > cutoff || day(e.availableDate) > cutoff) || selectedPolls.some(q => day(q.date) > cutoff || day(q.availableDate) > cutoff || day(q.fieldworkEnd) > cutoff)) throw new Error('Source unavailable at reference cutoff');
  if (selectedPolls.some(q => q.status !== 'supported' || q.context === 'senato' || q.roundingWidth !== 0.001)) throw new Error('Unsupported/excluded poll treatment');
  if (new Set(selectedPolls.map(q => q.surveyId)).size !== selectedPolls.length) throw new Error('Duplicate survey evidence');
  const choices = new Map(p.choices.map(c => [c.id, c])), units = new Map(p.units.map(u => [u.id, u]));
  for (const [id, edit] of Object.entries(selection.sourceMemberships ?? {})) {
    const c = choices.get(id);
    if (!c || !edit.provenance.trim() || !edit.members.length || new Set(edit.members).size !== edit.members.length || c.opaque && edit.members.length > 1) throw new Error('Invalid membership edit or unsupported opaque split');
  }
  const originalIds = new Set([...p.choices.flatMap(c => c.members), ...p.polls.flatMap(q => q.normalized.flatMap(a => a.members)), ...Object.values(selection.sourceMemberships ?? {}).flatMap(e => e.members)]);
  if (Object.entries(selection.componentMap ?? {}).some(([id, target]) => !originalIds.has(id) || !known.has(target))) throw new Error('Unknown component pooling entry');
  if (!['camera', 'european'].includes(selection.targetContext)) throw new Error('Unsupported profile context');
  const remap = (ids: string[]) => [...new Set(ids.map(id => selection.componentMap?.[id] ?? id))].sort();
  const rawMembersFor = (id: string) => {
    const c = choices.get(id); if (!c) throw new Error('Unknown choice');
    return selection.sourceMemberships?.[id]?.members ?? c.members;
  };
  const membersFor = (id: string) => {
    const members = remap(rawMembersFor(id));
    if (members.some(id => !known.has(id))) throw new Error('Incomplete selected membership/basis: ' + id);
    return members;
  };
  const warnings: string[] = [], elections: NationalElection[] = [], historical: ProfileSource['elections'] = [], information: string[][][] = [], excludedLocal: { event: string; unit: string; reason: string }[] = [];
  const supportStart = new Map<string, number>();
  const observed = (ids: string[], date: string) => ids.forEach(id => supportStart.set(id, Math.min(supportStart.get(id) ?? Infinity, day(date))));
  for (const event of selectedEvents) {
    const rows = await decodeSourceRows(source, event.id, await readChunk(event.chunk.path));
    const resident = rows.filter(r => units.get(r.unit)!.match !== 'unlocated');
    const totalGroups: string[][] = [], totals: number[] = [];
    const local: ProfileSource['elections'][number]['rows'] = [];
    let electors = 0, valid = 0;
    for (const r of resident) {
      const rawMembers = r.choices.flatMap(c => rawMembersFor(c.id));
      if (new Set(rawMembers).size !== rawMembers.length) throw new Error('Component competes in multiple original source choices; correct memberships before pooling');
      const groups = r.choices.map(c => membersFor(c.id)), counts = r.choices.map(c => c.count);
      const coarse = observableGroups(groups, counts), unit = units.get(r.unit)!, count = counts.reduce((n, x) => n + x, 0);
      r.choices.forEach(c => observed(rawMembersFor(c.id), event.date));
      // All resident source counts, including unmatched places, remain national.
      totalGroups.push(...coarse.map(g => g.members)); totals.push(...coarse.map(g => g.count)); electors += r.electors; valid += count;
      if (rowFlags(r, unit.match) & 1) { excludedLocal.push({ event: event.id, unit: r.unit, reason: 'Invalid original denominator; retained nationally, no local likelihood' }); continue; }
      local.push({ id: unit.targetCode ?? unit.id, region: unit.region, electors: r.electors, nonValid: r.electors - count, groups: coarse.map(g => g.members), counts: coarse.map(g => g.count) });
    }
    if (resident.length !== rows.length) warnings.push(event.id + ': unlocated fuori-sede retained in source, excluded from resident-denominator fit');
    const coarse = observableGroups(totalGroups, totals), context = event.context as 'camera' | 'european';
    // Use the same observable partition in the empirical local baseline, without
    // allocating any joint count. Original offered sets stay in the source chunk.
    const block = new Map(coarse.flatMap((g, i) => g.members.map(id => [id, i] as const)));
    for (const r of local) {
      const buckets = new Map<number, number>();
      r.groups.forEach((g, i) => { const b = block.get(g[0])!; if (g.some(id => block.get(id) !== b)) throw new Error('Broken observable partition'); buckets.set(b, (buckets.get(b) ?? 0) + r.counts[i]); });
      const entries = [...buckets].sort(([a], [b]) => a - b);
      r.groups = entries.map(([i]) => coarse[i].members); r.counts = entries.map(([, n]) => n);
    }
    information.push(coarse.map(g => g.members));
    elections.push({ id: event.id, date: event.date, context, electors, groups: coarse.map(g => g.members), counts: [electors - valid, ...coarse.map(g => g.count)] });
    historical.push({ id: event.id, date: event.date, context, rows: local });
  }
  for (const q of selectedPolls) for (const a of q.normalized.filter(a => !a.other)) observed(a.members, q.date);
  if (components.some(id => ![...supportStart.keys()].some(raw => (selection.componentMap?.[raw] ?? raw) === id))) throw new Error('No observed support for fitted component; new support needs a reference');
  const polls: NationalSource['polls'] = selectedPolls.map(q => {
    // Construct Other BEFORE pooling. A named minor answer and unnamed support
    // may both map to the same fitted residual; their original mass must survive.
    const namedMembers = q.normalized.filter(a => !a.other).flatMap(a => a.members), named = new Set(namedMembers);
    if (named.size !== namedMembers.length || q.normalized.filter(a => a.other).length > 1) throw new Error('Overlapping original poll choices/Other answers');
    const groups = q.normalized.map(a => remap(a.other ? [...supportStart.keys()].filter(id => !named.has(id) && supportStart.get(id)! <= day(q.date)) : a.members));
    if (groups.some(g => !g.length || g.some(id => !known.has(id)))) throw new Error('Unsupported poll offered set/Other complement');
    const coarse = observableGroups(groups, q.normalized.map(a => a.share));
    const total = coarse.reduce((n, g) => n + g.count, 0);
    if (Math.abs(total - 1) > 1e-8 || coarse.some(g => g.count <= 0) || coarse.length < 2) throw new Error('Unsupported poll shares');
    information.push(coarse.map(g => g.members));
    return { id: q.id, surveyId: q.surveyId, date: q.date, context: q.context as 'camera' | 'european', groups: coarse.map(g => g.members), shares: coarse.map(g => g.count / total), sample: q.effectiveSample,
      errorGroup: q.errorGroup, availableDate: q.availableDate, fieldworkEnd: q.fieldworkEnd };
  });
  if (!resolvedBasis(components, information)) throw new Error('UNRESOLVED_COMPONENT_BASIS: pool unsupported splits or supply distinguishing source information');
  if (new Set(selection.targets.map(t => t.code)).size !== selection.targets.length || selection.targets.some(t => !Number.isSafeInteger(t.electors) || t.electors <= 0)) throw new Error('Invalid conditional target electorate');
  const targets = selection.targets.map(t => { const g = p.geography.targets.find(g => g.code === t.code); if (!g) throw new Error('Unknown target geography'); return { id: t.code, region: g.region, electors: t.electors }; }).sort((a, b) => a.id.localeCompare(b.id));
  const national: NationalSource = normalizeSource({ components, referenceDate: selection.referenceDate, elections, polls });
  const profiles: ProfileSource = { components, referenceDate: selection.referenceDate, targetContext: selection.targetContext, nationalElections: elections, elections: historical, targets };
  if (cutoff > day(p.range.latestObservation)) warnings.push('Extrapolation beyond bundled election vintage ' + p.range.latestObservation);
  const nationalInputKey = await sourceKey('national-input-v1', { modelId: FAST_SHARE_MODEL, contextPolicy: 'camera-european-separate-v1', national }),
    profileInputKey = await sourceKey('profile-input-v1', { modelId: FAST_SHARE_MODEL, profilePolicy: 'observable-partition-profile-v1', ...profiles, geographyVersion: p.geography.version });
  const provenanceId = await sourceKey('source-provenance-v1', { sourceId: source.id, nationalInputKey, profileInputKey, memberships: selection.sourceMemberships ?? {} });
  return { national, profiles, nationalInputKey, profileInputKey, provenanceId, warnings, excludedLocal };
}
