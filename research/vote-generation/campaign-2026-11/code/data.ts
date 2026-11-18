/** Live research adapter. Archived FV-09 sources are deliberately not imported. */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { loadCandidate, loadCandidateSource } from '../../../../web/src/lib/politics/reform-2026/vote-share/release';
import { decodeSourceRows } from '../../../../web/src/lib/politics/reform-2026/vote-share/source-codec';
import { readBytes } from '../../../../web/scripts/data-preparation/fast-share-release-io';
import type { CompactSource, SourceGeography } from '../../../../web/src/lib/politics/reform-2026/vote-share/source-types';

export const categories = ['fdi', 'fi-pdl', 'lega', 'm5s', 'other', 'pd'];
export const sum = (x: ArrayLike<number>) => Array.from(x).reduce((a, b) => a + b, 0);
export const years = (a: string, b: string) => (Date.parse(a) - Date.parse(b)) / (365.25 * 86400000);
export const cutoffFor = (date: string, horizon: number) => new Date(Date.parse(date) - horizon * 86400000).toISOString().slice(0, 10);
export interface Row { id: string; region: string; electors: number; votes: number[] }
export interface Event { id: string; kind: 'camera' | 'european' | 'senato'; date: string; available: string; age: number; rows: Row[]; offered: boolean[] }
export interface Poll { id: string; date: string; available: string; end: string; sample: number; groups: string[][]; shares: number[]; errorGroup: string }
export interface StudyData { source: CompactSource; events: Event[]; polls: Poll[]; geography: SourceGeography; chamber: { sharedVariance: number; residualCorrelation: number; gapMultiplier: number }; sigma: {national: number; region: number; local: number}; audit: unknown }

/** Whole original tickets are assigned to observable electoral families. No joint
 * ticket is split into invented component counts. PD+Art1 belongs to PD; minor
 * lists without one of the five named identities belong to changing Other. */
export function family(members: string[]) {
  const named = [...new Set(members.filter(c => categories.includes(c) && c !== 'other'))];
  if (named.length > 1) throw new Error('AMBIGUOUS_RESEARCH_FAMILY:' + members.join('|'));
  return named[0] ?? 'other';
}
export async function loadData(root: string): Promise<StudyData> {
  const directory = resolve(root, 'web/static/data/reform-2026/fast-share'), read = readBytes(directory);
  const loaded = await loadCandidate(JSON.parse(await readFile(resolve(directory, 'candidate.json'), 'utf8')), read, {validateGeneration: false});
  const source = await loadCandidateSource(loaded.catalog, read), units = new Map(source.payload.units.map(u => [u.id, u]));
  const choices = new Map(source.payload.choices.map(c => [c.id, c])), geo = new Map(source.payload.geography.targets.map(g => [g.code, g]));
  const events: Event[] = [], exclusions: Record<string, Record<string, number>> = {};
  for (const e of source.payload.events) {
    const grouped = new Map<string, Row>(), offered = categories.map(() => false), excluded: Record<string, number> = {};
    for (const r of await decodeSourceRows(source, e.id, await read(e.chunk.path))) {
      const u = units.get(r.unit)!, valid = sum(r.choices.map(c => c.count));
      const reason = ['02', '04'].includes(u.region) ? 'special-ballot-regions' :
        u.match !== 'same-place' || !u.targetCode || !geo.has(u.targetCode) ? 'unmatched-geography' :
        r.electors <= 0 || valid <= 0 || valid > r.electors ? 'invalid-denominator' : null;
      if (reason) { excluded[reason] = (excluded[reason] ?? 0) + r.electors; continue; }
      const id = u.targetCode!;
      if (grouped.has(id)) throw new Error('DUPLICATE_TARGET:' + e.id + '/' + id);
      const votes = categories.map(() => 0);
      for (const c of r.choices) { const j = categories.indexOf(family(choices.get(c.id)!.members)); votes[j] += c.count; offered[j] = true; }
      grouped.set(id, {id, region: u.region, electors: r.electors, votes});
    }
    events.push({id:e.id, kind:e.context, date:e.date, available:e.availableDate, age:e.minimumAge, rows:[...grouped.values()].sort((a,b)=>a.id.localeCompare(b.id)), offered});
    exclusions[e.id] = excluded;
  }
  const polls: Poll[] = [];
  for (const p of source.payload.polls.filter(p => p.status === 'supported' && p.context === 'camera')) {
    const named = new Set(p.normalized.filter(a => !a.other).map(a => family(a.members))), buckets = new Map<string, {members:string[]; share:number}>();
    // Coarsen duplicate pooled answers and an overlapping original Other once.
    const raw = p.normalized.map(a => ({members: a.other ? categories.filter(c => !named.has(c)) : [family(a.members)], share:a.share}));
    const parent = new Map(categories.map(c => [c,c]));
    const top = (c:string):string => parent.get(c) === c ? c : top(parent.get(c)!);
    for (const a of raw) for (const c of a.members.slice(1)) parent.set(top(c), top(a.members[0]));
    for (const a of raw) {
      if (!a.members.length) continue;
      const key = top(a.members[0]), b = buckets.get(key) ?? {members:categories.filter(c => top(c) === key), share:0};
      b.share += a.share; buckets.set(key,b);
    }
    const groups = [...buckets.values()].filter(a => a.share > 0);
    if (groups.length < 2 || Math.abs(sum(groups.map(a=>a.share))-1)>1e-8) continue;
    polls.push({id:p.id, date:p.date, available:p.availableDate, end:p.fieldworkEnd, sample:p.effectiveSample,
      groups:groups.map(a=>a.members), shares:groups.map(a=>a.share), errorGroup:p.errorGroup});
  }
  const chamber = loaded.context.models.chambers.profiles.find(p=>p.id === 'central-v1')!;
  return {source, events:events.sort((a,b)=>a.date.localeCompare(b.date)||a.id.localeCompare(b.id)), polls,
    geography:source.payload.geography, chamber, sigma:loaded.context.models.chambers.sigmaPerSqrtDay,
    audit:{sourceId:source.id, sourceVersions:source.sourceVersions, exclusions, familyPolicy:'whole-ticket-five-named-plus-changing-other-v1'}};
}
export function totals(rows: Row[]) { return {electors:sum(rows.map(r=>r.electors)), votes:categories.map((_,j)=>sum(rows.map(r=>r.votes[j])))}; }
export function eligible(data: StudyData, date: string, horizon: number, regime: string) {
  const cutoff = cutoffFor(date, horizon);
  const history = data.events.filter(e=>e.kind!=='senato' && e.date < cutoff && e.available <= cutoff && (regime==='mixed'||e.kind==='camera'));
  const candidates = data.polls.filter(p=>p.date<=cutoff && p.available<=cutoff && p.end<=cutoff && years(cutoff,p.available)*365.25<=180);
  const newest = candidates.map(p=>p.available).sort().at(-1);
  const polls = newest ? candidates.filter(p=>years(newest,p.available)*365.25<=35) : [];
  if (!history.length) throw new Error('NO_ELIGIBLE_HISTORY');
  return {cutoff, history, polls};
}
export interface Mapping { id:string; weights:{row:number; weight:number}[] }
/** Both chamber predictions and truths are evaluated on each SAME footprint.
 * Historical boundaries are not silently equated to current boundaries. */
export function mappings(rows: Row[], geography: SourceGeography): Record<string,Mapping[]> {
  const cols = new Map(geography.colleges.map(c=>[c.id,c]));
  const out:Record<string,Mapping[]> = {national:[{id:'matched-domestic-domain',weights:rows.map((_,row)=>({row,weight:1}))}]};
  for (const ch of ['camera','senato'] as const) {
    for (const level of ['college','constituency']) {
      const areas = new Map<string,Mapping>();
      rows.forEach((r,row)=>{
        const parts = geography.cells.filter(c=>c.chamber===ch && c.municipality===r.id), total=sum(parts.map(c=>c.population));
        if (!total) throw new Error('MISSING_GEOGRAPHY:'+ch+'/'+r.id);
        for (const p of parts) {
          const id=level==='college'?p.collegeId:cols.get(p.collegeId)!.territoryId;
          const a=areas.get(id)??{id,weights:[]}; a.weights.push({row,weight:p.population/total}); areas.set(id,a);
        }
      });
      out[level+'_'+ch]=[...areas.values()].sort((a,b)=>a.id.localeCompare(b.id));
    }
  }
  return out;
}
