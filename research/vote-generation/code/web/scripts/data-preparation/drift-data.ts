import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { loadGenerationHistory, type GenerationEvent } from './generation-history';
import { prepareSelectedPolls } from './polls';
import { POLL_POLICY } from '../../src/lib/polls/forecast';
import { sha256 } from './sources';
import { dateDay } from './drift-math';

export const DRIFT_OUTCOMES = ['non-valid', 'pd', 'm5s', 'lega', 'other'] as const;
export const DRIFT_SIX_OUTCOMES = ['non-valid', 'pd', 'm5s', 'lega', 'fdi', 'fi-pdl', 'other'] as const;
export const DRIFT_DATA_VERSION = 'elector-drift-input-v1';
export interface DriftMunicipality { id: string; regionId: string; electors: number; counts: number[]; active: number[]; }
export interface DriftElection {
  id: string; kind: 'camera' | 'european'; date: string;
  municipalities: DriftMunicipality[];
  national: { electors: number; counts: number[] };
}
export interface DriftPoll {
  id: string; surveyId: string; house: string; date: string; publication: string; fieldworkEnd: string;
  kind: 'camera' | 'european'; group: string; sample: number;
  partitions: number[][]; shares: number[]; source: string; conditional: string | null;
}
export interface DriftInputs {
  version: string; sourceVersions: string[]; elections: DriftElection[]; polls: DriftPoll[];
  outcomes?: readonly string[];
  audit: { sourceHistory: string; elections: unknown[]; surveys: number; questions: number; excluded: { id: string; reason: string }[] };
}
const midpoint = (a: string, b: string) => new Date((Date.parse(a) + Date.parse(b)) / 2).toISOString();
const groupId = (date: string, kind: string) => date.slice(0, 4) + '-H' + (Number(date.slice(5, 7)) <= 6 ? '1' : '2') + '-' + kind;
/** Coarsen overlapping answer groups until they form a genuine partition of
 * the fixed research outcomes; no guessed allocation inside a grouped answer. */
export function collapsePollRows(rows: { entities: string[]; share: number }[], political: readonly string[] = DRIFT_OUTCOMES.slice(1)) {
  const parent = political.map((_, i) => i);
  const find = (x: number): number => parent[x] === x ? x : (parent[x] = find(parent[x]));
  const ids = (entities: string[]) => [...new Set(entities.map(id => {
    const exact = political.indexOf(id === 'fi' || id === 'pdl' ? 'fi-pdl' : id);
    return exact >= 0 ? exact : political.length - 1;
  }))];
  const observed = new Set<number>(rows.flatMap(r => ids(r.entities)));
  const mapped = rows.map(r => ({ ...r, group: r.entities.length ? ids(r.entities) : parent.filter(i => i === political.length - 1 || !observed.has(i)) }));
  for (const r of mapped) for (const x of r.group.slice(1)) parent[find(x)] = find(r.group[0]);
  const groups = [...new Set(parent.map((_, i) => find(i)))].sort((a, b) => a - b);
  const partitions = groups.map(g => parent.map((_, i) => i).filter(i => find(i) === g).map(i => i + 1));
  const shares = groups.map(g => mapped.reduce((s, r) => s + (find(r.group[0]) === g ? r.share : 0), 0));
  const total = shares.reduce((a, b) => a + b, 0);
  return { partitions, shares: shares.map(p => p / total) };
}
export async function loadDriftInputs(root: string, panel: 'core' | 'six' = 'core'): Promise<DriftInputs> {
  const history = await loadGenerationHistory(root);
  const outcomes: readonly string[] = panel === 'six' ? DRIFT_SIX_OUTCOMES : DRIFT_OUTCOMES;
  const directories = ['data/sources/polls/history', 'data/sources/polls'];
  const polls: DriftPoll[] = [], excluded: DriftInputs['audit']['excluded'] = [], sourceVersions = [...history.sourceVersions];
  let questions = 0;
  for (const directory of directories) {
    const archive = await prepareSelectedPolls(root, resolve(root, directory));
    for (const name of ['manifest.json', 'archive.json', 'entities.json']) sourceVersions.push(directory + '/' + name + ':' + sha256(await readFile(resolve(root, directory, name))));
    for (const q of archive.bundle.questions) {
      questions++;
      if (q.ballot.kind === 'conditional') { excluded.push({ id: q.id, reason: 'Conditional alternative is not the unspecified baseline ballot; same-survey alternatives are not independent observations.' }); continue; }
      const kind = /european|europe[ae]|europee/i.test(q.text + ' ' + q.source.title) ? 'european' as const : 'camera' as const;
      const collapsed = collapsePollRows(q.support.map(r => ({ entities: r.entityIds, share: r.share })), outcomes.slice(1));
      if (collapsed.partitions.length < 2 || collapsed.shares.some(p => p <= 0)) { excluded.push({ id: q.id, reason: 'No positive identifiable contrast after exact group reconciliation.' }); continue; }
      const nonresponse = q.answers.find(a => a.kind === 'nonresponse')?.percentage;
      const decided = nonresponse === undefined ? POLL_POLICY.unknownDecidedFraction : 1 - nonresponse / 100;
      const sample = Math.min(POLL_POLICY.maximumEffectiveSample, (q.questionSampleSize ?? q.source.sampleSize * decided) / POLL_POLICY.designEffect);
      polls.push({ id: q.id, surveyId: q.independenceGroup, house: q.source.pollster,
        date: midpoint(q.source.fieldworkStart, q.source.fieldworkEnd), publication: q.source.publicationDate,
        fieldworkEnd: q.source.fieldworkEnd, kind, group: groupId(q.source.fieldworkEnd, kind), sample,
        ...collapsed, source: q.source.url, conditional: null });
    }
  }
  const earlyPath = 'data/sources/polls/history/generation-horizon-observations.json';
  const bytes = await readFile(resolve(root, earlyPath));
  sourceVersions.push(earlyPath + ':' + sha256(bytes));
  const early = JSON.parse(bytes.toString('utf8'));
  const pinBytes = await readFile(resolve(root, 'data/sources/polls/history/generation-horizon-manifest.json'));
  if (JSON.parse(pinBytes.toString('utf8')).observationsSha256 !== sha256(bytes)) throw new Error('Early survey pin mismatch');
  sourceVersions.push('early-source-manifest:' + sha256(pinBytes));
  let extra: { earlyObservations: { id: string; shares: number[] }[] } | undefined;
  if (panel === 'six') {
    const extraBytes = await readFile(resolve(root, 'data/sources/polls/history/generation-six-category-observations.json'));
    sourceVersions.push('six-category-refinements:' + sha256(extraBytes)); extra = JSON.parse(extraBytes.toString('utf8'));
  }
  for (const p of early.observations) {
    questions++;
    const richer = extra?.earlyObservations.find(x => x.id === p.id);
    if (richer && (richer.shares.length !== 6 || richer.shares.slice(0, 3).some((v, j) => Math.abs(v - p.shares[j]) > 1e-8) ||
      Math.abs(richer.shares.reduce((a, b) => a + b, 0) - 1) > 1e-8)) throw new Error('Six-party refinement disagrees with reviewed common panel');
    const partitions = panel === 'six' ? (richer ? [[1], [2], [3], [4], [5], [6]] : [[1], [2], [3], [4, 5, 6]]) : [[1], [2], [3], [4]];
    polls.push({ id: p.id, surveyId: p.id, house: p.pollster,
      date: midpoint(p.fieldworkStart, p.fieldworkEnd), publication: p.publicationDate, fieldworkEnd: p.fieldworkEnd,
      kind: 'camera', group: groupId(p.fieldworkEnd, 'camera'),
      sample: Math.min(POLL_POLICY.maximumEffectiveSample, p.sampleSize * POLL_POLICY.unknownDecidedFraction / POLL_POLICY.designEffect),
      partitions, shares: richer?.shares ?? p.shares, source: p.sourceUrl, conditional: null });
  }
  if (new Set(polls.map(p => p.surveyId)).size !== polls.length) throw new Error('Repeated survey in drift observations');
  const elections = history.events.map(e => election(e, outcomes));
  const body = { sourceVersions, elections, polls, outcomes };
  return { ...body, version: DRIFT_DATA_VERSION + '-' + sha256(JSON.stringify(body)).slice(0, 12),
    audit: { sourceHistory: history.version, elections: history.events.map(e => ({ id: e.id, audit: e.audit })), surveys: polls.length, questions, excluded } };
}
function election(e: GenerationEvent, outcomes: readonly string[]): DriftElection {
  const domain = e.municipalities.filter(m => m.regionId !== '02');
  const map = (name: string) => {
    const core = e.mapping.find(x => x.name === name)?.party ?? 'other';
    if (core !== 'other' || outcomes.length === 5) return outcomes.indexOf(core);
    const normalized = name.normalize('NFKD').replace(/[\u0300-\u036f]/g, '').toUpperCase().replace(/[^A-Z0-9]/g, '');
    if (normalized.startsWith('FRATELLIDITALIA')) return outcomes.indexOf('fdi');
    if (normalized.startsWith('FORZAITALIA') || normalized.startsWith('ILPOPOLODELLALIBERTA')) return outcomes.indexOf('fi-pdl');
    return outcomes.length - 1;
  };
  const all = domain.map(m => {
    const counts = Array(outcomes.length).fill(0); counts[0] = m.electors - m.validVotes;
    for (const l of m.lists) counts[map(l.name)] += l.votes;
    return { m, counts };
  });
  const nationalCounts = Array.from({ length: outcomes.length }, (_, i) => all.reduce((s, r) => s + r.counts[i], 0));
  const municipalities = all.filter(({ m }) => m.matchable && m.electors > 0 && m.validVotes <= m.electors).map(({ m, counts }) => {
    const active = outcomes.map((_, i) => i).filter(j => j === 0 || nationalCounts[j] > 0 && m.lists.some(l => map(l.name) === j));
    return { id: m.id, regionId: m.regionId, electors: m.electors, counts, active };
  });
  return { id: e.id, kind: e.kind, date: e.id === 'european-2024' ? '2024-06-08' : e.date, municipalities,
    national: { electors: domain.reduce((s, m) => s + m.electors, 0),
      counts: nationalCounts } };
}
export function availableInputs(input: DriftInputs, cutoff: string) {
  return { elections: input.elections.filter(e => e.date < cutoff),
    polls: input.polls.filter(p => p.publication <= cutoff && p.fieldworkEnd <= cutoff && dateDay(p.date) <= dateDay(cutoff)) };
}
