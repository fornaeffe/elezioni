import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { archiveMember, delimited } from './formats';
import { reconstructHistoricalAttribution, sourceInteger } from './reform-history';
import { acquireSource, readSource, sha256, type Source } from './sources';

export const PANEL = ['pd', 'm5s', 'lega', 'other'] as const;
export interface CalibrationElection {
  id: string; kind: 'camera' | 'european'; date: string; cutoff: string;
  resultSourceId: string; baselineSourceId: string; questionIds: string[];
}
interface ElectionSource extends Source {
  member: string; method: string; additionalMembers?: string[];
  panel: Record<'pd' | 'm5s' | 'lega', string[]>;
}
export interface ElectionManifest { schemaVersion: 1; id: string; reviewedOn: string; sources: ElectionSource[]; elections: CalibrationElection[]; }
export async function electionManifest(root: string): Promise<ElectionManifest> {
  const m: ElectionManifest = JSON.parse(await readFile(resolve(root, 'data/sources/polls/history/elections-manifest.json'), 'utf8'));
  if (m.schemaVersion !== 1 || new Set(m.sources.map(s => s.id)).size !== m.sources.length ||
      new Set(m.elections.map(e => e.id)).size !== m.elections.length) throw new Error('Invalid poll election manifest');
  for (const e of m.elections) {
    for (const date of [e.date, e.cutoff]) if (!/^\d{4}-\d{2}-\d{2}$/.test(date) || !Number.isFinite(Date.parse(date)) || new Date(date).toISOString().slice(0, 10) !== date) throw new Error('Invalid calibration date');
    if (!['camera', 'european'].includes(e.kind) || e.cutoff >= e.date || !e.questionIds.length ||
      new Set(e.questionIds).size !== e.questionIds.length || ![e.resultSourceId, e.baselineSourceId].every(id => m.sources.some(s => s.id === id))) throw new Error('Invalid calibration election');
    const prefix = e.kind === 'camera' ? 'camera' : 'europee';
    const pattern = new RegExp(`^${prefix}-\\d{8}$`);
    if (![e.baselineSourceId, e.resultSourceId].every(id => pattern.test(id)) ||
      e.baselineSourceId >= e.resultSourceId || e.cutoff.replaceAll('-', '') >= e.resultSourceId.slice(-8)) throw new Error('Calibration baseline/kind must precede and match target');
  }
  return m;
}
export async function downloadCalibrationSources(root: string) {
  const m = await electionManifest(root);
  for (const s of m.sources) await acquireSource(resolve(root, 'data/sources'), s);
}
/** Reproducible source transformation. Never aggregate preference rows (which
 * repeat the same list votes for every candidate), or mix VdA/Estero ballots. */
export async function generateCalibrationResults(root: string) {
  const manifest = await electionManifest(root), results = [];
  for (const s of manifest.sources) {
    const bytes = await readSource(resolve(root, 'data/sources'), s);
    const members = [s.member, ...(s.additionalMembers ?? [])].map(name => ({ name, bytes: archiveMember(bytes, name) }));
    const rows = members.flatMap(m => delimited(m.bytes, ';'));
    const counts = new Map<string, number>();
    const add = (name: string, count: number) => {
      if (!name) throw new Error('Unnamed calibration result list');
      counts.set(name, (counts.get(name) ?? 0) + count);
    };
    let candidateOnly = 0;
    if (s.method === 'old-college-candidate-only-attribution') {
      const r = reconstructHistoricalAttribution(rows, 'camera');
      for (const v of r.output) add(v.row.DESCRLISTA, v.votes);
      candidateOnly = r.traces.reduce((n, r) => n + r.candidateOnly, 0);
    } else if (s.method === 'national-attributed-list-totals') {
      if (new Set(rows.map(r => r.LISTA)).size !== rows.length) throw new Error('Duplicate national list total');
      for (const r of rows) {
        if (!/^\d+,00$/.test(r.VOTILISTA)) throw new Error('Invalid national decimal vote count');
        add(r.LISTA, sourceInteger(r.VOTILISTA.split(',')[0]));
      }
    } else if (s.method === 'municipal-list-sum') {
      const keys = rows.map(r => JSON.stringify([r.CIRCOSCRIZIONE ?? r.DESCCIRCEUROPEA, r.REGIONE ?? r.DESCREGIONE, r.PROVINCIA ?? r.DESCPROVINCIA, r.COMUNE ?? r.DESCCOMUNE, r.LISTA ?? r.DESCLISTA]));
      if (new Set(keys).size !== rows.length) throw new Error('Duplicate municipal list result');
      for (const r of rows) add(r.LISTA ?? r.DESCLISTA, sourceInteger(r.VOTI_LISTA ?? r.NUMVOTI));
    } else throw new Error('Unsupported poll calibration result method');
    const total = [...counts.values()].reduce((a, b) => a + b, 0);
    if (!total || !Number.isSafeInteger(total)) throw new Error('Invalid poll calibration result total');
    const named = (['pd', 'm5s', 'lega'] as const).map(id => {
      if (!Array.isArray(s.panel[id]) || s.panel[id].some(name => !counts.has(name))) throw new Error(`Missing declared result mapping ${s.id}:${id}`);
      return { id, votes: s.panel[id].reduce((n, name) => n + counts.get(name)!, 0) };
    });
    if (new Set(Object.values(s.panel).flat()).size !== Object.values(s.panel).flat().length) throw new Error('Overlapping election panel mapping');
    results.push({ sourceId: s.id, sourceSha256: s.sha256, method: s.method, total, candidateOnly,
      members: members.map(m => ({ name: m.name, sha256: sha256(m.bytes), bytes: m.bytes.length })),
      lists: [...counts].sort(([a], [b]) => a < b ? -1 : 1).map(([name, votes]) => ({ name, votes })),
      panel: [...named, { id: 'other' as const, votes: total - named.reduce((n, p) => n + p.votes, 0) }].map(p => ({ ...p, share: p.votes / total })) });
  }
  return { schemaVersion: 1, sourceManifest: manifest.id, results };
}
export type CalibrationResults = Awaited<ReturnType<typeof generateCalibrationResults>>;

/** The small frozen result table makes calibration offline in a fresh clone;
 * `verify-results` replays it from the separately pinned government archives. */
export async function readCalibrationResults(root: string) {
  const directory = resolve(root, 'data/sources/polls/history'), manifest = await electionManifest(root);
  const pin = JSON.parse(await readFile(resolve(directory, 'results-pin.json'), 'utf8')) as Source & { manifestSha256: string };
  if (pin.path !== 'results.json' || pin.manifestSha256 !== sha256(await readFile(resolve(directory, 'elections-manifest.json')))) throw new Error('Calibration results manifest pin mismatch');
  const bytes = await readSource(directory, pin);
  const result: CalibrationResults = JSON.parse(bytes.toString('utf8'));
  if (result.schemaVersion !== 1 || result.sourceManifest !== manifest.id || result.results.length !== manifest.sources.length) throw new Error('Invalid calibration results');
  for (const s of manifest.sources) {
    const r = result.results.filter(r => r.sourceId === s.id);
    if (r.length !== 1 || r[0].sourceSha256 !== s.sha256 || r[0].method !== s.method || r[0].total <= 0 ||
      r[0].lists.some(l => !Number.isSafeInteger(l.votes) || l.votes < 0) ||
      r[0].lists.reduce((n, l) => n + l.votes, 0) !== r[0].total ||
      r[0].panel.map(p => p.id).join(',') !== PANEL.join(',') ||
      r[0].panel.some(p => !Number.isSafeInteger(p.votes) || p.votes < 0 || p.share !== p.votes / r[0].total) ||
      r[0].panel.reduce((n, p) => n + p.votes, 0) !== r[0].total) throw new Error('Invalid calibration source totals');
  }
  const archiveBytes = bytes.length + (await readFile(resolve(directory, 'results-pin.json'))).length +
    (await readFile(resolve(directory, 'elections-manifest.json'))).length;
  return { manifest, results: result.results, archiveBytes, sourceVersions: [`elections-manifest:${pin.manifestSha256}`, `results:${sha256(bytes)}`] };
}
