import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { sha256 } from './sources';
import { pollNameKey } from '../../src/lib/polls/reconciliation';
import type { NormalizedPollQuestion, PollAnswer, PollEntityCatalog, PollSource, ReviewedPollArchive, ReviewedPollQuestion } from '../../src/lib/polls/types';

// Small dependency-free JSON boundary, shared by every reviewed survey. Source
// The three reviewed JSON files are the local source of truth. Government
// references support manual audits; no captured document is a preparation input.
// Unknown fields are rejected to catch transcription typos.
type Row = Record<string, unknown>;
function row(v: unknown, keys: string): Row {
  if (!v || typeof v !== 'object' || Array.isArray(v) || Object.keys(v).some(k => !keys.split(' ').includes(k)) || keys.split(' ').some(k => !(k in v))) throw new Error(`Invalid poll object: expected ${keys}`);
  return v as Row;
}
function str(v: unknown): string { if (typeof v !== 'string' || !v.trim()) throw new Error('Expected nonempty poll text'); return v; }
function arr(v: unknown): unknown[] { if (!Array.isArray(v)) throw new Error('Expected poll array'); return v; }
function num(v: unknown, max = Infinity): number { if (typeof v !== 'number' || !Number.isFinite(v) || v < 0 || v > max) throw new Error('Invalid poll number'); return v; }
function positive(v: unknown): number { const n = num(v); if (!Number.isSafeInteger(n) || n < 1) throw new Error('Expected positive poll count'); return n; }
function date(v: unknown): string { const s = str(v); if (!/^\d{4}-\d{2}-\d{2}$/.test(s) || !Number.isFinite(Date.parse(s)) || new Date(s).toISOString().slice(0, 10) !== s) throw new Error('Invalid poll date'); return s; }
function literal<T extends string | number>(v: unknown, allowed: readonly T[]): T { if (!allowed.includes(v as T)) throw new Error(`Invalid poll value ${String(v)}`); return v as T; }
const strings = (v: unknown) => arr(v).map(str);
const unique = (values: readonly string[], name: string) => { if (new Set(values).size !== values.length) throw new Error(`Duplicate poll ${name}`); };

export function parsePollEntities(value: unknown): PollEntityCatalog {
  const root = row(value, 'schemaVersion version entities');
  const catalog: PollEntityCatalog = { schemaVersion: literal(root.schemaVersion, [1]), version: str(root.version), entities: arr(root.entities).map(value => {
    const e = row(value, 'id name aliases components proxyEntityIds proxyNote');
    return { id: str(e.id), name: str(e.name), aliases: strings(e.aliases), components: strings(e.components),
      proxyEntityIds: strings(e.proxyEntityIds), proxyNote: e.proxyNote === null ? null : str(e.proxyNote) };
  }) };
  unique(catalog.entities.map(e => e.id), 'entity');
  const ids = new Set(catalog.entities.map(e => e.id)), aliases = new Map<string, string>();
  for (const e of catalog.entities) {
    unique(e.components, 'component'); unique(e.proxyEntityIds, 'proxy');
    if ([...e.components, ...e.proxyEntityIds].some(id => id === e.id || !ids.has(id)) || e.proxyEntityIds.length && !e.proxyNote) throw new Error('Invalid poll entity links/proxy provenance');
    if (e.components.some(id => catalog.entities.find(c => c.id === id)!.components.length)) throw new Error('Nested poll entity groups are unsupported');
    for (const a of [e.name, ...e.aliases]) {
      const key = pollNameKey(a), previous = aliases.get(key);
      if (!key || previous && previous !== e.id) throw new Error(`Ambiguous poll alias: ${a}`);
      aliases.set(key, e.id);
    }
  }
  if (!catalog.entities.length) throw new Error('Empty poll entity catalog');
  return catalog;
}

function parseAnswer(value: unknown): PollAnswer {
  const a = row(value, 'label percentage kind entityIds');
  const answer: PollAnswer = { label: str(a.label), percentage: num(a.percentage, 100),
    kind: literal(a.kind, ['list', 'residual', 'coalition-total', 'nonresponse']), entityIds: strings(a.entityIds) };
  unique(answer.entityIds, 'answer entity');
  if (answer.kind === 'list' && !answer.entityIds.length || answer.kind !== 'list' && answer.entityIds.length) throw new Error('Poll answer kind conflicts with its entities');
  return answer;
}

export function parseReviewedPolls(value: unknown, entities: PollEntityCatalog): ReviewedPollArchive {
  const root = row(value, 'schemaVersion version reviewedOn questions');
  const ids = new Set(entities.entities.filter(e => !e.components.length).map(e => e.id));
  const archive: ReviewedPollArchive = { schemaVersion: literal(root.schemaVersion, [1]), version: str(root.version), reviewedOn: date(root.reviewedOn),
    questions: arr(root.questions).map(value => {
      const q = row(value, 'id sourceId questionNumber pages independenceGroup text ballot denominator questionSampleSize answers notes');
      const b = row(q.ballot, 'kind label assumptions'), d = row(q.denominator, 'kind basis note');
      const result: ReviewedPollQuestion = { id: str(q.id), sourceId: str(q.sourceId), questionNumber: positive(q.questionNumber), pages: arr(q.pages).map(positive),
        independenceGroup: str(q.independenceGroup), text: str(q.text),
        ballot: { kind: literal(b.kind, ['generic', 'conditional']), label: b.label === null ? null : str(b.label), assumptions: strings(b.assumptions) },
        denominator: { kind: literal(d.kind, ['declared-vote-intentions']), basis: literal(d.basis, ['reported', 'estimated']), note: str(d.note) },
        questionSampleSize: q.questionSampleSize === null ? null : positive(q.questionSampleSize), answers: arr(q.answers).map(parseAnswer), notes: strings(q.notes) };
      if (!result.pages.length || result.independenceGroup !== result.sourceId || (result.ballot.kind === 'conditional') !== (result.ballot.label !== null)) throw new Error('Invalid poll question provenance/ballot');
      unique(result.pages.map(String), 'source page'); unique(result.answers.map(a => a.label), 'answer label');
      const used = result.answers.flatMap(a => a.entityIds);
      unique(used, 'overlapping answer entity');
      if (used.some(id => !ids.has(id))) throw new Error('Poll answer names an unknown or composite entity');
      return result;
    }) };
  unique(archive.questions.map(q => q.id), 'question');
  unique(archive.questions.map(q => `${q.sourceId}:${q.questionNumber}`), 'source question');
  if (!archive.questions.length) throw new Error('Empty reviewed poll archive');
  return archive;
}

function parseSource(value: unknown): PollSource {
  const s = row(value, 'id reference url retrieval capturedOn title pollster commissioner purchaser fieldworkStart fieldworkEnd publicationDate population territory sampleSize sampleDescription samplingDescription collectionModes marginOfError confidenceLevel');
  const source: PollSource = { id: str(s.id), reference: str(s.reference), url: str(s.url), retrieval: str(s.retrieval),
    capturedOn: date(s.capturedOn), title: str(s.title), pollster: str(s.pollster), commissioner: str(s.commissioner), purchaser: str(s.purchaser),
    fieldworkStart: date(s.fieldworkStart), fieldworkEnd: date(s.fieldworkEnd), publicationDate: date(s.publicationDate), population: str(s.population), territory: str(s.territory),
    sampleSize: positive(s.sampleSize), sampleDescription: str(s.sampleDescription), samplingDescription: str(s.samplingDescription), collectionModes: strings(s.collectionModes),
    marginOfError: s.marginOfError === null ? null : num(s.marginOfError, 1), confidenceLevel: s.confidenceLevel === null ? null : num(s.confidenceLevel, 1) };
  if (!source.url.startsWith('https://') || !source.collectionModes.length ||
    source.fieldworkStart > source.fieldworkEnd || source.fieldworkEnd > source.publicationDate || source.publicationDate > source.capturedOn) throw new Error('Invalid poll source metadata/dates');
  return source;
}

export function normalizeReviewedPoll(question: ReviewedPollQuestion, source: PollSource): NormalizedPollQuestion {
  const support = question.answers.filter(a => a.kind === 'list' || a.kind === 'residual');
  const reportedTotal = support.reduce((n, a) => n + a.percentage, 0);
  if (question.sourceId !== source.id || !support.length || reportedTotal <= 0 || reportedTotal > 101 ||
    question.questionSampleSize !== null && question.questionSampleSize > source.sampleSize) throw new Error('Invalid poll support total/source/sample base');
  const notes = [...question.notes];
  // Small rounding differences are normalized with an exact recorded factor.
  // Material missing mass is an estimated residual, not an absent/zero party.
  const missing = reportedTotal < 99;
  if (missing) notes.push(`POLL_MISSING_SUPPORT_ESTIMATE: ${100 - reportedTotal} percentage points retained as unreported residual support.`);
  const total = missing ? 100 : reportedTotal;
  const rows: NormalizedPollQuestion['support'] = support.map(a => ({ label: a.label, entityIds: [...a.entityIds], share: a.percentage / total, kind: a.kind as 'list' | 'residual' }));
  if (missing) rows.push({ label: '[unreported support estimate]', entityIds: [], share: (100 - reportedTotal) / 100, kind: 'residual' });
  return { ...structuredClone(question), notes, source: structuredClone(source), reportedTotal, support: rows,
    normalization: { method: missing ? 'missing-support-residual' : 'reported-total', factor: 100 / total } };
}

export async function prepareSelectedPolls(root: string, directory = resolve(root, 'data/sources/polls')) {
  const files = await Promise.all(['manifest.json', 'archive.json', 'entities.json'].map(async path =>
    ({ path, bytes: await readFile(resolve(directory, path)) })));
  const json = (path: string): unknown => JSON.parse(files.find(f => f.path === path)!.bytes.toString('utf8'));
  const m = row(json('manifest.json'), 'schemaVersion id reviewedOn maximumArchiveBytes sources');
  literal(m.schemaVersion, [2]);
  const id = str(m.id), reviewedOn = date(m.reviewedOn), maximumArchiveBytes = positive(m.maximumArchiveBytes);
  const sources = arr(m.sources).map(parseSource);
  unique(sources.map(s => s.id), 'source'); unique(sources.map(s => s.reference), 'source reference');
  if (!sources.length) throw new Error('Incomplete selected poll manifest');
  const archiveBytes = files.reduce((n, f) => n + f.bytes.length, 0);
  if (archiveBytes > maximumArchiveBytes) throw new Error('Selected poll archive exceeds its reviewed Git size budget');
  const entities = parsePollEntities(json('entities.json'));
  const archive = parseReviewedPolls(json('archive.json'), entities);
  if (archive.version !== id || archive.reviewedOn !== reviewedOn || sources.some(s => s.capturedOn > reviewedOn)) throw new Error('Poll review identities/dates differ');
  const questions = archive.questions.map(q => {
    const source = sources.find(s => s.id === q.sourceId);
    if (!source || source.publicationDate > archive.reviewedOn) throw new Error('Poll question has an absent source or future publication');
    return normalizeReviewedPoll(q, source);
  });
  if (sources.some(s => !questions.some(q => q.sourceId === s.id))) throw new Error('Selected survey has no reviewed question');
  // Content identities version derived output automatically. They are not
  // source pins and do not require manually updating a hash after a JSON edit.
  const sourceVersions = files.map(f => `${f.path}:${sha256(f.bytes)}`);
  const body = { schemaVersion: 2 as const, kind: 'poll-observations' as const, methodVersion: 'poll-preparation-v2', sourceArchive: id, reviewedOn,
    sourceVersions, entityCatalog: entities, questions };
  const bundle = { ...body, version: `poll-observations-v2-${sha256(JSON.stringify(body)).slice(0, 12)}` };
  const audit = { schemaVersion: 2, version: bundle.version, sourceOfTruth: 'reviewed-json', surveys: sources.length, questions: questions.length,
    independenceGroups: new Set(questions.map(q => q.independenceGroup)).size, conditionalQuestions: questions.filter(q => q.ballot.kind === 'conditional').length,
    archiveBytes, maximumArchiveBytes, offline: true,
    sourceDateRange: { first: sources.map(s => s.fieldworkStart).sort()[0], last: sources.map(s => s.fieldworkEnd).sort().at(-1) },
    missingQuestionSampleSizes: questions.filter(q => q.questionSampleSize === null).map(q => q.id),
    limitations: ['Selected ingestion archive, not a representative poll census or evidence of historical bias calibration.',
      'Conditional questions retain their ballot labels and shared survey identity; no averaging or forecast is performed.',
      'Full survey sample size is not asserted as the effective size of declared vote intentions.',
      'Reviewed JSON is authoritative; survey references support manual government-site audits without PDF checksums or local documents.'] };
  return { bundle, audit };
}
