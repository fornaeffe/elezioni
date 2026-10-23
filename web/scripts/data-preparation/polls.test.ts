import { afterEach, describe, expect, it, vi } from 'vitest';
import { copyFile, mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { tmpdir } from 'node:os';
import { basename, join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { normalizeReviewedPoll, parsePollEntities, parseReviewedPolls, prepareSelectedPolls } from './polls';
import { writeVersion } from './sources';
import { reconcilePoll } from '../../src/lib/polls/reconciliation';
import type { PollReconciliationContext } from '../../src/lib/polls/types';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const archiveDirectory = resolve(root, 'data/sources/polls');
const prepared = await prepareSelectedPolls(root);
const { bundle } = prepared;
const sample = bundle.questions.find(q => q.id === 'pcm-14362-q1')!;
const directories: string[] = [];
async function temporary() {
  const directory = await mkdtemp(join(tmpdir(), 'elezioni-polls-test-'));
  directories.push(directory); return directory;
}
afterEach(async () => {
  vi.unstubAllGlobals();
  for (const directory of directories.splice(0)) {
    const inside = relative(resolve(tmpdir()), resolve(directory));
    if (!basename(directory).startsWith('elezioni-polls-test-') || inside.startsWith('..') || !inside) throw new Error('Unsafe poll test cleanup');
    await rm(directory, { recursive: true, force: true });
  }
});
const total = (r: ReturnType<typeof reconcilePoll>) => r.shares.reduce((n, s) => n + s.share, 0);
const ctx = (lists: PollReconciliationContext['lists'], extra: Partial<PollReconciliationContext> = {}): PollReconciliationContext => ({ lists, historicalLists: [], correspondences: [], ...extra });
function single(entityIds: string[], percentage = 100) {
  const question = structuredClone(sample);
  question.answers = [{ label: 'Reviewed answer', percentage, entityIds, kind: 'list' }];
  return normalizeReviewedPoll(question, sample.source);
}

describe('selected poll archive preparation', () => {
  it('validates the reviewed JSON, keeping eight questions and seven survey groups', () => {
    expect(prepared.audit).toMatchObject({ sourceOfTruth: 'reviewed-json', surveys: 7, questions: 8, independenceGroups: 7, conditionalQuestions: 3,
      sourceDateRange: { first: '2026-10-06', last: '2026-10-19' } });
    expect(prepared.audit.archiveBytes).toBeLessThan(60_000);
    expect(sample.source.reference).toBe('Sondaggio14362');
    expect(sample.source).not.toHaveProperty('sha256');
    expect(bundle.sourceVersions.map(v => v.split(':')[0])).toEqual(['manifest.json', 'archive.json', 'entities.json']);
    for (const q of bundle.questions) expect(q.support.reduce((n, s) => n + s.share, 0)).toBeCloseTo(1, 12);
    expect(sample.source.fieldworkStart).toBe('2026-10-14');
    expect(sample.source.fieldworkEnd).toBe('2026-10-15');
    expect(sample.source.sampleSize).toBe(800);
    expect(sample.questionSampleSize).toBeNull();
    expect(sample.answers.find(a => a.kind === 'nonresponse')?.percentage).toBe(39.7);
    expect(sample.support.some(a => a.label.includes('ASTENSIONE'))).toBe(false);
  });

  it('retains conditional alternatives and excludes coalition totals, unrelated subsamples and seat estimates', () => {
    const alternatives = bundle.questions.filter(q => q.sourceId === 'pcm-14363');
    expect(alternatives.map(q => q.ballot.label)).toEqual(['centrosinistra-schlein', 'centrosinistra-conte']);
    expect(new Set(alternatives.map(q => q.independenceGroup)).size).toBe(1);
    expect(alternatives[0].support.find(a => a.entityIds.includes('pd'))?.share).toBeCloseTo(.212, 12);
    expect(alternatives[1].support.find(a => a.entityIds.includes('pd'))?.share).toBeCloseTo(.205, 12);
    for (const q of alternatives) expect(q.support.some(a => a.label.startsWith('CENTROSINISTRA'))).toBe(false);
    expect(bundle.questions.filter(q => q.sourceId === 'pcm-14338')).toHaveLength(1);
    expect(bundle.questions.find(q => q.sourceId === 'pcm-14346')?.source.sampleSize).toBe(1200);
    expect(bundle.questions.find(q => q.sourceId === 'pcm-14358')?.pages).toEqual([3, 4]);
  });

  it('reproduces immutable outputs offline from just the selected Git archive', async () => {
    const directory = await temporary(), sources = join(directory, 'sources'); await mkdir(sources);
    const manifest = JSON.parse(await readFile(join(archiveDirectory, 'manifest.json'), 'utf8'));
    for (const name of ['manifest.json', 'archive.json', 'entities.json']) {
      await copyFile(join(archiveDirectory, name), join(sources, name));
    }
    vi.stubGlobal('fetch', () => { throw new Error('Network forbidden in poll preparation'); });
    const first = await prepareSelectedPolls(directory, sources), second = await prepareSelectedPolls(directory, sources);
    expect(first).toEqual(second); expect(first).toEqual(prepared);
    const path = join(directory, `${first.bundle.version}.json`);
    await writeVersion(path, first.bundle); const bytes = await readFile(path);
    await writeVersion(path, second.bundle); expect(await readFile(path)).toEqual(bytes);
    await writeFile(join(sources, 'manifest.json'), JSON.stringify({ ...manifest, maximumArchiveBytes: 100 }));
    await expect(prepareSelectedPolls(directory, sources)).rejects.toThrow('size budget');
    await copyFile(join(archiveDirectory, 'manifest.json'), join(sources, 'manifest.json'));
    // Optional downloads are not inputs, even when present and unreadable as PDFs.
    await writeFile(join(sources, 'Sondaggio14362.pdf'), 'corrupt');
    expect(await prepareSelectedPolls(directory, sources)).toEqual(first);
  });

  it('accepts valid JSON edits without repinning and versions their derived output', async () => {
    const directory = await temporary();
    for (const name of ['manifest.json', 'archive.json', 'entities.json']) await copyFile(join(archiveDirectory, name), join(directory, name));
    const input = JSON.parse(await readFile(join(directory, 'archive.json'), 'utf8'));
    input.questions[0].answers[0].percentage += .1;
    await writeFile(join(directory, 'archive.json'), JSON.stringify(input));
    const edited = await prepareSelectedPolls(root, directory);
    expect(edited.bundle.version).not.toBe(bundle.version);
    expect(edited.bundle.questions[0].answers[0].percentage).toBe(input.questions[0].answers[0].percentage);
    expect(edited.bundle.questions[0].support).not.toEqual(bundle.questions[0].support);
    const manifest = JSON.parse(await readFile(join(directory, 'manifest.json'), 'utf8'));
    manifest.sources[0].title += ' (reviewed)';
    await writeFile(join(directory, 'manifest.json'), JSON.stringify(manifest));
    const metadataEdit = await prepareSelectedPolls(root, directory);
    expect(metadataEdit.bundle.version).not.toBe(edited.bundle.version);
    const entities = JSON.parse(await readFile(join(directory, 'entities.json'), 'utf8'));
    entities.entities[0].aliases.push('Additional reviewed alias');
    await writeFile(join(directory, 'entities.json'), JSON.stringify(entities));
    expect((await prepareSelectedPolls(root, directory)).bundle.version).not.toBe(metadataEdit.bundle.version);
  });

  it('rejects missing JSON and malformed reviewed translations', async () => {
    const directory = await temporary();
    await copyFile(join(archiveDirectory, 'manifest.json'), join(directory, 'manifest.json'));
    await expect(prepareSelectedPolls(root, directory)).rejects.toThrow();
    for (const name of ['archive.json', 'entities.json']) await copyFile(join(archiveDirectory, name), join(directory, name));
    await writeFile(join(directory, 'archive.json'), '{}');
    await expect(prepareSelectedPolls(root, directory)).rejects.toThrow('Invalid poll object');
  });

  it('requires usable survey references and rejects duplicate, absent or inconsistent sources', async () => {
    const directory = await temporary();
    for (const name of ['manifest.json', 'archive.json', 'entities.json']) await copyFile(join(archiveDirectory, name), join(directory, name));
    const manifest = JSON.parse(await readFile(join(directory, 'manifest.json'), 'utf8'));
    const check = async (edit: (m: typeof manifest) => void, error: string) => {
      const bad = structuredClone(manifest); edit(bad);
      await writeFile(join(directory, 'manifest.json'), JSON.stringify(bad));
      await expect(prepareSelectedPolls(root, directory)).rejects.toThrow(error);
    };
    await check(m => { m.sources[0].reference = ''; }, 'nonempty');
    await check(m => { m.sources[1].reference = m.sources[0].reference; }, 'Duplicate poll source reference');
    await check(m => { m.sources[0].url = 'unusable'; }, 'metadata');
    await check(m => { m.sources[0].fieldworkEnd = '2026-10-24'; }, 'metadata');
    await check(m => { m.sources.splice(0, 1); }, 'absent source');
    await check(m => { m.sources.push({ ...m.sources[0], id: 'unused', reference: 'Unused survey' }); }, 'no reviewed question');
  });

  it('rejects malformed contracts, overlapping entities, ambiguous aliases and invalid dates', () => {
    const input = { schemaVersion: 1, version: 'test', reviewedOn: '2026-02-30', questions: [sample] };
    expect(() => parseReviewedPolls(input, bundle.entityCatalog)).toThrow('date');
    const catalog = structuredClone(bundle.entityCatalog); catalog.entities[0].aliases.push('Lega');
    expect(() => parsePollEntities(catalog)).toThrow('Ambiguous');
    const { source: _source, support: _support, normalization: _normalization, reportedTotal: _reportedTotal, ...question } = sample;
    expect(() => parseReviewedPolls({ ...input, reviewedOn: '2026-10-23', questions: [{ ...question, typo: true }] }, bundle.entityCatalog)).toThrow('object');
    const duplicate = structuredClone(question); duplicate.answers.push({ ...duplicate.answers[0], label: 'duplicate party' });
    expect(() => parseReviewedPolls({ ...input, reviewedOn: '2026-10-23', questions: [duplicate] }, bundle.entityCatalog)).toThrow('overlapping');
  });

  it('records rounding and estimates genuinely missing support without erasing the observed level', () => {
    const rounded = single(['pd'], 99.9);
    expect(rounded.support[0].share).toBe(1); expect(rounded.reportedTotal).toBe(99.9);
    expect(rounded.normalization.factor).toBeCloseTo(100 / 99.9);
    const incomplete = single(['pd'], 80);
    expect(incomplete.support.map(a => a.share)).toEqual([.8, .2]);
    expect(incomplete.normalization.method).toBe('missing-support-residual');
    expect(incomplete.notes.some(n => n.startsWith('POLL_MISSING_SUPPORT_ESTIMATE'))).toBe(true);
    const inconsistent = structuredClone(sample); inconsistent.answers = [{ ...sample.answers[0], percentage: 60 }, { ...sample.answers[1], percentage: 60 }];
    expect(() => normalizeReviewedPoll(inconsistent, sample.source)).toThrow('total');
  });
});

describe('poll reconciliation with computable estimates', () => {
  it('aggregates separately polled parties onto a joint scenario list', () => {
    const r = reconcilePoll(sample, bundle.entityCatalog, ctx([{ id: 'joint', name: 'AZIONE - ITALIA VIVA - CALENDA' }, { id: 'rest', name: 'Other option' }]));
    const joint = r.assignments.filter(a => ['AZIONE CON CALENDA SIAMO EUROPEI-RENEW EUROPE', 'CASA RIFORMISTA-ITALIA VIVA-RENZI'].includes(a.label));
    expect(joint.map(a => a.targets)).toEqual([[{ listId: 'joint', fraction: 1 }], [{ listId: 'joint', fraction: 1 }]]);
    expect(joint.reduce((n, a) => n + a.share, 0)).toBeCloseTo(.056);
    expect(total(r)).toBeCloseTo(1, 12);
  });

  it('reuses normalized historical splits and gives explicit poll mappings precedence', () => {
    const c = ctx([{ id: 'a', name: 'Future A' }, { id: 'b', name: 'Future B' }], {
      historicalLists: [{ id: 'past', name: 'Partito Democratico' }],
      correspondences: [{ pastListId: 'past', futureListId: 'a', factor: 2 }, { pastListId: 'past', futureListId: 'b', factor: 6 }] });
    const r = reconcilePoll(single(['pd']), bundle.entityCatalog, c);
    expect(r.shares.map(s => s.share)).toEqual([.25, .75]);
    expect(r.assignments[0]).toMatchObject({ method: 'historical-correspondence', estimated: true });
    const explicit = reconcilePoll(single(['pd']), bundle.entityCatalog, { ...c, mappings: [{ entityId: 'pd', listId: 'b', factor: 1 }] });
    expect(explicit.shares.map(s => s.share)).toEqual([0, 1]);
  });

  it('keeps arbitrary historical factor scales from changing the projection', () => {
    const c = ctx([{ id: 'a', name: 'Future A' }, { id: 'b', name: 'Future B' }], {
      historicalLists: [{ id: 'old1', name: 'PD' }, { id: 'old2', name: 'Partito Democratico' }],
      correspondences: [{ pastListId: 'old1', futureListId: 'a', factor: 100 }, { pastListId: 'old2', futureListId: 'b', factor: 1 }] });
    expect(reconcilePoll(single(['pd']), bundle.entityCatalog, c).shares.map(s => s.share)).toEqual([.5, .5]);
  });

  it('keeps an absent party computable through the reviewed proxy and records its nature', () => {
    const r = reconcilePoll(single(['futuro-nazionale']), bundle.entityCatalog, ctx([{ id: 'l', name: 'Lega' }]));
    expect(r.shares).toEqual([{ listId: 'l', share: 1 }]);
    expect(r.assignments[0]).toMatchObject({ method: 'proxy', estimated: true });
    expect(r.estimates.some(e => e.code === 'POLL_PARTY_PROXY')).toBe(true);
  });

  it('allocates unmapped support and residuals using positive baseline weights, then equal weights', () => {
    const c = ctx([{ id: 'a', name: 'Future A', baselineShare: .8 }, { id: 'b', name: 'Future B', baselineShare: .2 }]);
    const r = reconcilePoll(single(['progetto-civico']), bundle.entityCatalog, c);
    expect(r.shares.map(s => s.share)).toEqual([.8, .2]);
    expect(r.assignments[0].method).toBe('residual-baseline');
    expect(r.estimates.some(e => e.code === 'POLL_UNMAPPED_SUPPORT')).toBe(true);
    const equal = reconcilePoll(single(['progetto-civico']), bundle.entityCatalog, { ...c, lists: c.lists.map(l => ({ ...l, baselineShare: 0 })) });
    expect(equal.shares.map(s => s.share)).toEqual([.5, .5]);
    expect(equal.assignments[0].method).toBe('residual-equal');
  });

  it('preserves group support when some members are unmapped and labels equal splits', () => {
    const r = reconcilePoll(single(['pd', 'progetto-civico']), bundle.entityCatalog, ctx([{ id: 'pd', name: 'PD' }, { id: 'other', name: 'Other option' }]));
    expect(r.shares.map(s => s.share)).toEqual([.5, .5]);
    expect(r.estimates.map(e => e.code)).toContain('POLL_SPLIT_ESTIMATE');
    expect(total(r)).toBe(1);
  });

  it('records a new party geographic donor and uniform fallback without fabricating municipal observations', () => {
    const c = ctx([{ id: 'fn', name: 'Futuro Nazionale' }, { id: 'new', name: 'New unknown list' }], {
      historicalLists: [{ id: 'old-lega', name: 'LEGA PER SALVINI PREMIER' }] });
    const r = reconcilePoll(single(['futuro-nazionale']), bundle.entityCatalog, c);
    expect(r.territorialPriors).toEqual([{ listId: 'fn', method: 'entity-proxy', historicalListIds: ['old-lega'] },
      { listId: 'new', method: 'uniform-filed-support', historicalListIds: [] }]);
    expect(total(r)).toBe(1);
    expect(c.lists).toEqual([{ id: 'fn', name: 'Futuro Nazionale' }, { id: 'new', name: 'New unknown list' }]);
  });

  it('rejects malformed weights and abstention targets while retaining known zero support', () => {
    const c = ctx([{ id: 'pd', name: 'PD' }, { id: 'l', name: 'Lega' }]);
    expect(reconcilePoll(single(['pd']), bundle.entityCatalog, c).shares[1].share).toBe(0);
    expect(() => reconcilePoll(sample, bundle.entityCatalog, ctx([{ id: 'a', name: 'astensione' }]))).toThrow('eligible');
    expect(() => reconcilePoll(sample, bundle.entityCatalog, { ...c, mappings: [{ entityId: 'pd', listId: 'pd', factor: 0 }] })).toThrow('zero');
    expect(() => reconcilePoll(sample, bundle.entityCatalog, { ...c, lists: [{ id: 'pd', name: 'PD', baselineShare: NaN }] })).toThrow();
  });

});
