import { run, archive } from './files.fixture';
import { describe, expect, it, vi } from 'vitest';
import { gzipSync } from 'fflate';
import { fixture } from '../scenario/scenario.fixture';
import { selectGenerator, selectLaw } from '../scenario/edits';
import { ROSATELLUM, type PoliticsScenario } from '../scenario/schema';
import type { PoliticsArchiveEnvelope } from '../scenario/envelopes';
import { encodeScenario } from '../scenario/serialization';
import { encodeState } from '../reform-2026/vote-share/state-codec';
import { fittedStateShapes, profileStateShapes } from '../reform-2026/vote-share/state-shapes';
import { fastGenerationFixture } from '../../../../tests/fixtures/fast-generation-input';
import { createPoliticsCheckpoint, validatePoliticsCheckpoint } from './checkpoint';
import { inspectPoliticsFile, readPoliticsScenarioFile } from './metadata';
import { politicsDraftKey, readPoliticsDraft, writePoliticsDraft } from './draft';
import { importPoliticsArchiveFile, parsePoliticsArchive, parsePoliticsResult, readPoliticsResultFile,
  serializePoliticsResult, streamPoliticsArchive, type PoliticsArchiveSink } from '../../core/result-export';
import { fileWorkspace } from '../../workspaces/import-routing';

async function text(value: PoliticsArchiveEnvelope) {
  let result = ''; for await (const chunk of streamPoliticsArchive(value)) result += chunk; return result;
}
function file(json: string, gzip = false) {
  return new File([gzip ? gzipSync(new TextEncoder().encode(json)) as BlobPart : json], gzip ? 'test.json.gz' : 'test.json');
}
function sink() {
  const staged: unknown[] = [], published: unknown[] = [{ existing: true }];
  return { staged, published,
    table: vi.fn(async (index, descriptor) => { staged.push({ index, descriptor }); }),
    row: vi.fn(async (index, cells) => { staged.push({ index, cells }); }),
    warning: vi.fn(async warning => { staged.push(warning); }),
    commit: vi.fn(async header => { published.push({ header, rows: [...staged] }); }),
    abort: vi.fn(async () => { staged.length = 0; }),
  } satisfies PoliticsArchiveSink & { staged: unknown[]; published: unknown[] };
}

describe('CP-05 common politics files', () => {
  it('roundtrips all supported results and inactive settings through strict JSON/gzip readers', async () => {
    const s = fixture().scenario;
    for (const scenario of [s, selectGenerator(s, 'M01', '2.0.0'), selectLaw(s, ROSATELLUM)]) for (const gzip of [false, true]) {
      const result = run(scenario);
      expect(await readPoliticsResultFile(file(serializePoliticsResult(result), gzip))).toEqual(result);
      const bytes = encodeScenario(scenario, gzip);
      expect(await readPoliticsScenarioFile(new File([bytes], gzip ? 'scenario.json.gz' : 'scenario.json'))).toEqual(scenario);
    }
  });
  it('streams archives and commits only after all rows, evidence and identity checks succeed', async () => {
    const original = archive();
    for (const gzip of [false, true]) {
      const destination = sink();
      const header = await importPoliticsArchiveFile(file(await text(original), gzip), destination);
      expect(header.run.executedScenario).toEqual(original.run.executedScenario);
      expect(header.run.session).toEqual(original.run.session);
      expect(header.run.result.tables).toEqual(original.run.result.tables.map(t => ({ ...t, rows: [] })));
      expect(destination.row.mock.calls.map(([, cells]) => cells)).toEqual(original.run.result.tables[0].rows);
      expect(destination.warning).toHaveBeenCalledWith(original.run.result.warnings[0]);
      expect(destination.commit).toHaveBeenCalledOnce();
      expect(destination.abort).not.toHaveBeenCalled();
    }
  });
  it('supports a bounded external row/diagnostic source', async () => {
    const a = archive();
    const header = { ...a, run: { ...a.run, result: { ...a.run.result, tables: a.run.result.tables.map(t => ({ ...t, rows: [] })), warnings: [] } } };
    let output = '';
    for await (const chunk of streamPoliticsArchive(header, {
      async *rows() { yield* a.run.result.tables[0].rows; }, async *warnings() { yield* a.run.result.warnings; },
    })) output += chunk;
    expect(JSON.parse(output)).toEqual(a);
  });
  it('rejects malformed, unsupported and late-corrupt imports without publishing staged rows', async () => {
    const good = await text(archive());
    const variants = [good.slice(0, -1), good + ' garbage', good.replace('"schemaVersion":1', '"schemaVersion":999'),
      good.replace('"portableFit":null', '"portableFit":null,"surprise":1'),
      good.replace('"portableFit":null', '"portableFit":null,"kind":"politics-result-archive"'),
      good.replace('"simulationId":2', '"simulationId":3'),
      good.replace('[2,"",false,null]', '[2,"",false]'), good.replace('"assemblyKey":"assembly"', '"assemblyKey":"assembly","extra":0')];
    for (const json of variants) {
      const destination = sink();
      await expect(importPoliticsArchiveFile(file(json), destination)).rejects.toThrow();
      expect(destination.published).toEqual([{ existing: true }]);
      expect(destination.staged).toEqual([]);
      expect(destination.commit).not.toHaveBeenCalled();
      expect(destination.abort).toHaveBeenCalledOnce();
    }
    const invalidUtf8 = new Uint8Array([...new TextEncoder().encode(good.slice(0, -1)), 0xff, 0x7d]);
    const destination = sink();
    await expect(importPoliticsArchiveFile(new File([invalidUtf8], 'invalid.json'), destination)).rejects.toThrow();
    expect(destination.commit).not.toHaveBeenCalled();
    expect(destination.abort).toHaveBeenCalledOnce();
  });
  it('checks provenance, execution versions, result coverage and session membership', () => {
    const valid = JSON.parse(serializePoliticsResult(run()));
    const edits = [(r: typeof valid) => r.execution.generator.version = '9.0.0',
      (r: typeof valid) => r.execution.scrutiny.id = ROSATELLUM,
      (r: typeof valid) => r.execution.data[0].sha256 = 'c'.repeat(64),
      (r: typeof valid) => r.execution.simulationSeed = 'changed',
      (r: typeof valid) => r.result.benchmark.simulations = 9,
      (r: typeof valid) => r.session.simulations.push(r.session.simulations[0]),
      (r: typeof valid) => r.session.state = 'complete',
      (r: typeof valid) => r.result.completedSimulations = 0,
      (r: typeof valid) => r.result.tables[0].columns.push('id')];
    for (const edit of edits) { const bad = structuredClone(valid); edit(bad); expect(() => parsePoliticsResult(bad)).toThrow(); }
    const zero = { ...run(), session: { ...run().session, simulations: [] }, result: { ...run().result, completedSimulations: 0, warnings: [] } };
    expect(parsePoliticsResult(zero).result.completedSimulations).toBe(0);
  });
  it('inspects metadata before any large or malformed result body and rejects old politics headers', async () => {
    const a = archive(), s = a.run.executedScenario;
    const prefix = `{"kind":"politics-result-archive","schemaVersion":1,"run":{"kind":"politics-result","schemaVersion":1,"executedScenario":{"kind":"politics-scenario","schemaVersion":1,"selection":${JSON.stringify(s.selection)},"rest":INVALID`;
    expect((await inspectPoliticsFile(file(prefix))).selection).toEqual(s.selection);
    expect((await fileWorkspace(file(prefix))).algorithm).toBe(s.selection.scrutiny.id);
    // Inspection is routing evidence only; full imports still fail and abort.
    await expect(importPoliticsArchiveFile(file(prefix), sink())).rejects.toThrow();
    for (const json of [prefix.replace('"schemaVersion":1', '"schemaVersion":2'),
      prefix.replace('"kind":"politics-result","schemaVersion":1', '"kind":"politics-result","schemaVersion":2'),
      prefix.replace('"kind":"politics-scenario","schemaVersion":1', '"kind":"politics-scenario","schemaVersion":2')])
      await expect(inspectPoliticsFile(file(json))).rejects.toThrow('VERSION');
    for (const value of [{ kind: 'politics-reform-scenario', schemaVersion: 5 }, { kind: 'politics-reform-session', schemaVersion: 1 },
      { schema_version: 9, scenario: { defaultSource: { electionKind: 'politiche' } } },
      { schema_version: 3, scenario: { kind: 'politics-reform-scenario', schemaVersion: 5 }, result: {} }]) for (const compressed of [false, true]) {
      await expect(fileWorkspace(file(JSON.stringify(value), compressed))).rejects.toThrow('precedente non supportato');
      await expect(inspectPoliticsFile(file(JSON.stringify(value), compressed))).rejects.toThrow('precedente non supportato');
    }
  });
});

describe('CP-05 checkpoint and draft isolation', () => {
  it('binds portable v1 numeric state to the exact common scenario and rejects former scenario-bearing states', async () => {
    const r = run(fixture().scenario), s = r.executedScenario;
    const metadata = { methodVersions: [r.execution.implementationSha256], sourceVersions: [s.data.historical.version], assumptions: [] };
    const f = fastGenerationFixture();
    const fitState = { parents: { nationalInputKey: 'national', implementationId: r.execution.implementationSha256, fitSeed: s.execution.fittingSeed, solver: 'dense' },
      fit: { ...f.assets.fit, referenceDate: s.referenceDate }, provenanceId: 'test-fit', warnings: [],
      sampler: { chains: 4, burn: 1000, thin: 2, retention: 'all-accepted-joint-context-draws' } };
    const profileState = { parents: { profileInputKey: 'profiles', pairedInputKey: 'paired', implementationId: r.execution.implementationSha256, referenceDate: s.referenceDate },
      cameraProfile: f.assets.cameraProfile, senateAdapter: { ...f.assets.senateAdapter!, referenceDate: s.referenceDate }, pairedAudit: { constructed: true } };
    const fit = await encodeState('fast-share-fit', fitState, metadata, fittedStateShapes(fitState.fit));
    const profiles = await encodeState('fast-share-profiles', profileState, metadata, profileStateShapes(profileState.cameraProfile));
    const checkpoint = await createPoliticsCheckpoint(s, r.execution, [
      { ...fit, envelope: { ...fit.envelope, kind: 'fast-share-fit' } },
      { ...profiles, envelope: { ...profiles.envelope, kind: 'fast-share-profiles' } },
    ]);
    expect(await validatePoliticsCheckpoint(checkpoint, s, r.execution)).toEqual(checkpoint);
    expect(checkpoint.assets.every(a => a.chunks.every(c => c.length <= 65536))).toBe(true);
    const a = { ...archive(r), portableFit: checkpoint };
    expect(await parsePoliticsArchive(JSON.parse(await text(a)))).toEqual(a);
    let largestChunk = 0;
    for await (const chunk of streamPoliticsArchive(a)) largestChunk = Math.max(largestChunk, chunk.length);
    expect(largestChunk).toBeLessThanOrEqual(65539);
    await expect(parsePoliticsArchive(archive(r))).rejects.toThrow('MISSING_POLITICS_PORTABLE_FIT');
    const changed = { ...s, name: 'Even an inactive/presentation edit changes the exact scenario' };
    await expect(validatePoliticsCheckpoint(checkpoint, changed, r.execution)).rejects.toThrow('IDENTITY_MISMATCH');
    await expect(validatePoliticsCheckpoint(checkpoint, s, { ...r.execution, implementationSha256: 'c'.repeat(64) })).rejects.toThrow('IDENTITY_MISMATCH');
    const corrupt = { ...checkpoint, assets: [{ ...checkpoint.assets[0], chunks: ['AAAA'] }, ...checkpoint.assets.slice(1)] };
    await expect(validatePoliticsCheckpoint(corrupt, s, r.execution)).rejects.toThrow();
    for (const old of [{ kind: 'politics-reform-session', schemaVersion: 1, scenario: s },
      { ...checkpoint, states: [{ kind: 'fast-share-scenario', descriptor: checkpoint.states[0].descriptor }] }])
      await expect(validatePoliticsCheckpoint(old, s, r.execution)).rejects.toThrow();
  });
  it('reads/writes only the common key, validates before writing and never mutates state on failure', () => {
    const oldKeys = ['elezioni:politics-reform-2026:fast-draft:v1', 'elezioni:web:politics-scenario:v1'];
    const values = new Map(oldKeys.map(k => [k, '{invalid old draft}']));
    const storage = { getItem: vi.fn((k: string) => values.get(k) ?? null), setItem: vi.fn((k: string, v: string) => { values.set(k, v); }) };
    expect(readPoliticsDraft(storage)).toBeNull();
    expect(storage.getItem.mock.calls).toEqual([[politicsDraftKey]]);
    const s = fixture().scenario; writePoliticsDraft(storage, s);
    expect(readPoliticsDraft(storage)).toEqual(s);
    const before = new Map(values);
    expect(() => writePoliticsDraft(storage, { ...s, schemaVersion: 99 } as unknown as PoliticsScenario)).toThrow();
    expect(values).toEqual(before);
    values.set(politicsDraftKey, '{corrupt');
    expect(() => readPoliticsDraft(storage)).toThrow();
    expect(values.get(politicsDraftKey)).toBe('{corrupt');
    for (const key of oldKeys) expect(values.get(key)).toBe('{invalid old draft}');
  });
});
