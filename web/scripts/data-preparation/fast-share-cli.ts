import { mkdir, readFile, writeFile } from 'node:fs/promises';
import { resolve, dirname } from 'node:path';
import { fileURLToPath } from 'node:url';
import { acquireSource, writeVersion } from './sources';
import { shareSourceManifests } from './fast-share-history';
import { prepareFastShareSource } from './fast-share-preparation';
import { decodeAsset, decodeSourceRows, sourceKey, validateSourceEnvelope, type ByteAsset } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { compileSource, type SourceSelection } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { fitReference, FAST_SHARE_MODEL, FAST_SHARE_NUMERICS } from '../../src/lib/politics/reform-2026/vote-share/index';
import type { CompactSource } from '../../src/lib/politics/reform-2026/vote-share/source-types';

const root = fileURLToPath(new URL('../../../', import.meta.url)), [command, ...args] = process.argv.slice(2);
const allowed = new Set(['--sources','--output','--asset','--request','--seed','--only','--report']);
if (args.length % 2 || args.some((v, i) => i % 2 === 0 && !allowed.has(v)) || new Set(args.filter((_, i) => i % 2 === 0)).size !== args.length / 2) throw new Error('Invalid/duplicate fast-share option');
const option = (name: string, fallback = '') => args.includes(name) ? args[args.indexOf(name) + 1] : fallback;
const output = resolve(option('--output', resolve(root, 'data/derived/fast-share'))), sources = resolve(option('--sources', resolve(root, 'data/sources')));
async function immutableBytes(path: string, bytes: Uint8Array) {
  try { if (!Buffer.from(bytes).equals(await readFile(path))) throw new Error('Immutable source asset differs: ' + path); return; }
  catch (e) { if ((e as NodeJS.ErrnoException).code !== 'ENOENT') throw e; }
  await mkdir(dirname(path), { recursive: true }); await writeFile(path, bytes, { flag: 'wx' });
}
async function readBundle() {
  const path = option('--asset'); if (!path) throw new Error('--asset DESCRIPTOR.json is required; there is no mutable latest/default pointer');
  const asset: ByteAsset = JSON.parse(await readFile(resolve(path), 'utf8'));
  if (!/^[\w.-]+$/.test(asset.path)) throw new Error('Unsafe asset path');
  const directory = dirname(resolve(path)), bytes = await decodeAsset(asset, await readFile(resolve(directory, asset.path)));
  const source: CompactSource = JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(bytes));
  await validateSourceEnvelope(source);
  return { source, directory, asset };
}
if (command === 'download') {
  const manifests = await shareSourceManifests(root), selected = manifests.sources.filter(s => !option('--only') || s.id === option('--only'));
  if (!selected.length) throw new Error('Unknown fast-share source');
  for (const source of selected) { await acquireSource(sources, source); console.log('Verified ' + source.id); }
} else if (command === 'prepare') {
  const started = performance.now(), prepared = await prepareFastShareSource(root, sources);
  for (const [path, bytes] of prepared.assets) await immutableBytes(resolve(output, path), bytes);
  const stem = prepared.envelope.id;
  await writeVersion(resolve(output, stem + '.asset.json'), prepared.descriptor);
  const auditFile = await sourceKey('source-audit-v1', prepared.audit) + '.json';
  await writeVersion(resolve(output, auditFile), prepared.audit);
  console.log(JSON.stringify({ id: stem, asset: resolve(output, stem + '.asset.json'), auditFile: resolve(output, auditFile), milliseconds: performance.now() - started, audit: { ...prepared.audit, sourceEvents: undefined, targetMissingHistories: undefined, membershipWarnings: undefined } }, null, 2));
} else if (command === 'check') {
  const { source, directory, asset } = await readBundle();
  const rows: { event: string; municipalities: number; explicitZeros: number; decodedBytes: number }[] = [];
  const memory = [{ phase: 'metadata-loaded', liveNumericBytes: 0, ...process.memoryUsage() }];
  for (const event of source.payload.events) {
    const original = await decodeSourceRows(source, event.id, await readFile(resolve(directory, event.chunk.path)), (phase, liveNumericBytes) => memory.push({ phase: event.id + ':' + phase, liveNumericBytes, ...process.memoryUsage() }));
    if (event.id === 'camera-2022' || event.id === 'senato-2022') {
      const units = new Map(source.payload.units.map(u => [u.id, u])), codes = new Set(original.map(r => units.get(r.unit)!.targetCode));
      if (source.payload.geography.targets.some(t => !codes.has(t.code))) throw new Error('Missing paired production target history');
    }
    rows.push({ event: event.id, municipalities: original.length, explicitZeros: original.reduce((n, r) => n + r.choices.filter(c => c.count === 0).length, 0), decodedBytes: event.chunk.decodedBytes });
  }
  if (source.payload.events.length !== 10 || source.payload.geography.targets.length !== 7904 || source.payload.geography.colleges.length !== 86 || source.payload.events.find(e => e.id === 'senato-2018')?.minimumAge !== 25) throw new Error('Incomplete FV-03 production source envelope');
  const started = performance.now(), compressed = await readFile(resolve(directory, asset.path));
  const decoded = await decodeAsset(asset, compressed), parsed = JSON.parse(new TextDecoder().decode(decoded));
  const report = { passed: true, sourceId: source.id, originalCounts: rows, metadataDecodeMilliseconds: performance.now() - started, decodedMetadataBytes: decoded.length,
    decodedSourceId: parsed.id, memory, peakRssKiB: process.resourceUsage().maxRSS, qualification: 'Measured Node memory at inflate/column/row boundaries and process lifetime peak RSS; includes metadata/JSON rows/hash copies and prior chunks awaiting GC. Not browser process peak memory.' };
  if (option('--report')) { const path = resolve(option('--report')); await mkdir(dirname(path), { recursive: true }); await writeFile(path, JSON.stringify(report, null, 2) + '\n'); }
  console.log(JSON.stringify(option('--report') ? { passed: true, sourceId: source.id, events: rows.length, peakRssKiB: report.peakRssKiB, report: resolve(option('--report')) } : report, null, 2));
} else if (command === 'prefit') {
  const { source, directory } = await readBundle();
  if (!option('--request')) throw new Error('prefit requires --request with explicit source selection, basis, targets and date; no production default is selected');
  const request: SourceSelection = JSON.parse(await readFile(resolve(option('--request')), 'utf8'));
  const compiled = await compileSource(source, path => readFile(resolve(directory, path)), request), seed = option('--seed', 'fast-share-prefit-v1');
  const fit = fitReference(compiled.national, seed);
  const payload = JSON.parse(JSON.stringify({ ...fit, sourceBundleId: source.id, provenanceId: compiled.provenanceId, selection: request,
    nationalInputKey: compiled.nationalInputKey, profileInputKey: compiled.profileInputKey, fitSeed: seed, warnings: compiled.warnings,
    sampler: { chains: 4, burn: 1000, thin: 2, retained: 'adaptive-2000-4000-8000' } }, (_, v) => ArrayBuffer.isView(v) ? Array.from(v as unknown as ArrayLike<number>) : v));
  const body = { schemaVersion: 1, kind: 'fast-share-fit', modelId: FAST_SHARE_MODEL, numericalVersion: FAST_SHARE_NUMERICS, sourceVersions: source.sourceVersions,
    methodVersions: source.methodVersions, assumptions: [...source.assumptions, 'Numerical Camera/European reference only; no Senate/default/forecast/profile installation'], payload };
  const envelope = { ...body, id: await sourceKey('fast-share-fit-v1', body) };
  await writeVersion(resolve(output, envelope.id + '.json'), envelope);
  console.log(JSON.stringify({ id: envelope.id, nationalInputKey: compiled.nationalInputKey, accepted: fit.political.diagnostics.passed && fit.participation.diagnostics.passed, installed: false }));
} else throw new Error('Usage: data:reform:shares:download | prepare | check --asset FILE | prefit --asset FILE --request FILE');
