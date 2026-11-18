import { encodeSource, decodeSourceRows, decodeAsset, validateSourceEnvelope } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { compileSource } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { originalShareFixture, sourceSelection } from './fast-share-source-input';
import type { ByteAsset } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import type { CompactSource } from '../../src/lib/politics/reform-2026/vote-share/source-types';
self.onmessage = async event => {
  try {
    if (event.data.action === 'fixture') {
      const encoded = await encodeSource(originalShareFixture()), compiled = await compileSource(encoded.envelope, async p => encoded.assets.get(p)!, sourceSelection());
      self.postMessage({ ok: true, id: encoded.envelope.id, nationalInputKey: compiled.nationalInputKey, profileInputKey: compiled.profileInputKey, national: compiled.national, missingRows: compiled.profiles.elections.map(e => e.rows.length) });
    } else if (event.data.action === 'full') {
      const root: string = event.data.root;
      const execute = async () => {
      const started = performance.now();
      const asset: ByteAsset = await (await fetch(root + '/descriptor.json')).json();
      const load = async (path: string) => new Uint8Array(await (await fetch(root + '/' + path)).arrayBuffer());
      const meta = await decodeAsset(asset, await load(asset.path));
      const source: CompactSource = JSON.parse(new TextDecoder().decode(meta)); await validateSourceEnvelope(source);
      const metadataMilliseconds = performance.now() - started, events = []; let maxLiveNumericBytes = 0;
      for (const e of source.payload.events) {
        const t = performance.now(), rows = await decodeSourceRows(source, e.id, await load(e.chunk.path), (_, bytes) => { maxLiveNumericBytes = Math.max(maxLiveNumericBytes, bytes); });
        events.push({ id: e.id, rows: rows.length, milliseconds: performance.now() - t, valid: rows.reduce((n, r) => n + r.choices.reduce((s, c) => s + c.count, 0), 0) });
      }
      return { ok: true, id: source.id, metadataMilliseconds, totalMilliseconds: performance.now() - started, events, maxLiveNumericBytes,
        decodedMetadataBytes: meta.length, targets: source.payload.geography.targets.length, physicalColleges: source.payload.geography.colleges.length };
      };
      const runs = []; for (let i = 0; i < 6; i++) runs.push(await execute());
      const warm = runs.slice(1).map(r => r.totalMilliseconds).sort((a, b) => a - b);
      self.postMessage({ ...runs[0], runs: runs.map((r, i) => ({ run: i, metadataMilliseconds: r.metadataMilliseconds, totalMilliseconds: r.totalMilliseconds })),
        warmMedianMilliseconds: warm[2], warmMaximumMilliseconds: warm.at(-1), timingQualification: 'First worker run plus five warm runs; local intercepted asset delivery, not a network-transfer speed benchmark',
        memoryQualification: 'Owned live numeric byte count excludes JS row objects, hash copies, metadata and browser process overhead; Node decode process memory is measured separately' });
    }
  } catch (e) { self.postMessage({ ok: false, error: e instanceof Error ? e.message : String(e) }); }
};
