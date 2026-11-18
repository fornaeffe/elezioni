import { archive } from '../../src/lib/politics/files/files.fixture';
import { handleResultService } from '../../src/lib/workers/result-service';
import { deleteStoredResult, getStoredResult } from '../../src/lib/core/result-store';

// Constructed, deterministic data exercises the real parser, chunk store,
// membership checks, gzip and OPFS. It is not a national simulation campaign.
self.onmessage = async (event: MessageEvent<{ rows: number }>) => {
  try {
    const n = event.data.rows, original = archive();
    const tables = [{ name: 'Archive benchmark',
      columns: ['Simulation', ...Array.from({ length: 15 }, (_, i) => `col${i}`)],
      rows: Array.from({ length: n }, (_, i) => [i % 2 + 1, 'camera', `constituency-${i % 28}`, `college-${i % 245}`,
        `list-${i % 18}`, `Candidato "${i}" \\ 😀`, 100000 + i % 971, (i % 1000) / 1000,
        i % 7, i % 2 === 0, null, '', 'ordinary', `party-${i % 34}`, i % 9, i / 17]) }];
    const value = { ...original, run: { ...original.run, result: { ...original.run.result, warnings: [], tables } } };
    const raw = new TextEncoder().encode(JSON.stringify(value));
    value.run.result.tables[0].rows = [];
    const compressed = await new Response(new Blob([raw]).stream().pipeThrough(new CompressionStream('gzip'))).blob();
    const file = new File([compressed], 'benchmark.json.gz'), owner = 'archive-performance';
    let requestId = 0;
    async function call(action: 'import' | 'export', resultId: string, input?: File) {
      self.postMessage({ stage: action, requestId: requestId + 1 });
      const reply = await handleResultService({ type: 'result-service', requestId: ++requestId, action, resultId, owner, file: input, format: 'json' });
      if (!reply.ok) throw new Error(reply.error?.message);
      return reply.data;
    }
    async function digest(input: Blob) {
      const text = await new Response(input.stream().pipeThrough(new DecompressionStream('gzip'))).text();
      return Array.from(new Uint8Array(await crypto.subtle.digest('SHA-256', new TextEncoder().encode(
        text.replace(/("exportedAt":)"[^"]+"/, '$1"fixed"')))), b => b.toString(16).padStart(2, '0')).join('');
    }
    const trials = [];
    for (let i = 0; i < 3; i++) {
      const id = crypto.randomUUID(), restoredId = crypto.randomUUID();
      const start = performance.now(); await call('import', id, file); const importMs = performance.now() - start;
      const beforeExport = performance.now(), exported = await call('export', id) as File, exportMs = performance.now() - beforeExport;
      const beforeRoundtrip = performance.now(); await call('import', restoredId, exported); const reimportMs = performance.now() - beforeRoundtrip;
      const again = await call('export', restoredId) as File;
      if (await digest(exported) !== await digest(again) || await digest(file) !== await digest(exported)) throw new Error('Archive roundtrip mismatch');
      const stored = await getStoredResult(restoredId, owner);
      if (stored.result.storage!.tables[0].rows !== n) throw new Error('Missing rows');
      trials.push({ importMs, exportMs, reimportMs, exportBytes: exported.size, compressedStorageBytes: stored.result.storage!.compressedBytes });
      self.postMessage({ stage: 'trial', trial: trials.at(-1) });
      await deleteStoredResult(id, owner); await deleteStoredResult(restoredId, owner);
    }
    self.postMessage({ ok: true, rows: n, rawBytes: raw.length, gzipBytes: file.size, trials, roundtrip: true });
  } catch (error) { self.postMessage({ ok: false, error: String(error) }); }
};
