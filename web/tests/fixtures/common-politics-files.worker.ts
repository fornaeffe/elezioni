import { archive } from '../../src/lib/politics/files/files.fixture';
import { importPoliticsArchiveFile, streamPoliticsArchive } from '../../src/lib/core/result-export';
import { inspectPoliticsFile } from '../../src/lib/politics/files/metadata';
self.onmessage = async () => {
  try {
    const original = archive();
    const encoder = new TextEncoder(), chunks = streamPoliticsArchive(original);
    const stream = new ReadableStream<Uint8Array<ArrayBuffer>>({ async pull(controller) {
      const next = await chunks.next(); if (next.done) controller.close(); else controller.enqueue(encoder.encode(next.value));
    } });
    const file = new File([await new Response(stream.pipeThrough(new CompressionStream('gzip'))).blob()], 'results.json.gz');
    const rows: unknown[] = [], warnings: unknown[] = []; let committed = false, aborted = false;
    const sink = { async table() {}, async row(_i: number, row: unknown) { rows.push(row); }, async warning(warning: unknown) { warnings.push(warning); },
      async commit() { committed = true; }, async abort() { aborted = true; } };
    const header = await importPoliticsArchiveFile(file, sink);
    const metadata = await inspectPoliticsFile(file);
    const corrupt = new File(['{"kind":"politics-result-archive","schemaVersion":1,"run":'], 'corrupt.json');
    let rejected = false;
    try { await importPoliticsArchiveFile(corrupt, { ...sink, async commit() { throw new Error('Must not commit'); } }); }
    catch { rejected = true; }
    self.postMessage({ ok: true, rows, warnings, header, metadata, committed, aborted, rejected });
  } catch (error) { self.postMessage({ ok: false, error: String(error) }); }
};
