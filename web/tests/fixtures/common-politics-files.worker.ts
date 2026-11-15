import { archive } from '../../src/lib/politics/files/files.fixture';
import { importPoliticsArchiveFile, streamPoliticsArchive } from '../../src/lib/core/result-export';
import { inspectPoliticsFile } from '../../src/lib/politics/files/metadata';
import { handleResultService } from '../../src/lib/workers/result-service';
import { deleteStoredResult } from '../../src/lib/core/result-store';
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
    const resultId = crypto.randomUUID(), owner = 'cp08-files';
    // No opt-in flag is needed: the production service has only a strict common reader.
    const current = await handleResultService({ type: 'result-service', requestId: 1, action: 'import', resultId, owner, file });
    const old = await handleResultService({ type: 'result-service', requestId: 2, action: 'import', resultId: crypto.randomUUID(), owner,
      file: new File([JSON.stringify({ schema_version: 3, scenario: {}, result: {} })], 'old.json') });
    if (current.ok) await deleteStoredResult(resultId, owner);
    self.postMessage({ ok: true, rows, warnings, header, metadata, committed, aborted, rejected, currentAccepted: current.ok, oldRejected: !old.ok });
  } catch (error) { self.postMessage({ ok: false, error: String(error) }); }
};
