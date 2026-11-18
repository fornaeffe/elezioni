/** Test-only static candidate reader. The production fit/client lifecycle is FV-06. */
import {
  loadCandidate,
  loadCandidateSource,
  type CandidatePointer,
  type ReleaseCatalog,
} from '../../src/lib/politics/reform-2026/vote-share/release';
import { decodeState } from '../../src/lib/politics/reform-2026/vote-share/state-codec';
import { decodeSourceRows } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
export interface ReleaseWorkerRequest {
  root: string;
  implementation: string;
  action: 'generate' | 'source';
}
const scope = self as unknown as {
  onmessage: ((event: MessageEvent<ReleaseWorkerRequest>) => void) | null;
  postMessage(value: unknown): void;
};
scope.onmessage = async (event) => {
  const started = performance.now(),
    r = event.data,
    requests: string[] = [],
    encodings: string[] = [];
  let receivedBodyBytes = 0,
    reportedEncodedBytes = 0;
  try {
    const read = async (path: string) => {
      if (!/^[\w.-]+$/.test(path) || path === '.' || path === '..')
        throw new Error('Unsafe asset name');
      requests.push(path);
      const response = await fetch(r.root + path);
      if (!response.ok) throw new Error('HTTP ' + response.status + ' ' + path);
      const body = new Uint8Array(await response.arrayBuffer());
      receivedBodyBytes += body.length;
      const length = response.headers.get('Content-Length');
      if (length === null || !/^\d+$/.test(length))
        throw new Error('Test server must report Content-Length');
      reportedEncodedBytes += Number(length);
      encodings.push(response.headers.get('Content-Encoding') ?? 'identity');
      return { transport: 'http' as const, body };
    };
    const pointerBytes = (await read('candidate.json')).body,
      pointer: CandidatePointer = JSON.parse(
        new TextDecoder().decode(pointerBytes),
      );
    if (r.action === 'source') {
      const catalog = await decodeState<ReleaseCatalog>(
        pointer.catalog,
        read,
        'fast-share-catalog',
      );
      if (
        catalog.envelope.id !== pointer.catalogId ||
        catalog.state.generationImplementationId !== r.implementation
      )
        throw new Error(
          'Catalog implementation mismatch: ' +
            catalog.state.generationImplementationId +
            ' / ' +
            r.implementation,
        );
      const source = await loadCandidateSource(catalog.state, read);
      let rows = 0;
      for (const e of source.payload.events)
        rows += (await decodeSourceRows(source, e.id, await read(e.chunk.path)))
          .length;
      scope.postMessage({
        ok: true,
        action: r.action,
        sourceId: source.id,
        events: source.payload.events.map((e) => ({
          id: e.id,
          minimumAge: e.minimumAge,
        })),
        rows,
        requests,
        receivedBodyBytes,
        reportedEncodedBytes,
        encodings,
        milliseconds: performance.now() - started,
      });
      return;
    }
    const loaded = await loadCandidate(pointer, read, {
        expectedGenerationImplementationId: r.implementation,
      }),
      result = loaded.prepared!.run(1);
    const hash = async (bytes: Uint8Array<ArrayBuffer>) =>
      [...new Uint8Array(await crypto.subtle.digest('SHA-256', bytes))]
        .map((x) => x.toString(16).padStart(2, '0'))
        .join('');
    scope.postMessage({
      ok: true,
      action: r.action,
      catalogId: pointer.catalogId,
      inputHash: await hash(
        new TextEncoder().encode(JSON.stringify(result.input)),
      ),
      fitHash: await hash(
        new Uint8Array(loaded.assets.fit.political.values.buffer),
      ),
      counts: {
        camera: loaded.prepared!.cells.camera.reduce(
          (a, c) => a + c.electors,
          0,
        ),
        senato: loaded.prepared!.cells.senato.reduce(
          (a, c) => a + c.electors,
          0,
        ),
      },
      requests,
      receivedBodyBytes,
      reportedEncodedBytes,
      encodings,
      expectedBytes:
        loaded.catalog.byteBudget.generationPayloadCompressedBytes +
        pointer.catalog.compressedBytes +
        pointerBytes.length,
      sourceMetadata: loaded.catalog.assets.source.path,
      projectionResidual: result.audit.projectionResidual,
      milliseconds: performance.now() - started,
    });
  } catch (e) {
    scope.postMessage({
      ok: false,
      error: e instanceof Error ? e.message : String(e),
      requests,
      receivedBodyBytes,
      reportedEncodedBytes,
      encodings,
    });
  }
};
