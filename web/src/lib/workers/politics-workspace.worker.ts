import { handleResultService, setResultActive, type ResultServiceRequest } from './result-service';
import { ReformComputeClient } from './reform-pool';
import { PoliticsPreparationCache, preparePoliticsExecution, type PreparedPoliticsExecution } from '../politics/execution/prepare';
import type { PoliticsComputePacket } from './politics-compute';
import type { PoliticsScenario, Asset } from '../politics/scenario/schema';
import type { PoliticsCheckpoint } from '../politics/scenario/envelopes';
import { commonPoliticsImplementation } from '../politics/execution/build';
import { checkpointStates, executionCheckpoint } from '../politics/execution/checkpoint';
import { politicsCapacity } from '../politics/execution/resources';
import { runStoredPolitics } from '../politics/execution/stored-run';
import { canonicalJson } from '../politics/scenario/dependencies';
import type { AssetBytes } from '../politics/reform-2026/vote-share/source-codec';
import { readPreparation, writePreparation } from '../politics/execution/preparation-store';

export interface PoliticsTaskRequest {
  type: 'politics-task'; requestId: number; action: 'prepare' | 'run' | 'cancel' | 'checkpoint' | 'dispose';
  basePath: string; scenario?: PoliticsScenario; force?: boolean; checkpoint?: PoliticsCheckpoint; id?: string; owner?: string;
}
let prepared: PreparedPoliticsExecution | undefined, packet: PoliticsComputePacket | undefined;
let task: AbortController | undefined, sequence = 0, running = false, cancellation = false;
const cache = new PoliticsPreparationCache();
// At most the current source's chunks and sixteen content-pinned JSON assets.
const assetCache = new Map<string, Uint8Array<ArrayBuffer>>(), chunkCache = new Map<string, AssetBytes>();
let sourceIdentity = '';
let restoredCache = false, persistedKeys = '', cacheNote = '';
const ownedWorkers = new Set<ReformComputeClient>();
self.onmessage = (event: MessageEvent<PoliticsTaskRequest | ResultServiceRequest>) => {
  const request = event.data;
  if (request.type === 'result-service') { void handleResultService(request).then(reply => self.postMessage(reply)); return; }
  const reply = (data: unknown) => self.postMessage({ type: 'politics-task', requestId: request.requestId, ok: true, data });
  if (request.action === 'dispose') { task?.abort(); ++sequence; for (const worker of ownedWorkers) worker.close(); ownedWorkers.clear(); reply(null); self.close(); return; }
  if (request.action === 'cancel') { cancellation = true; task?.abort(); ++sequence; reply(null); return; }
  void (async () => {
    if (running) throw new Error('Execution is locked; cancel it explicitly before changing tasks');
    if (request.action === 'checkpoint') { if (!prepared) throw new Error('No prepared model'); return reply(await executionCheckpoint(prepared)); }
    if (request.action === 'run') {
      if (!prepared || !packet || canonicalJson(prepared.scenario) !== canonicalJson(request.scenario)) throw new Error('STALE_POLITICS_PREPARATION');
      running = true; cancellation = false;
      setResultActive(request.id!, true);
      try {
        const record = await runStoredPolitics(prepared, packet, request.id!, request.owner!, () => cancellation,
          progress => self.postMessage({ type: 'politics-task', requestId: request.requestId, progress }), () => {
            const worker = new ReformComputeClient(new Worker(new URL('./reform-compute.worker.ts', import.meta.url), { type: 'module' }));
            ownedWorkers.add(worker);
            const close = worker.close.bind(worker);
            worker.close = () => { close(); ownedWorkers.delete(worker); };
            return worker;
          });
        // Fitted arrays remain worker-owned; result browsing never transfers them to the UI.
        reply({ ...record, politics: record.politics ? { ...record.politics, checkpoint: null } : undefined });
      } finally { running = false; setResultActive(request.id!, false); }
      return;
    }
    task?.abort(); const controller = new AbortController(); task = controller;
    const revision = ++sequence, s = request.scenario!;
    if (!commonPoliticsImplementation) throw new Error('Missing common politics build identity');
    if (!restoredCache) {
      restoredCache = true;
      try {
        const saved = await readPreparation();
        if (saved) {
          const restored = await checkpointStates(saved.checkpoint, saved.scenario, commonPoliticsImplementation);
          cache.fit = { key: saved.fitKey, state: restored.states.fit };
          cache.profiles = { key: saved.profileKey, state: restored.states.profiles };
          persistedKeys = `${saved.fitKey}/${saved.profileKey}`;
        }
      } catch { cacheNote = 'Cache locale non riutilizzabile; preparazione ordinaria.'; }
    }
    if (request.force) { cache.clear(); persistedKeys = ''; }
    const assets = new Map<string, Uint8Array<ArrayBuffer>>();
    const sourceKey = `${request.basePath}/${s.data.historical.sha256}`;
    if (sourceIdentity !== sourceKey) { chunkCache.clear(); sourceIdentity = sourceKey; }
    const chunks = chunkCache;
    const fetchBytes = async (uri: string) => {
      const response = await fetch(`${request.basePath}/${uri.replace(/^\//, '')}`, { signal: controller.signal });
      if (!response.ok) throw new Error(`Exact execution asset unavailable: ${uri} (${response.status})`);
      return new Uint8Array(await response.arrayBuffer());
    };
    const restored = request.checkpoint ? await checkpointStates(request.checkpoint, s, commonPoliticsImplementation) : undefined;
    let lastYield = performance.now(), lastProgress = 0;
    const next = await preparePoliticsExecution(s, { implementationId: commonPoliticsImplementation,
      asset: async (pin: Asset) => {
        const key = `${request.basePath}/${pin.uri}/${pin.sha256}`;
        const bytes = assetCache.get(key) ?? await fetchBytes(pin.uri);
        if (!assetCache.has(key)) { assetCache.set(key, bytes); if (assetCache.size > 16) assetCache.delete(assetCache.keys().next().value!); }
        assets.set(pin.id, bytes); return bytes;
      },
      sourceChunk: async path => {
        if (!/^[\w.-]+$/.test(path)) throw new Error('Invalid source chunk path');
        // Original-column chunks retain their pinned preparation delivery root.
        if (!chunks.has(path)) chunks.set(path, { transport: 'http', body: await fetchBytes(`data/reform-2026/fast-share/${path}`) });
        return chunks.get(path)!;
      },
    }, { cache, numericStates: restored?.states, signal: controller.signal,
      checkpoint: async p => {
        const now = performance.now();
        // Promise-only stepping starves worker messages. Yield a task so cancel,
        // disposal and newer revisions can interrupt even a long national fit.
        if (now - lastYield >= 16) { await new Promise(resolve => setTimeout(resolve, 0)); lastYield = performance.now(); }
        if (now - lastProgress >= 100 && revision === sequence && !controller.signal.aborted) {
          lastProgress = now;
          self.postMessage({ type: 'politics-task', requestId: request.requestId,
            progress: { type: 'progress', phase: 'prepare', completed: p.completed, total: p.total, elapsedMs: 0, detail: p.phase } });
        }
      } });
    if (revision !== sequence || controller.signal.aborted) throw new Error('Preparation cancelled');
    if (restored && restored.file.preparationKey !== next.keys.preparationKey) throw new Error('POLITICS_CHECKPOINT_IDENTITY_MISMATCH');
    prepared = next;
    packet = { scenario: next.scenario, implementationId: next.implementationId, keys: next.keys, numericStates: next.numericStates,
      assets: [...assets].map(([id, bytes]) => ({ id, bytes })), sourceChunks: [...chunks].map(([path, bytes]) => ({ path, bytes })) };
    const cacheKeys = cache.fit && cache.profiles ? `${cache.fit.key}/${cache.profiles.key}` : '';
    if (next.numericStates && cacheKeys && cacheKeys !== persistedKeys) {
      try {
        const checkpoint = await executionCheckpoint(next);
        if (checkpoint && revision === sequence && !controller.signal.aborted) {
          await writePreparation({ scenario: next.scenario, checkpoint, fitKey: cache.fit!.key, profileKey: cache.profiles!.key }); persistedKeys = cacheKeys;
        }
      } catch { cacheNote = 'Fitting pronto in memoria; cache locale non salvata.'; }
    }
    if (revision !== sequence || controller.signal.aborted) throw new Error('Preparation cancelled');
    reply({ keys: next.keys, reuse: next.reuse, capacity: politicsCapacity(packet, navigator), note: cacheNote });
  })().catch(error => self.postMessage({ type: 'politics-task', requestId: request.requestId, ok: false, error: String(error) }));
};
