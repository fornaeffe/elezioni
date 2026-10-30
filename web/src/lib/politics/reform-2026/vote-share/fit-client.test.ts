import { describe, it, expect, vi } from 'vitest';
import { ReformFitClient, type FitWorkerPort } from './fit-client';
import type { FitWorkerReply, FitWorkerRequest } from './fit-types';
import { loadedFitFixture } from '../../../../../tests/fixtures/fast-fit-input';
class ManualWorker implements FitWorkerPort {
  onmessage: FitWorkerPort['onmessage'] = null;
  onerror: FitWorkerPort['onerror'] = null;
  onmessageerror: FitWorkerPort['onmessageerror'] = null;
  sent: FitWorkerRequest[] = [];
  terminated = false;
  postMessage(r: FitWorkerRequest) {
    this.sent.push(structuredClone(r));
  }
  terminate() {
    this.terminated = true;
  }
  reply(r: FitWorkerReply) {
    this.onmessage?.({
      data: structuredClone(r),
    } as MessageEvent<FitWorkerReply>);
  }
}
async function setup() {
  const f = await loadedFitFixture(),
    workers: ManualWorker[] = [],
    client = new ReformFitClient(f.configuration, {
      workerFactory: () => {
        const w = new ManualWorker();
        workers.push(w);
        return w;
      },
    });
  const load = client.load();
  await vi.waitFor(() => expect(workers[0]?.sent.length).toBe(1));
  const request = workers[0].sent[0];
  workers[0].reply({ ...request, action: 'result', result: f.result });
  await load;
  return { ...f, workers, client };
}
describe('FV-06 worker revision and ownership lifecycle', () => {
  it('rejects cancelled promises immediately and ignores late replies after a rapid edit', async () => {
    const f = await setup(),
      s = f.client.copyDraft()!;
    s.referenceDate = '2026-10-20';
    f.client.setDraft(s);
    const pending = f.client.fit(),
      rejection = expect(pending).rejects.toMatchObject({ name: 'AbortError' });
    await vi.waitFor(() => expect(f.workers[1]?.sent.length).toBe(1));
    const request = f.workers[1].sent[0];
    s.referenceDate = '2026-10-21';
    f.client.setDraft(s);
    await rejection;
    expect(f.workers[1].terminated).toBe(true);
    f.workers[1].reply({ ...request, action: 'result', result: f.result });
    await Promise.resolve();
    expect(f.client.canRun).toBe(false);
    expect(f.client.copyDraft()!.referenceDate).toBe('2026-10-21');
    f.client.restoreAccepted();
    expect(f.client.canRun).toBe(true);
    expect(f.client.copyDraft()!.referenceDate).toBe('2026-10-19');
    f.client.dispose();
  });
  it('keeps the previous accepted state after worker error, decoding failure and failed retry', async () => {
    const f = await setup(),
      s = f.client.copyDraft()!;
    s.seed += 'edit';
    f.client.setDraft(s);
    let pending = f.client.validate();
    let rejected = expect(pending).rejects.toThrow('synthetic failure');
    await vi.waitFor(() => expect(f.workers[1]?.sent.length).toBe(1));
    f.workers[1].reply({
      ...f.workers[1].sent[0],
      action: 'error',
      error: 'synthetic failure',
      cancelled: false,
    });
    await rejected;
    expect(f.client.state.phase).toBe('error');
    expect(f.client.canRun).toBe(false);
    pending = f.client.fit();
    rejected = expect(pending).rejects.toThrow('decode');
    await vi.waitFor(() => expect(f.workers[2]?.sent.length).toBe(1));
    f.workers[2].onmessageerror?.({} as MessageEvent);
    await rejected;
    f.client.restoreAccepted();
    expect(f.client.canRun).toBe(true);
    f.client.dispose();
  });
  it('binds the reply fingerprint and the returned scenario before atomic installation', async () => {
    const f = await setup(),
      s = f.client.copyDraft()!;
    s.seed += 'edit';
    f.client.setDraft(s);
    const pending = f.client.validate(),
      rejected = expect(pending).rejects.toThrow('BINDING');
    await vi.waitFor(() => expect(f.workers[1]?.sent.length).toBe(1));
    const req = f.workers[1].sent[0];
    f.workers[1].reply({
      ...req,
      fingerprint: 'stale-fingerprint',
      action: 'result',
      result: f.result,
    });
    await Promise.resolve();
    expect(f.client.state.phase).toBe('validating');
    f.workers[1].reply({ ...req, action: 'result', result: f.result });
    await rejected;
    expect(f.client.canRun).toBe(false);
    f.client.restoreAccepted();
    expect(f.client.copyDraft()!.seed).not.toBe(s.seed);
    f.client.dispose();
  });
  it('cancels during fingerprinting without spawning a worker and supports restoring the accepted revision', async () => {
    const f = await setup(),
      pending = f.client.fit(),
      rejection = expect(pending).rejects.toMatchObject({ name: 'AbortError' });
    f.client.cancel();
    await rejection;
    await new Promise((done) => setTimeout(done, 10));
    expect(f.workers).toHaveLength(1);
    f.client.restoreAccepted();
    expect(f.client.canRun).toBe(true);
    f.client.dispose();
    expect(() => f.client.restoreAccepted()).toThrow('disposed');
  });
  it('reserves bounded consumer copies, keeps authoritative arrays attached and prevents concurrent fitting', async () => {
    const f = await setup(),
      handoff = f.client.reserveGenerationCopies(
        f.client.generationCapacity({
          deviceMemory: 8,
          hardwareConcurrency: 8,
        })!.maximum,
        {
          deviceMemory: 8,
          hardwareConcurrency: 8,
        },
      ),
      first = handoff.leases[0],
      bytes = first.packet.assets.fit.political.values.byteLength;
    const received = structuredClone(first.packet, {
      transfer: first.transfer,
    });
    expect(first.packet.assets.fit.political.values.byteLength).toBe(0);
    expect(received.assets.fit.political.values.byteLength).toBe(bytes);
    await expect(f.client.fit()).rejects.toThrow('active simulation');
    expect(() =>
      f.client.reserveGenerationCopies(1, {
        deviceMemory: 8,
        hardwareConcurrency: 8,
      }),
    ).toThrow('MEMORY_LIMIT');
    for (const lease of handoff.leases) {
      lease.release();
      lease.release();
    }
    const next = f.client.reserveGenerationCopies(1, {
      deviceMemory: 8,
      hardwareConcurrency: 8,
    });
    expect(next.leases[0].packet.assets.fit.political.values.byteLength).toBe(
      bytes,
    );
    next.leases[0].release();
    f.client.dispose();
  });
});
