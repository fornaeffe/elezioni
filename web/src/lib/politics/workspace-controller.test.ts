import { describe, expect, it, vi } from 'vitest';
import { PoliticsWorkspaceController, politicsSessionsKey } from './workspace-controller';
import { run as resultFixture } from './files/files.fixture';
import { politicsDraftKey } from './files/draft';
import { fixture } from './scenario/scenario.fixture';
import { selectLaw } from './scenario/edits';
import { ROSATELLUM } from './scenario/schema';
import type { StoredResult } from '../core/result-store';
import type { PreparationSummary, PoliticsRuntime } from './execution/client';

const deferred = <T>() => { let resolve!: (v: T) => void, reject!: (e: unknown) => void;
  const promise = new Promise<T>((a, b) => { resolve = a; reject = b; }); return { promise, resolve, reject }; };
const summary = { keys: { preparationKey: 'p', generationKey: 'g', assemblyKey: 'a' }, reuse: { fit: true, profiles: true },
  capacity: { maximum: 8, recommended: 2 } } as PreparationSummary;
function record(id = 'saved'): StoredResult {
  const run = resultFixture();
  return { id, owner: 'owner', scenario: run.executedScenario, exports: [], politics: { run, checkpoint: null },
    result: { ...run.result, tables: [], warnings: [], storage: { id, state: 'interrupted', tables: [], completedSimulations: 2,
      compressedBytes: 0, maxChunkBytes: 0, warningCounts: { info: 0, warning: 0, error: 0 } } } };
}
function harness(saved: Record<string, string> = {}) {
  const storage = { getItem: vi.fn((key: string) => saved[key] ?? null), setItem: vi.fn((key: string, v: string) => { saved[key] = v; }) };
  const runtime: PoliticsRuntime = { prepare: vi.fn(async () => summary), run: vi.fn(async () => record()),
    cancel: vi.fn(async () => {}), checkpoint: vi.fn(async () => null), close: vi.fn() };
  const manifest = vi.fn(async (id: string) => record(id)), importResult = vi.fn(async () => record('imported'));
  const controller = new PoliticsWorkspaceController(fixture().scenario, { storage, runtime, manifest, importResult,
    validateScenario: async () => {}, changed: vi.fn() });
  return { controller, storage, saved, runtime, manifest, importResult };
}
describe('CP-07 session action table and task ownership', () => {
  it('keeps result browsing, imports, switching and explicit scenario loading independent', async () => {
    const h = harness(); await h.controller.initialize(); await h.controller.selectSession('saved');
    const initial = h.controller.scenario;
    await h.controller.importResult(new File(['{}'], 'result.json'));
    expect(h.controller.scenario).toEqual(initial); expect(h.controller.selected).toBe('imported');
    h.controller.change(selectLaw(initial, ROSATELLUM));
    expect(h.controller.selected).toBe('imported'); expect(h.controller.stale).toBe(true);
    await h.controller.loadViewedScenario();
    expect(h.controller.scenario).toEqual(record().politics!.run.executedScenario);
    expect(h.controller.stale).toBe(false);
    expect(h.storage.getItem.mock.calls.every(([key]) => [politicsDraftKey, politicsSessionsKey].includes(key))).toBe(true);
  });
  it('ignores delayed preparation, view and import replies after newer revisions or disposal', async () => {
    const h = harness(), a = deferred<PreparationSummary>(), b = deferred<PreparationSummary>();
    vi.mocked(h.runtime.prepare).mockReturnValueOnce(a.promise).mockReturnValueOnce(b.promise);
    await h.controller.initialize(); h.controller.change(selectLaw(h.controller.scenario, ROSATELLUM));
    a.resolve({ ...summary, keys: { ...summary.keys, assemblyKey: 'old' } }); await Promise.resolve();
    expect(h.controller.preparation).toBeNull();
    b.resolve(summary); await Promise.resolve(); expect(h.controller.preparation).toEqual(summary);
    const first = deferred<StoredResult>(); h.manifest.mockReturnValueOnce(first.promise);
    const pending = h.controller.selectSession('first'); await h.controller.selectSession('second');
    first.resolve(record('first')); await pending; expect(h.controller.selected).toBe('second');
    h.controller.dispose(); expect(h.runtime.close).toHaveBeenCalledOnce();
  });
  it('restores only the selected session and reports a missing session without substituting another', async () => {
    const h = harness({ [politicsSessionsKey]: JSON.stringify({ owner: 'owner', selected: 'missing', sessions: [{ id: 'other',
      law: ROSATELLUM, model: 'M01@2.0.0', createdAt: '2026-11-14T00:00:00.000Z', renewal: 'joint', completed: 1, requested: 3 }] }) });
    h.manifest.mockRejectedValueOnce(new Error('unavailable')); await h.controller.initialize();
    expect(h.manifest).toHaveBeenCalledExactlyOnceWith('missing', 'owner');
    expect(h.controller.viewed).toBeNull(); expect(h.controller.selected).toBe('missing');
    expect(h.controller.error).toContain('non disponibile');
  });
  it('retains the in-memory draft on quota failure and current results on a corrupt import', async () => {
    const h = harness(); await h.controller.initialize(); await h.controller.selectSession('saved');
    h.storage.setItem.mockImplementation(() => { throw new Error('QuotaExceededError'); });
    const next = selectLaw(h.controller.scenario, ROSATELLUM); h.controller.change(next);
    expect(h.controller.scenario).toEqual(next); expect(h.controller.storageError).toContain('non salvata');
    h.importResult.mockRejectedValueOnce(new Error('corrupt')); await h.controller.importResult(new File(['{'], 'bad.json'));
    expect(h.controller.viewed?.id).toBe('saved'); expect(h.controller.scenario).toEqual(next);
  });
  it('reports a corrupt shared draft while keeping the detached default editable', async () => {
    const h = harness({ [politicsDraftKey]: '{' }); await h.controller.initialize();
    expect(h.controller.error).toContain('Bozza salvata non caricata');
    expect(h.controller.scenario).toEqual(fixture().scenario);
  });
  it('recommends 1000 only after ten complete outcomes, without changing either seeds or count', async () => {
    const h = harness(), saved = record(), run = saved.politics!.run;
    saved.politics!.run = { ...run, executedScenario: { ...run.executedScenario, execution: { ...run.executedScenario.execution, simulations: 10 } },
      session: { ...run.session, state: 'complete', simulations: Array.from({ length: 10 }, (_, i) => ({ id: i + 1, outcome: 'complete' })) } };
    h.manifest.mockResolvedValue(saved); await h.controller.initialize(); const execution = h.controller.scenario.execution;
    await h.controller.selectSession('saved'); expect(h.controller.recommend1000).toBe(true);
    expect(h.controller.scenario.execution).toEqual(execution); expect(h.runtime.run).not.toHaveBeenCalled();
    h.controller.viewed = { ...saved, politics: { ...saved.politics!, run: { ...saved.politics!.run,
      session: { ...saved.politics!.run.session, simulations: [{ id: 1, outcome: 'partial' }] } } } };
    expect(h.controller.recommend1000).toBe(false);
  });
  it('locks execution, keeps both seeds and never auto-reruns or replaces a browsed session', async () => {
    const h = harness(); await h.controller.initialize(); await Promise.resolve();
    const pending = deferred<StoredResult>(); vi.mocked(h.runtime.run).mockReturnValueOnce(pending.promise);
    const captured = h.controller.scenario, running = h.controller.run(); await Promise.resolve();
    expect(h.controller.busy).toBe(true); expect(h.controller.change(selectLaw(captured, ROSATELLUM))).toBe(false);
    await h.controller.selectSession('browsed'); await h.controller.cancel();
    pending.resolve(record('new-run')); await running;
    expect(h.controller.selected).toBe('browsed'); expect(h.controller.scenario.execution).toEqual(captured.execution);
    expect(h.runtime.run).toHaveBeenCalledOnce(); expect(h.runtime.cancel).toHaveBeenCalledOnce();
  });
});
