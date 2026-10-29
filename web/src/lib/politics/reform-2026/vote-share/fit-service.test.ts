import { describe, it, expect, vi } from 'vitest';
import {
  loadedFitFixture,
  originalFitFixture,
} from '../../../../../tests/fixtures/fast-fit-input';
import { runFitTask } from './fit-service';
import { canonicalJson } from './source-codec';
import {
  copyGenerationPacket,
  generationCapacity,
  ownedBuffers,
  MiB,
} from './fit-resources';
describe('FV-06 semantic refitting and task-owned state', () => {
  it('loads the precomputed default without source fetches or fits and reuses it for future-only edits', async () => {
    const f = await loadedFitFixture();
    expect(f.result.audit).toMatchObject({
      nationalFits: 0,
      profileFits: 0,
      pairedFits: 0,
      sourceLoaded: false,
    });
    const scenario = structuredClone(f.accepted.scenario);
    scenario.seed += 'changed';
    scenario.electionDate = '2027-10-01';
    scenario.lists.camera[0] = {
      ...scenario.lists.camera[0],
      name: scenario.lists.camera[0].name + ' renamed',
    };
    const read = vi.fn(f.read),
      result = await runFitTask(
        {
          action: 'validate',
          configuration: f.configuration,
          previous: f.accepted,
          scenario,
        },
        read,
      );
    expect(read).not.toHaveBeenCalled();
    expect(result.accepted!.assets.fit).toBe(f.accepted.assets.fit);
    expect(result.accepted!.keys.runKey).not.toBe(f.accepted.keys.runKey);
  });
  it('refits a custom poll once, reuses camera and Senate profiles, and preserves original source counts', async () => {
    const f = await originalFitFixture(),
      before = canonicalJson(f.source),
      scenario = structuredClone(f.accepted.scenario);
    scenario.polls.added = [f.customPoll];
    scenario.polls.selected.push(f.customPoll.id);
    const plan = await runFitTask(
      {
        action: 'validate',
        configuration: f.configuration,
        previous: f.accepted,
        scenario,
      },
      f.read,
    );
    expect(plan.accepted).toBeUndefined();
    expect(plan.plan).toMatchObject({
      national: true,
      profiles: false,
      paired: false,
    });
    const result = await runFitTask(
      {
        action: 'fit',
        configuration: f.configuration,
        previous: f.accepted,
        scenario,
      },
      f.read,
    );
    expect(result.audit).toMatchObject({
      nationalFits: 1,
      profileFits: 0,
      pairedFits: 0,
    });
    expect(result.accepted!.assets.cameraProfile).toBe(
      f.accepted.assets.cameraProfile,
    );
    expect(result.accepted!.assets.senateAdapter).toBe(
      f.accepted.assets.senateAdapter,
    );
    expect(result.accepted!.assets.fittedStateId).not.toBe(
      f.accepted.assets.fittedStateId,
    );
    expect(canonicalJson(f.source)).toBe(before);
    const previous = result.accepted!;
    scenario.data = { ...previous.scenario.data };
    scenario.polls.added[0].citation += ' correction';
    const read = vi.fn(f.read),
      citation = await runFitTask(
        {
          action: 'validate',
          configuration: f.configuration,
          previous,
          scenario,
        },
        read,
      );
    expect(read).not.toHaveBeenCalled();
    expect(citation.audit.nationalFits).toBe(0);
    expect(citation.accepted!.provenanceId).not.toBe(previous.provenanceId);
  });
  it('invalidates historical memberships and even a one-day date change; paired-only edits reuse the C/E fit', async () => {
    const f = await originalFitFixture(),
      scenario = structuredClone(f.accepted.scenario);
    scenario.sourceMemberships['c10:0'].members = ['b'];
    scenario.sourceMemberships['c10:1'].members = ['a'];
    const history = await runFitTask(
      {
        action: 'fit',
        configuration: f.configuration,
        previous: f.accepted,
        scenario,
      },
      f.read,
    );
    expect(history.audit).toMatchObject({
      nationalFits: 1,
      profileFits: 1,
      pairedFits: 1,
    });
    const date = structuredClone(f.accepted.scenario);
    date.referenceDate = '2026-10-20';
    const changed = await runFitTask(
      {
        action: 'fit',
        configuration: f.configuration,
        previous: f.accepted,
        scenario: date,
      },
      f.read,
    );
    expect(changed.audit).toMatchObject({
      nationalFits: 1,
      profileFits: 1,
      pairedFits: 1,
    });
    expect(changed.accepted!.assets.fit.referenceDate).toBe(date.referenceDate);
    const paired = structuredClone(f.accepted.scenario);
    paired.sourceMemberships['s22:0'].members = ['c'];
    paired.sourceMemberships['s22:2'].members = ['a'];
    const p = await runFitTask(
      {
        action: 'fit',
        configuration: f.configuration,
        previous: f.accepted,
        scenario: paired,
      },
      f.read,
    );
    expect(p.audit).toMatchObject({
      nationalFits: 0,
      profileFits: 0,
      pairedFits: 1,
    });
  });
  it('rejects invalid evidence/bindings and aggregate budgets without overwriting the accepted fit', async () => {
    const f = await originalFitFixture(),
      before = f.accepted.assets.fittedStateId,
      scenario = structuredClone(f.accepted.scenario);
    scenario.polls.added = [{ ...f.customPoll, effectiveSample: 0 }];
    scenario.polls.selected.push(f.customPoll.id);
    await expect(
      runFitTask(
        {
          action: 'fit',
          configuration: f.configuration,
          previous: f.accepted,
          scenario,
        },
        f.read,
      ),
    ).rejects.toThrow('INVALID_USER_POLL');
    scenario.polls.added = [structuredClone(f.customPoll)];
    scenario.polls.added[0].original[0].percentage = 0;
    await expect(runFitTask({ action: 'fit', configuration: f.configuration, previous: f.accepted, scenario }, f.read)).rejects.toThrow('INVALID_USER_POLL');
    scenario.polls.added = [];
    scenario.referenceDate = '2020-01-01';
    scenario.polls.selected = [];
    await expect(
      runFitTask(
        {
          action: 'fit',
          configuration: f.configuration,
          previous: f.accepted,
          scenario,
        },
        f.read,
      ),
    ).rejects.toThrow();
    await expect(
      runFitTask(
        {
          action: 'fit',
          configuration: { ...f.configuration, maximumAggregateBytes: MiB },
          previous: f.accepted,
          scenario: f.accepted.scenario,
        },
        f.read,
      ),
    ).rejects.toThrow('AGGREGATE_MEMORY');
    expect(f.accepted.assets.fittedStateId).toBe(before);
  });
  it('cancels between numeric batches and transfers only disposable consumer copies', async () => {
    const f = await originalFitFixture(),
      controller = new AbortController(),
      scenario = structuredClone(f.accepted.scenario);
    scenario.fitSeed += 'refit';
    await expect(
      runFitTask(
        {
          action: 'fit',
          configuration: f.configuration,
          previous: f.accepted,
          scenario,
        },
        f.read,
        {
          signal: controller.signal,
          checkpoint: async (p) => {
            if (p.phase === 'sampling') controller.abort();
          },
        },
      ),
    ).rejects.toMatchObject({ name: 'AbortError' });
    const packet = copyGenerationPacket(f.accepted),
      bytes = f.accepted.assets.fit.political.values.byteLength;
    const received = structuredClone(packet, {
      transfer: ownedBuffers(packet),
    });
    expect(packet.assets.fit.political.values.byteLength).toBe(0);
    expect(received.assets.fit.political.values.byteLength).toBe(bytes);
    expect(f.accepted.assets.fit.political.values.byteLength).toBe(bytes);
    expect(
      generationCapacity(received, { deviceMemory: 1, hardwareConcurrency: 8 })
        .maximum,
    ).toBe(0);
    expect(
      generationCapacity(received, { deviceMemory: 8, hardwareConcurrency: 8 })
        .maximum,
    ).toBeGreaterThanOrEqual(2);
  });
});
