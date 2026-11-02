import { describe, it, expect, vi } from 'vitest';
// Constructed fixtures use synthetic implementation IDs. Browser release tests
// exercise the independently fingerprinted application build instead.
vi.mock('./build', () => ({ fastReformImplementations: undefined }));
import {
  loadedFitFixture,
  originalFitFixture,
} from '../../../../../tests/fixtures/fast-fit-input';
import {
  createCheckpoint,
  checkpointReader,
  requirePortableFitFile,
  sameCheckpointScenario,
} from './checkpoint';
import { loadCandidate } from './release';
import { runFitTask } from './fit-service';
import {
  prepareFastCompute,
  computeReformRun,
} from '../../../workers/reform-compute';
import { copyGenerationPacket } from './fit-resources';
import {
  addFastList,
  addReferencedList,
  setFastReference,
  removeFastList,
  moveFastComponent,
  labelFastDraft,
  futureEvidenceSignature,
} from './editor-model';
import { requireFastScenario, validateFastScenario } from './scenario';
import {
  createSimulationResultExport,
  parseSimulationResultExport,
} from '../../../core/result-export';
import type { SimulationResult } from '../../../core/types';
describe('FV-07 scenario editing and portable fitted sessions', () => {
  it('requires frozen fitted state for schema 3 results and preserves columnar replay', async () => {
    const f = await loadedFitFixture(),
      file = await createCheckpoint(f.accepted),
      prepared = await prepareFastCompute(copyGenerationPacket(f.accepted)),
      batch = computeReformRun(prepared, 1).batch;
    const result: SimulationResult = {
      type: 'result',
      status: 'completed',
      tables: batch.tables,
      warnings: batch.warnings,
      benchmark: {
        startedAt: '2026-10-29T12:00:00.000Z',
        elapsedMs: 1,
        simulations: 1,
        dataVersion: f.accepted.scenario.data.sourceBundleId,
        scrutinyAlgorithmId: f.accepted.scenario.algorithmId,
        seed: f.accepted.scenario.seed,
      },
    };
    expect(() =>
      createSimulationResultExport({
        scenario: file.scenario,
        result,
        exportedAt: '2026-10-29T12:00:00.000Z',
      }),
    ).toThrow('frozen fitted session');
    const archive = createSimulationResultExport({
      scenario: file.scenario,
      result,
      fitFile: file,
      exportedAt: '2026-10-29T12:00:00.000Z',
    });
    expect(archive.schema_version).toBe(3);
    expect(
      parseSimulationResultExport(JSON.stringify(archive)).result.tables,
    ).toEqual(result.tables);
    const mismatch = structuredClone(archive);
    mismatch.scenario.seed = 'different';
    expect(() =>
      parseSimulationResultExport(JSON.stringify(mismatch)),
    ).toThrow();
  });
  it('round trips exact encoded fitted arrays and generation without original history or fitting', async () => {
    const f = await loadedFitFixture(),
      file = await createCheckpoint(f.accepted),
      read = vi.fn(checkpointReader(file));
    const restored = await loadCandidate(file.pointer, read, {
      expectedGenerationImplementationId: f.implementation,
      allowEditedEvidence: true,
    });
    expect(restored.assets.fit.political.values).toEqual(
      f.accepted.assets.fit.political.values,
    );
    expect(
      read.mock.calls.every(([path]) => Object.hasOwn(file.assets, path)),
    ).toBe(true);
    const restore = await runFitTask(
      { action: 'restore', configuration: f.configuration, file },
      () => {
        throw new Error('Network not allowed');
      },
    );
    expect(restore.audit).toMatchObject({
      sourceLoaded: false,
      nationalFits: 0,
      profileFits: 0,
      pairedFits: 0,
    });
    const a = await prepareFastCompute(copyGenerationPacket(f.accepted)),
      b = await prepareFastCompute(copyGenerationPacket(restore.accepted!));
    expect(b.generate(2).input).toEqual(a.generate(2).input);
    expect(computeReformRun(b, 2).batch).toEqual(computeReformRun(a, 2).batch);
  });
  it('saves a stale evidence draft separately from its last accepted fit', async () => {
    const f = await loadedFitFixture(),
      draft = structuredClone(f.accepted.scenario);
    draft.referenceDate = '2026-09-01';
    const file = await createCheckpoint(f.accepted, draft),
      r = await runFitTask(
        { action: 'restore', configuration: f.configuration, file },
        f.read,
      );
    expect(file.scenario.referenceDate).toBe('2026-09-01');
    expect(r.accepted!.scenario.referenceDate).toBe(
      f.accepted.scenario.referenceDate,
    );
    expect(sameCheckpointScenario(file.scenario, r.accepted!.scenario)).toBe(
      false,
    );
  });
  it('rejects incomplete, corrupt and incompatible sessions before installation', async () => {
    const f = await loadedFitFixture(),
      file = await createCheckpoint(f.accepted),
      path = Object.keys(file.assets)[0];
    const missing = structuredClone(file);
    delete missing.assets[path];
    await expect(
      runFitTask(
        { action: 'restore', configuration: f.configuration, file: missing },
        f.read,
      ),
    ).rejects.toThrow();
    const corrupt = structuredClone(file);
    corrupt.assets[path][0] = 'AAAA';
    await expect(
      runFitTask(
        { action: 'restore', configuration: f.configuration, file: corrupt },
        f.read,
      ),
    ).rejects.toThrow();
    await expect(
      runFitTask(
        {
          action: 'restore',
          configuration: {
            ...f.configuration,
            implementations: {
              ...f.configuration.implementations,
              generation: 'unknown',
            },
          },
          file,
        },
        f.read,
      ),
    ).rejects.toThrow();
    expect(() =>
      requirePortableFitFile({ ...file, schemaVersion: 2 }),
    ).toThrow();
    expect(() =>
      requireFastScenario({ ...file.scenario, correspondences: [] }),
    ).toThrow();
  });
  it('keeps future edits separate from evidence and preserves blank versus zero references', async () => {
    const f = await loadedFitFixture(),
      s = labelFastDraft(f.accepted.scenario),
      signature = futureEvidenceSignature(s);
    let n = addFastList(s, 'Congiunta', ['a', 'b'], ['camera']);
    expect(futureEvidenceSignature(n)).toBe(signature);
    n = setFastReference(n, {
      scope: { kind: 'national' },
      channel: 'camera',
      subject: { kind: 'list', id: n.lists.camera.at(-1)!.id },
      percent: 0,
      sd: 0,
    });
    expect(n.references.at(-1)!.mean).toBe(0);
    n = setFastReference(n, {
      scope: { kind: 'national' },
      channel: 'camera',
      subject: n.references.at(-1)!.subject,
      percent: undefined,
      sd: 0,
    });
    expect(
      n.references.some((r) => r.subject.id === n.lists.camera.at(-1)!.id),
    ).toBe(false);
    const before = structuredClone(n.nonValidReferences);
    n = setFastReference(n, {
      scope: { kind: 'national' },
      channel: 'common',
      subject: { kind: 'list', id: n.lists.camera[0].id },
      percent: 20,
      sd: 2,
    });
    expect(n.nonValidReferences).toEqual(before);
    n = addReferencedList(n, 'Nuova', 1, ['camera', 'senato']);
    expect(validateFastScenario(n)).toEqual([]);
    expect(n.components.at(-1)).toMatchObject({
      kind: 'new',
      fallback: { geographicLogProfile: 0 },
    });
    const id = n.lists.camera.at(-1)!.id;
    n = removeFastList(n, 'camera', id);
    n = removeFastList(n, 'senato', id);
    expect(n.components).toHaveLength(s.components.length);
    expect(s.lists.camera.some((l) => l.name === 'Congiunta')).toBe(false);
  });
  it('moves whole components and invalidates stale frozen packets', async () => {
    const f = await loadedFitFixture(),
      s = labelFastDraft(f.accepted.scenario),
      original = s.lists.camera.find((l) => l.componentIds.includes('a'))!;
    let n = addFastList(s, 'Nuova lista', ['b'], ['camera']);
    n = moveFastComponent(
      n,
      'camera',
      n.lists.camera.at(-1)!.id,
      'a',
      true,
      true,
    );
    expect(
      n.lists.camera.find((l) => l.id === original.id)!.componentIds,
    ).not.toContain('a');
    const packet = copyGenerationPacket(f.accepted);
    packet.scenario.seed = 'edited-after-handoff';
    await expect(prepareFastCompute(packet)).rejects.toThrow(
      'Frozen run binding mismatch',
    );
  });
  it('can checkpoint a newly fitted poll while reusing the geographic states', async () => {
    const f = await originalFitFixture(),
      s = structuredClone(f.accepted.scenario);
    s.polls.added = [f.customPoll];
    s.polls.selected.push(f.customPoll.id);
    const fitted = await runFitTask(
      {
        action: 'fit',
        configuration: f.configuration,
        previous: f.accepted,
        scenario: s,
      },
      f.read,
    );
    expect(fitted.audit.nationalFits).toBe(1);
    // The fixture replaces its geographic states after loading; make their
    // portable representation faithful before testing the save boundary.
    const a = fitted.accepted!;
    a.portableStates.profiles.state.cameraProfile = a.assets.cameraProfile;
    a.portableStates.profiles.state.senateAdapter = a.assets.senateAdapter!;
    a.portableStates.profiles.state.parents.profileInputKey = a.profileInputKey;
    a.portableStates.profiles.state.parents.pairedInputKey = a.pairedInputKey;
    // Actual browser loading always installs matching IDs; the constructed
    // original-history fixture is exercised through the same codec below.
    const { encodeState } = await import('./state-codec'),
      { profileStateShapes } = await import('./state-shapes');
    const p = await encodeState(
      'fast-share-profiles',
      a.portableStates.profiles.state,
      a.portableStates.profiles.metadata,
      profileStateShapes(a.assets.cameraProfile),
    );
    a.assets.profileStateId = p.envelope.id;
    a.scenario.data.profileStateId = p.envelope.id;
    const { generationKeys } = await import('./release');
    a.keys = await generationKeys(a.scenario, a.context, a.electorateKey);
    const file = await createCheckpoint(a),
      restored = await loadCandidate(file.pointer, checkpointReader(file), {
        allowEditedEvidence: true,
        expectedGenerationImplementationId:
          f.configuration.implementations.generation,
      });
    expect(restored.scenario.polls.added[0].id).toBe(f.customPoll.id);
    expect(restored.assets.fit.political.values).toEqual(
      a.assets.fit.political.values,
    );
  });
});
