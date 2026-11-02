import { describe, it, expect } from 'vitest';
import {
  correspondenceRow,
  equalComposition,
  normalizedComposition,
} from './correspondences';
import { latestLegacyPair, compileLegacy } from './legacy-adapter';
import { prepareScenarioGeneration } from './unified-generation';
import { runFitTask } from './fit-service';
import { createCheckpoint, checkpointReader } from './checkpoint';
import { loadCandidate } from './release';
import { originalFitFixture } from '../../../../../tests/fixtures/fast-fit-input';
import { generationFixture } from '../generation.fixture';
import { preparePairedVoteReferences } from '../paired-vote-generation';
import { latestLegacyPoints, refreshLegacyPolls, type LegacyPollObservation } from './legacy-polls';
// Frozen output of the original comparison adapter at cd69edd; no research import.
import comparedPolls from '../../../../../tests/fixtures/legacy-poll-comparison.json';

describe('unified generation contract', () => {
  it('normalizes source composition, aggregates binary future membership and retains structural members at zero weight', () => {
    const composition = {
        fractions: { a: 60, b: 40 },
        provenance: 'assumption',
      },
      lists = [
        { id: 'joint', componentIds: ['a', 'c'] },
        { id: 'b', componentIds: ['b'] },
      ];
    expect(correspondenceRow(['a', 'b'], composition, lists)).toEqual([
      { futureListId: 'joint', factor: 0.6 },
      { futureListId: 'b', factor: 0.4 },
    ]);
    expect(equalComposition(['a', 'b']).fractions).toEqual({ a: 0.5, b: 0.5 });
    expect(
      normalizedComposition(['a', 'b'], {
        fractions: { a: 0, b: 1 },
        provenance: 'zero assumed contribution',
      }),
    ).toHaveLength(2);
    expect(() =>
      normalizedComposition(['a'], { fractions: { a: 0 }, provenance: 'zero' }),
    ).toThrow();
  });
  it('conditions mapped and unfiled political support without changing turnout; archives retain old policy', () => {
    const { scenario, snapshot } = generationFixture();
    scenario.correspondences = scenario.correspondences.filter(
      (c) => c.pastListId === 'A',
    );
    const old = preparePairedVoteReferences(scenario, snapshot);
    scenario.projectionPolicy = 'preserve-historical-turnout-v1';
    const aligned = preparePairedVoteReferences(scenario, snapshot);
    expect(old.units[0].camera.abstention).toBeCloseTo(0.73);
    expect(aligned.units[0].camera.abstention).toBeCloseTo(0.4);
    scenario.correspondences = [];
    const unmapped = preparePairedVoteReferences(scenario, snapshot);
    expect(unmapped.units[0].camera.abstention).toBeCloseTo(0.4);
    expect(
      [...unmapped.units[0].camera.shares.values()].reduce((a, b) => a + b, 0),
    ).toBeCloseTo(1);
  });
  it('ports exactly the compared latest-publication poll calculation, including mixed-wave normalization', () => {
    const { scenario, snapshot } = generationFixture(),
      b = structuredClone(scenario);
    const lists = scenario.lists.camera.map((l) => ({
      id: l.id,
      members: [l.id],
    }));
    const groups = lists.map((l) => [l.id]),
      shares = [0.6, 0.404];
    const raw = {
      polls: [
        {
          id: 'latest',
          kind: 'camera',
          date: '2026-10-18',
          end: '2026-10-18',
          available: '2026-10-19',
          groups,
          shares,
          sample: 300,
          source: 'test',
        },
      ],
      supplementalPolls: [],
    };
    const polls: LegacyPollObservation[] = raw.polls.map((p) => ({
      ...p,
      context: 'camera',
      enabled: true,
      groups: p.groups.map((members, i) => ({
        members,
        label: members.join('+'),
        share: p.shares[i],
      })),
    }));
    refreshLegacyPolls(
      b,
      snapshot,
      polls,
      '2026-10-19',
      lists.map((l) => ({ id: l.id, componentIds: l.members })),
      {},
      (id) => id,
    );
    expect(b.pollReferences!.rows).toEqual(comparedPolls.rows);
    expect(b.projectionDate).toBe(comparedPolls.projectionDate);
  });
  it('projects one poll independently to each chamber ticket and rejects duplicate choices or invented poll identities', () => {
    const {scenario,snapshot}=generationFixture(),
      ids=scenario.lists.camera.map(l=>l.id),
      a=ids[0],b=ids[1],
      lists=scenario.lists.camera.map(l=>({id:l.id,componentIds:[l.id]})),
      p:LegacyPollObservation={id:'scoped',date:'2026-10-18',available:'2026-10-19',end:'2026-10-18',context:'camera',sample:1000,source:'test',enabled:true,
        groups:[{label:'alpha',members:[a],share:0.7},{label:'beta',members:[b],share:0.3}]};
    refreshLegacyPolls(scenario,snapshot,[p],'2026-10-19',lists,{},id=>id,{
      camera:lists,senato:lists.map(l=>({...l,componentIds:[l.id===a?b:l.id===b?a:l.id]}))});
    const share=(ch:string,id:string)=>scenario.pollReferences!.rows.find(r=>r.chamber===ch&&r.listId===id)!.share;
    expect(share('camera',a)).toBeCloseTo(0.7); expect(share('senato',a)).toBeCloseTo(0.3);
    expect(share('camera',b)).toBeCloseTo(0.3); expect(share('senato',b)).toBeCloseTo(0.7);
    const duplicate=structuredClone(p);duplicate.groups[1].members=[a];
    expect(()=>latestLegacyPoints([duplicate],'2026-10-19',lists,{},id=>id)).toThrow(/multiple poll choices/);
    expect(()=>latestLegacyPoints([p],'2026-10-19',lists,{'poll:scoped:alpha':{fractions:{[b]:1},proxy:true,provenance:'invented'}},id=>id)).toThrow(/unmeasured identity/);
    expect(normalizedComposition(['a','b'],{fractions:{a:1e308,b:1e308},provenance:'large finite relative weights'}).map(r=>r.fraction)).toEqual([0.5,0.5]);
  });
  it('prepares legacy without candidate refitting, freezes it for offline replay, and refits candidate after evidence changes', async () => {
    const f = await originalFitFixture(),
      draft = structuredClone(f.accepted.scenario);
    // The original-count fixture replaces arrays without serializing them;
    // install a real fitted checkpoint before checking portable legacy replay.
    draft.referenceDate = '2026-10-20';
    const fitted = await runFitTask(
        {
          action: 'fit',
          configuration: f.configuration,
          previous: f.accepted,
          scenario: draft,
        },
        f.read,
      ),
      s = structuredClone(fitted.accepted!.scenario);
    s.schemaVersion = 5;
    s.generation = {
      model: 'M01', modelVersion: "2.0.0",
      compositions: {},
      supplementalPolls: [],
    };
    expect(latestLegacyPair(f.source, s.referenceDate).senato.id).toBe('s22');
    expect(() => latestLegacyPair(f.source, '2000-01-01')).toThrow(
      /No eligible/,
    );
    const prepared = await runFitTask(
      {
        action: 'validate',
        configuration: f.configuration,
        previous: fitted.accepted!,
        scenario: s,
      },
      f.read,
    );
    expect(prepared.audit.nationalFits).toBe(0);
    expect(prepared.accepted!.legacy!.audit.baseline.camera).toBe('c22');
    const accepted = prepared.accepted!,
      first = prepareScenarioGeneration(
        s,
        accepted.assets,
        accepted.context,
        accepted.legacy,
      ).run(2);
    const overlapping = structuredClone(s);
    overlapping.lists.camera[1].componentIds.push(
      overlapping.lists.camera[0].componentIds[0],
    );
    expect(() =>
      prepareScenarioGeneration(
        overlapping,
        accepted.assets,
        accepted.context,
        accepted.legacy,
      ),
    ).toThrow('multiple future choices');
    const file = await createCheckpoint(accepted),
      restored = await loadCandidate(file.pointer, checkpointReader(file), {
        allowEditedEvidence: true,
      });
    expect(
      prepareScenarioGeneration(
        restored.scenario,
        restored.assets,
        restored.context,
        restored.legacy,
      ).run(2),
    ).toEqual(first);
    const edited = structuredClone(s);
    edited.referenceDate = '2026-10-21';
    const recompiled = await runFitTask(
      {
        action: 'validate',
        configuration: f.configuration,
        previous: accepted,
        scenario: edited,
      },
      f.read,
    );
    expect(recompiled.audit.nationalFits).toBe(0);
    edited.generation!.model = 'M02';
    const stale = await runFitTask(
      {
        action: 'validate',
        configuration: f.configuration,
        previous: recompiled.accepted!,
        scenario: edited,
      },
      f.read,
    );
    expect(stale.accepted).toBeUndefined();
    expect(stale.plan.national).toBe(true);
  });
});
