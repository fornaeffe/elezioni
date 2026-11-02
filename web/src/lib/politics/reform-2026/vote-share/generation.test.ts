import { describe, it, expect } from 'vitest';
import { fastGenerationFixture } from '../../../../../tests/fixtures/fast-generation-input';
import { prepareFastGeneration } from './generation';
import { prepareSenateAdapter } from './senate-adapter';
import { validateReformScrutinyInput } from '../validation';
import { runPoliticsReformScrutiny } from '../scrutiny';
import { OTHER_LIST_ID, OTHER_LIST_NAME } from '../../../core/other-list';
describe('FV-04 paired physical-cell reform adapter', () => {
  it('rejects broken portable fit states before generation', () => {
    const f = fastGenerationFixture(),
      broken = structuredClone(f.assets);
    broken.fit.political.values[0] += 0.1;
    expect(() => prepareFastGeneration(f.scenario, broken, f.context)).toThrow('broken simplex');
    const failed = structuredClone(f.assets);
    failed.fit.political.diagnostics.coordinates[0].bulkEss = 0;
    expect(() => prepareFastGeneration(f.scenario, failed, f.context)).toThrow('coordinate diagnostics');
  });
  it('runs fully specified uncertain references and traces deterministic centre normalization separately', () => {
    const f = fastGenerationFixture();
    f.scenario.references = f.scenario.lists.camera.map((l, i) => ({
      id: 'r' + i,
      scope: { kind: 'national' },
      channel: 'camera',
      subject: { kind: 'list', id: l.id },
      mean: i ? 0.6 : 0.4,
      sdPercentagePoints: 2
    }));
    const sampled = prepareFastGeneration(f.scenario, f.assets, f.context).run(3);
    expect(sampled.audit.referenceTargets[0].groups.reduce((n, g) => n + g.share, 0)).toBeCloseTo(1, 12);
    f.scenario.references = f.scenario.references.map((r, i) => ({
      ...r,
      mean: i ? 0.3 : 0.2,
      sdPercentagePoints: 0
    }));
    f.scenario.nonValidReferences = [
      { id: 'turnout', scope: { kind: 'national' }, channel: 'camera', mean: 0.35, sdPercentagePoints: 1 }
    ];
    const normalized = prepareFastGeneration(f.scenario, f.assets, f.context).run(3);
    expect(normalized.trace.normalization).toContainEqual({
      scope: { kind: 'national' },
      chamber: 'camera',
      reason: 'camera reference centres before joint calibration',
      before: 0.5,
      after: 1
    });
    expect(
      normalized.trace.warnings.find((w) => w.code === 'NORMALIZED_REFERENCE_CENTRES')!.message
    ).toContain('"after":{"mean":0.4');
    expect(normalized.audit.countryReference.camera![0]).toBeCloseTo(
      normalized.audit.referenceTargets[0].nonValid!,
      8
    );
  });
  it('generates complete legal paired inputs including VdA/TAA, without a second TAA ballot', () => {
    const { scenario, assets, context } = fastGenerationFixture(),
      prepared = prepareFastGeneration(scenario, assets, context),
      result = prepared.run(1);
    expect(validateReformScrutinyInput(result.input).valid).toBe(true);
    expect(result.audit.projectionResidual).toBeLessThanOrEqual(1e-9);
    const chambers =
      result.input.renewal.kind === 'joint' ? [result.input.renewal.camera, result.input.renewal.senato] : [];
    for (const ch of chambers) {
      const rows = [
        ...ch.ordinaryVotes.filter(
          (r) =>
            context.seats.geography.colleges.find((c) => c.id === r.collegeId)!.kind !== 'taa-proportional'
        ),
        ...ch.specialVotes
      ];
      expect(rows.reduce((s, r) => s + r.electorate.electors!, 0)).toBe(100000);
      for (const row of rows) expect(row.votes.kind).toBe('attributed');
    }
    // Tiny constructed electorate can hit the law's truncated-quota failure.
    // Keep that failure, including its original simulation ID; never reroll it.
    const checked = runPoliticsReformScrutiny(result.input);
    expect(checked.stages.find((s) => s.norm === 'N13')).toMatchObject({
      status: 'non-computable',
      reason: 'initial-overallocation'
    });
    assets.electorates.forEach((e) => {
      e.camera *= 1000;
      e.senato *= 1000;
    });
    const full = prepareFastGeneration(scenario, assets, context).run(1);
    expect(runPoliticsReformScrutiny(full.input).stages.some((s) => s.status === 'non-computable')).toBe(
      false
    );
  });
  it('is deterministic across row order, display names, simulation order and geographic chunks', () => {
    const f = fastGenerationFixture(),
      a = prepareFastGeneration(f.scenario, f.assets, f.context, { chunkSize: 1 }).run(7);
    f.scenario.lists.camera.reverse();
    f.scenario.lists.senato.reverse();
    f.scenario.components.reverse();
    f.assets.geography.cells.reverse();
    f.scenario.lists.camera = f.scenario.lists.camera.map((l) => ({ ...l, name: l.name + ' renamed' }));
    const p = prepareFastGeneration(f.scenario, f.assets, f.context, { chunkSize: 7 });
    p.run(2);
    const b = p.run(7);
    const counts = (r: typeof a) =>
      r.input.renewal.kind === 'joint'
        ? [r.input.renewal.camera, r.input.renewal.senato].flatMap((c) =>
            [...c.ordinaryVotes, ...c.specialVotes].map((r) => ({
              college: r.collegeId,
              electors: r.electorate.electors,
              votes:
                r.votes.kind === 'attributed'
                  ? r.votes.listVotes.slice().sort((a, b) => a.listId.localeCompare(b.listId))
                  : []
            }))
          )
        : [];
    expect(counts(b)).toEqual(counts(a));
    expect(b.audit.countryForecast).toEqual(a.audit.countryForecast);
  });
  it('applies national then local references with fixed turnout and records effective reference margins', () => {
    const f = fastGenerationFixture();
    f.scenario.references = [
      {
        id: 'national-a',
        scope: { kind: 'national' },
        channel: 'camera',
        subject: { kind: 'list', id: f.scenario.lists.camera[0].id },
        mean: 0.4,
        sdPercentagePoints: 0
      },
      {
        id: 'local-a',
        scope: { kind: 'region', code: '01' },
        channel: 'camera',
        subject: { kind: 'list', id: f.scenario.lists.camera[0].id },
        mean: 0.8,
        sdPercentagePoints: 0
      }
    ];
    const base = prepareFastGeneration({ ...f.scenario, references: [] }, f.assets, f.context),
      p = prepareFastGeneration(f.scenario, f.assets, f.context);
    p.centralReference.camera.forEach((r, i) => expect(r[0]).toBe(base.centralReference.camera[i][0]));
    const group = [1, 2];
    let numerator = 0,
      denominator = 0;
    p.cells.camera.forEach((c, i) => {
      const r = p.centralReference.camera[i];
      numerator += c.electors * (r[group[0]] + r[group[1]]);
      denominator += c.electors * (1 - r[0]);
    });
    expect(numerator / denominator).toBeCloseTo(0.42, 8);
    expect(p.run(3).audit.referenceTargets.filter((r) => r.chamber === 'camera')).toHaveLength(2);
  });
  it('supports an explicitly referenced new list with labelled fallback, while unfiled support stays zero', () => {
    const f = fastGenerationFixture();
    f.scenario.components.push({
      id: 'new-n',
      kind: 'new',
      provenance: 'User new party',
      fallback: {
        geographicLogProfile: 0,
        profileSd: 0.45,
        localInnovationSd: 0.25,
        nationalInnovationSd: 0.35
      }
    });
    for (const ch of ['camera', 'senato'] as const) {
      f.scenario.lists[ch].push({
        id: 'N',
        name: 'New party',
        coalitionId: null,
        protectedMinority: null,
        kind: 'list',
        componentIds: ['new-n']
      });
      f.scenario.coverage.filter((c) => c.chamber === ch).forEach((c) => c.listIds.push('N'));
    }
    f.scenario.references.push({
      id: 'new-reference',
      scope: { kind: 'national' },
      channel: 'common',
      subject: { kind: 'component', id: 'new-n' },
      mean: 0.1,
      sdPercentagePoints: 1
    });
    const p = prepareFastGeneration(f.scenario, f.assets, f.context),
      run = p.run(2);
    expect(validateReformScrutinyInput(run.input).valid).toBe(true);
    expect(p.componentIds).toContain('new-n');
    f.scenario.references = [];
    expect(() => prepareFastGeneration(f.scenario, f.assets, f.context)).toThrow(
      'NEW_COMPONENT_REFERENCE_REQUIRED'
    );
  });
  it('rejects stale fits, overlapping filing, 25+ Senate history and impossible physical capacities', () => {
    const f = fastGenerationFixture();
    expect(() =>
      prepareFastGeneration({ ...f.scenario, referenceDate: '2026-10-20' }, f.assets, f.context)
    ).toThrow('FIT_STALE');
    const bad = structuredClone(f.scenario);
    bad.lists.camera[1].componentIds.push('a');
    expect(() => prepareFastGeneration(bad, f.assets, f.context)).toThrow('COMPONENT_OVERLAP');
    expect(() =>
      prepareSenateAdapter(f.assets.cameraProfile, f.scenario.referenceDate, {
        date: '2018-03-04',
        availableDate: '2018-03-05',
        minimumAge: 25,
        sourceVersions: ['constructed'],
        rows: f.pairedRows
      })
    ).toThrow('18+');
    f.scenario.coverage
      .filter(
        (c) =>
          c.chamber === 'camera' &&
          c.collegeId !== f.scenario.coverage.find((c) => c.chamber === 'camera')!.collegeId
      )
      .forEach((c) => (c.listIds = c.listIds.slice(1)));
    f.scenario.references = [
      {
        id: 'capacity',
        scope: { kind: 'national' },
        channel: 'camera',
        subject: { kind: 'list', id: f.scenario.lists.camera[0].id },
        mean: 0.9,
        sdPercentagePoints: 0
      }
    ];
    expect(() => prepareFastGeneration(f.scenario, f.assets, f.context)).toThrow('CAPACITY');
  });
  it('uses common anchors, explicit chamber precedence and single-chamber renewals', () => {
    const f = fastGenerationFixture(),
      list = f.scenario.lists.camera[0].id;
    f.scenario.references = [
      {
        id: 'common',
        scope: { kind: 'national' },
        channel: 'common',
        subject: { kind: 'list', id: list },
        mean: 0.4,
        sdPercentagePoints: 2
      },
      {
        id: 'camera',
        scope: { kind: 'national' },
        channel: 'camera',
        subject: { kind: 'list', id: list },
        mean: 0.3,
        sdPercentagePoints: 0
      }
    ];
    const p = prepareFastGeneration(f.scenario, f.assets, f.context),
      r = p.run(4);
    expect(r.audit.referenceTargets.find((t) => t.chamber === 'camera')!.groups[0].share).toBe(0.3);
    expect(r.audit.referenceTargets.find((t) => t.chamber === 'senato')!.groups[0].share).not.toBe(0.3);
    expect(r.trace.references.find((t) => t.listIds.camera === list)!.camera!.share).toBeCloseTo(0.3, 8);
    f.scenario.renewal = 'camera';
    f.scenario.references = f.scenario.references.filter((r) => r.channel === 'common');
    const single = prepareFastGeneration(f.scenario, f.assets, f.context).run(4);
    expect(single.input.renewal.kind).toBe('single');
    expect(single.audit.countryForecast.senato).toBeUndefined();
    expect(single.trace.references.every((r) => r.senato === null)).toBe(true);
  });
  it('distinguishes list identities sharing components in disjoint physical colleges', () => {
    const f = fastGenerationFixture(),
      a = f.scenario.lists.camera[0];
    f.scenario.renewal = 'camera';
    f.scenario.lists.camera.push({ ...a, id: 'regional-a', name: 'Regional A' });
    f.scenario.coverage
      .filter((c) => c.chamber === 'camera')
      .forEach((c) => {
        const territory = f.context.seats.geography.colleges.find((g) => g.id === c.collegeId)!.territoryId;
        if (Number(territory.slice(-2)) % 2)
          c.listIds = c.listIds.map((id) => (id === a.id ? 'regional-a' : id));
      });
    f.scenario.references = [
      {
        id: 'a1',
        scope: { kind: 'national' },
        channel: 'camera',
        subject: { kind: 'list', id: a.id },
        mean: 0.2,
        sdPercentagePoints: 0
      },
      {
        id: 'a2',
        scope: { kind: 'national' },
        channel: 'camera',
        subject: { kind: 'list', id: 'regional-a' },
        mean: 0.2,
        sdPercentagePoints: 0
      }
    ];
    const p = prepareFastGeneration(f.scenario, f.assets, f.context),
      r = p.run(2);
    expect(r.audit.referenceTargets[0].groups).toHaveLength(2);
    for (const t of r.audit.referenceTargets[0].groups) expect(t.share).toBe(0.2);
  });
  it('keeps transition zero-horizon identity separate from profile uncertainty and election discrepancy', () => {
    const f = fastGenerationFixture();
    f.scenario.electionDate = f.scenario.referenceDate;
    f.scenario.references = [
      {
        id: 'a',
        scope: { kind: 'national' },
        channel: 'camera',
        subject: { kind: 'list', id: f.scenario.lists.camera[0].id },
        mean: 0.4,
        sdPercentagePoints: 0
      }
    ];
    const p = prepareFastGeneration(f.scenario, f.assets, f.context),
      r = p.run(8);
    expect(
      r.audit.countryReference.camera!.slice(1, 3).reduce((a, b) => a + b, 0) /
        (1 - r.audit.countryReference.camera![0])
    ).toBeCloseTo(0.4, 8);
    expect(r.audit.countryForecast.camera![0]).toBe(r.audit.countryReference.camera![0]);
    expect(r.audit.countryForecast.camera).not.toEqual(r.audit.countryReference.camera);
    f.assets.fit.audit.electionDiscrepancy = 0;
    const zero = prepareFastGeneration(f.scenario, f.assets, f.context).run(8);
    zero.audit.countryForecast.camera!.forEach((x, j) =>
      expect(x).toBeCloseTo(zero.audit.countryReference.camera![j], 12)
    );
  });
  it('regroups the same component counts exactly when separate lists become joint lists', () => {
    const f = fastGenerationFixture(),
      joint = prepareFastGeneration(f.scenario, f.assets, f.context).run(6);
    f.scenario.bonusLinks = [];
    for (const ch of ['camera', 'senato'] as const) {
      const a = f.scenario.lists[ch][0];
      f.scenario.lists[ch] = f.scenario.components.map((c) => ({
        ...a,
        id: 'list-' + c.id,
        name: c.id,
        componentIds: [c.id]
      }));
      f.scenario.coverage
        .filter((c) => c.chamber === ch)
        .forEach((c) => (c.listIds = f.scenario.lists[ch].map((l) => l.id)));
    }
    const separate = prepareFastGeneration(f.scenario, f.assets, f.context).run(6);
    if (joint.input.renewal.kind !== 'joint' || separate.input.renewal.kind !== 'joint') throw new Error();
    for (const ch of ['camera', 'senato'] as const) {
      const j = joint.input.renewal[ch],
        s = separate.input.renewal[ch];
      for (const row of [...j.ordinaryVotes, ...j.specialVotes]) {
        const other = [...s.ordinaryVotes, ...s.specialVotes].find((x) => x.collegeId === row.collegeId)!;
        if (row.votes.kind !== 'attributed' || other.votes.kind !== 'attributed') throw new Error();
        const votes = new Map(other.votes.listVotes.map((r) => [r.listId, r.votes]));
        expect(row.votes.listVotes[0].votes).toBe(votes.get('list-a')! + votes.get('list-b')!);
        expect(row.votes.listVotes[1].votes).toBe(votes.get('list-c')! + votes.get('list-d')!);
      }
    }
  });
  it('preserves supplied vote compositions, preference budgets and people through shared assembly', () => {
    const f = fastGenerationFixture(),
      original = prepareFastGeneration(f.scenario, f.assets, f.context).run(1).input;
    if (original.renewal.kind !== 'joint') throw new Error();
    const row = original.renewal.camera.ordinaryVotes[0];
    if (row.votes.kind !== 'attributed') throw new Error();
    const direct = row.votes.listVotes.map((r) => ({ ...r, votes: Math.floor(r.votes / 2) }));
    f.scenario.suppliedVotes = [
      {
        chamber: 'camera',
        collegeId: row.collegeId,
        votes: {
          kind: 'components',
          directListVotes: direct,
          bonusOnlyVotes: row.votes.listVotes.map((r, i) => ({
            subject: { kind: 'list', id: r.listId },
            votes: r.votes - direct[i].votes
          })),
          provenance: { kind: 'supplied', reference: 'Reviewed explicit composition' }
        }
      }
    ];
    const slate = original.renewal.camera.ordinarySlates.find((s) => s.collegeId === row.collegeId)!;
    const person = original.candidates.find((c) => c.id === slate.candidateIds[0])!;
    f.scenario.candidateTemplates.push({
      chamber: 'camera',
      role: 'ordinary',
      locationId: row.collegeId,
      ownerId: slate.listId,
      affiliationListId: slate.listId,
      position: 1,
      candidate: { ...person, id: 'supplied-person', name: 'Supplied person' }
    });
    const result = prepareFastGeneration(f.scenario, f.assets, f.context).run(1).input;
    if (result.renewal.kind !== 'joint') throw new Error();
    expect(result.candidates.find((c) => c.id === 'supplied-person')?.name).toBe('Supplied person');
    expect(
      result.renewal.camera.ordinarySlates.find(
        (s) => s.collegeId === row.collegeId && s.listId === slate.listId
      )!.candidateIds[0]
    ).toBe('supplied-person');
    expect(result.renewal.camera.ordinaryVotes.find((r) => r.collegeId === row.collegeId)!.votes).toEqual(
      f.scenario.suppliedVotes[0].votes
    );
    for (const s of result.renewal.camera.ordinarySlates.filter((s) => s.collegeId === row.collegeId)) {
      if (s.preferences.kind !== 'counts' || s.preferences.validity.kind !== 'combinations')
        throw new Error();
      expect(s.preferences.validity.ballots.reduce((n, b) => n + b.count, 0)).toBe(
        direct.find((d) => d.listId === s.listId)!.votes
      );
    }
    f.scenario.suppliedPreferences.push({
      chamber: 'camera',
      collegeId: row.collegeId,
      listId: slate.listId,
      preferences: { kind: 'missing', reason: 'Explicitly unavailable' }
    });
    const missing = prepareFastGeneration(f.scenario, f.assets, f.context).run(1).input;
    if (missing.renewal.kind !== 'joint') throw new Error();
    expect(
      missing.renewal.camera.ordinarySlates.find(
        (s) => s.collegeId === row.collegeId && s.listId === slate.listId
      )!.preferences
    ).toEqual({ kind: 'missing', reason: 'Explicitly unavailable' });
  });
  it('uses Camera coordinates for Senate and makes zero/doubled historical gaps explicit', () => {
    const f = fastGenerationFixture(),
      centralId = f.scenario.chamberProfileId,
      p = f.context.models.chambers.profiles.find((p) => p.id === centralId)!;
    p.gapMultiplier = 0;
    const zero = prepareFastGeneration(f.scenario, f.assets, f.context);
    zero.centralReference.camera[0].forEach((x, j) =>
      expect(x).toBeCloseTo(zero.centralReference.senato[0][j], 12)
    );
    const original = zero.run(3).audit.countryForecast;
    const K = f.assets.fit.componentIds.length,
      values = f.assets.fit.political.values;
    for (let r = 0; r < values.length; r += 2 * K) for (let j = K; j < 2 * K; j++) values[r + j] = 1 / K;
    expect(prepareFastGeneration(f.scenario, f.assets, f.context).run(3).audit.countryForecast).toEqual(
      original
    );
    p.gapMultiplier = 2;
    const doubled = prepareFastGeneration(f.scenario, f.assets, f.context);
    expect(doubled.centralReference.senato).not.toEqual(doubled.centralReference.camera);
  });
  it('retains Other as valid residual support without fabricating its candidates', () => {
    const f = fastGenerationFixture(),
      old = f.scenario.lists.camera[1].id;
    for (const ch of ['camera', 'senato'] as const) {
      f.scenario.lists[ch][1] = {
        ...f.scenario.lists[ch][1],
        id: OTHER_LIST_ID,
        name: OTHER_LIST_NAME,
        kind: 'other'
      };
      f.scenario.coverage
        .filter((c) => c.chamber === ch)
        .forEach((c) => (c.listIds = c.listIds.map((id) => (id === old ? OTHER_LIST_ID : id))));
    }
    f.scenario.bonusLinks = f.scenario.bonusLinks.filter((l) => l.camera.id !== old && l.senato.id !== old);
    const result = prepareFastGeneration(f.scenario, f.assets, f.context).run(1).input;
    if (result.renewal.kind !== 'joint') throw new Error();
    for (const ch of ['camera', 'senato'] as const) {
      const c = result.renewal[ch];
      expect(c.otherListCoverage?.length).toBeGreaterThan(0);
      expect(c.ordinarySlates.some((s) => s.listId === OTHER_LIST_ID)).toBe(false);
      expect(
        c.ordinaryVotes.some(
          (r) =>
            r.votes.kind === 'attributed' &&
            r.votes.listVotes.some((v) => v.listId === OTHER_LIST_ID && v.votes > 0)
        )
      ).toBe(true);
    }
  });
});
