import { describe, expect, it } from 'vitest';
import { runPoliticsReformScrutiny, ReformScrutinyInputError } from './scrutiny';
import { completeScrutinyFixture, coalitionScrutinyFixture } from './scrutiny.fixture';
import { validateReformScrutinyInput } from './validation';

describe('R26-18 complete direct-input composition', () => {
  it.each([
    [45, false, 'no-bonus'], [45, true, 'bonus-without-cap'], [60, true, 'bonus-with-cap']
  ] as const)('finishes supplied preferences through both chambers (%s, linked=%s)', (share, linked, path) => {
    const input = completeScrutinyFixture(share, linked), before = structuredClone(input);
    expect(validateReformScrutinyInput(input).issues).toEqual([]);
    const r = runPoliticsReformScrutiny(input);
    expect(r.status).toBe('complete');
    expect(r.seats).toHaveLength(588);
    expect(r.chambers[0].national).toMatchObject({ status: 'complete', path });
    expect(r.chambers[1].regional).toMatchObject({ status: 'complete', path });
    expect(r.stages.find(s => s.chamber === 'senato' && s.norm === 'N22')).toEqual(path === 'bonus-with-cap'
      ? { norm: 'N22', chamber: 'senato', status: 'complete' }
      : { norm: 'N22', chamber: 'senato', status: 'not-run', reason: 'alternative-allocation-path', affectedIds: [] });
    // Independent quota arithmetic: 384/314 Camera ordinary and 189/154 Senate
    // ordinary. TAA adds A/B/C=1/1/1 at 45%, 2/1/0 at 60%; all uni wins A.
    // Senate's cap gives B the first unused remainder, then regional-vote priority.
    const expected = !linked ? [[179, 149, 64], [92, 73, 31]] : share === 45 ? [[217, 122, 53], [111, 59, 26]] : [[220, 121, 51], [113, 64, 19]];
    for (const [i, side] of (['camera', 'senato'] as const).entries()) expect(['A', 'B', 'C'].map(id =>
      r.seats.filter(s => s.entitlement.chamber === side && s.entitlement.subject.id === id).length)).toEqual(expected[i]);
    expect(new Set(r.seats.flatMap(s => s.status === 'filled' ? [s.candidateId] : [])).size).toBe(588);
    for (const [side, count, special, bonus] of [['camera', 392, 8, 70], ['senato', 196, 7, 35]] as const) {
      const seats = r.seats.filter(s => s.entitlement.chamber === side);
      expect(seats).toHaveLength(count);
      expect(seats.filter(s => s.category === 'special-uninominal' || s.category === 'taa-proportional')).toHaveLength(special);
      expect(seats.filter(s => s.category === 'bonus')).toHaveLength(linked ? bonus : 0);
      if (path === 'bonus-with-cap') expect(seats.filter(s => s.entitlement.subject.id === 'A')).toHaveLength(side === 'camera' ? 220 : 113);
    }
    expect(r.chambers.every(s => s.recovery?.terminalReason === 'all-filled')).toBe(true);
    if (linked) expect(r.recoveries.some(t => t.outcome === 'filled')).toBe(true);
    expect(r.draws.length).toBeGreaterThan(0);
    expect(new Set(r.draws.map(d => d.contextId)).size).toBe(r.draws.length);
    expect(r.warnings.filter(w => w.interpretationId === 'I03')).toHaveLength(r.draws.filter(d => d.rule === 'I03').length);
    expect(input).toEqual(before);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
    expect(runPoliticsReformScrutiny(input)).toEqual(r);
  }, 30000);
  it('validates the entire paired envelope before consuming an injected draw', () => {
    const input = completeScrutinyFixture(); input.simulationId = -1;
    expect(() => runPoliticsReformScrutiny(input, { draw() { throw new Error('Must not draw'); } })).toThrow(ReformScrutinyInputError);
  });
  it('rejects duplicated TAA proportional views before choosing a stage communication', () => {
    const input = completeScrutinyFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    const c = input.renewal.camera, first = c.colleges.find(p => p.id === 'TP')!;
    first.seatsWithBonus = first.seatsWithoutBonus = 1;
    c.colleges.push({ ...first, id: 'TP2', seatsWithBonus: 2, seatsWithoutBonus: 2 });
    c.ordinarySlates.push(...c.ordinarySlates.filter(s => s.collegeId === 'TP').map(s => ({ ...s, collegeId: 'TP2' })));
    c.ordinaryVotes.push({ ...c.ordinaryVotes.find(r => r.collegeId === 'TP')!, collegeId: 'TP2' });
    expect(validateReformScrutinyInput(input).issues).toContainEqual(expect.objectContaining({ code: 'INVALID_ENDOWMENT', lawReference: 'C93-septies(2)' }));
    expect(() => runPoliticsReformScrutiny(input, { draw() { throw new Error('Must not draw'); } })).toThrow(/exactly one TAA/);
  });
  it('retains shortages and missing preferences as partial candidate results', () => {
    const missing = completeScrutinyFixture();
    if (missing.renewal.kind !== 'joint') throw new Error();
    for (const c of [missing.renewal.camera, missing.renewal.senato]) for (const s of c.ordinarySlates) s.preferences = { kind: 'missing', reason: 'not supplied' };
    const r = runPoliticsReformScrutiny(missing);
    expect(r.status).toBe('partial'); expect(r.seats).toHaveLength(588);
    expect(r.seats.some(s => s.status === 'unresolved' && s.reason.includes('preferences'))).toBe(true);
    const exhausted = completeScrutinyFixture();
    if (exhausted.renewal.kind !== 'joint') throw new Error();
    const ordinaryIds = new Set([exhausted.renewal.camera, exhausted.renewal.senato].flatMap(c => c.ordinarySlates.flatMap(s => s.candidateIds)));
    for (const p of exhausted.candidates) if (ordinaryIds.has(p.id)) p.availability = { kind: 'unavailable', reason: 'after-filing unavailability' };
    const e = runPoliticsReformScrutiny(exhausted);
    expect(e.status).toBe('partial'); expect(e.seats).toHaveLength(588);
    expect(e.seats.some(s => s.status === 'unfilled')).toBe(true);
  });
  it.each(['camera', 'senato'] as const)('executes a single %s renewal without inventing paired bonus data', side => {
    const input = completeScrutinyFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    const chamber = input.renewal[side]; chamber.bonusSlates = [];
    input.renewal = { kind: 'single', chamber }; input.bonusLinks = [];
    const r = runPoliticsReformScrutiny(input);
    expect(r.status).toBe('complete'); expect(r.renewedChambers).toEqual([side]);
    expect(r.seats).toHaveLength(side === 'camera' ? 392 : 196);
    expect(r.bonus).toMatchObject({ status: 'not-awarded', reasons: [{ code: 'single-chamber-renewal', chamber: side }] });
  });
  it('owns simulation-specific draw streams and accepts deterministic injected draws', () => {
    const input = completeScrutinyFixture();
    const custom = () => ({ draw: (request: import('./result').DrawRequest) => ({ ...request, chosenId: request.alternativeIds[0], streamId: 'fixture-legal' }) });
    const a = runPoliticsReformScrutiny(input, custom());
    expect(runPoliticsReformScrutiny(input, custom())).toEqual(a);
    expect(a.draws.every(d => d.streamId === 'fixture-legal')).toBe(true);
    const ordinary = runPoliticsReformScrutiny(input); input.simulationId++;
    const other = runPoliticsReformScrutiny(input);
    expect(other.chambers.every(c => c.simulationId === input.simulationId)).toBe(true);
    expect(other.draws.every(d => !ordinary.draws.some(a => a.streamId === d.streamId))).toBe(true);
    expect(other.warnings.every(w => w.simulationId === input.simulationId)).toBe(true);
  });
  it('keeps mixed-affiliation coalition bonus seats and passes through final remainder evidence', () => {
    const r = runPoliticsReformScrutiny(coalitionScrutinyFixture());
    expect(r.status).toBe('complete');
    const bonus = r.seats.filter(s => s.category === 'bonus');
    expect(bonus).toHaveLength(105);
    expect(bonus.every(s => s.entitlement.subject.id === 'G' && s.entitlement.listId === null)).toBe(true);
    expect(new Set(r.chambers.flatMap(c => c.recovery!.retained.filter(e => e.role === 'bonus').map(e => e.affiliationListId)))).toEqual(new Set(['A', 'C']));
    for (const transfer of r.transfers) for (const at of [transfer.from, transfer.to]) {
      if (!at) continue;
      if (at.listId === 'A' || at.listId === 'C') expect(at.subject).toEqual({ kind: 'coalition', id: 'G' });
    }
    for (const s of r.chambers) {
      const colleges = s.cameraTerritorial?.colleges ?? s.senateTerritorial!.colleges;
      for (const m of colleges.matrices.filter(m => m.correction !== 'college-groups')) for (const cell of m.cells) {
        expect(s.recovery!.remainders.find(r => r.id === JSON.stringify([m.contextId, cell.id]))).toMatchObject({
          collegeId: cell.areaId, subject: { kind: 'list', id: cell.targetId }, value: cell.decimalPart, state: cell.remainderState
        });
      }
      for (const t of s.recovery!.recoveries) for (const used of t.remainders) {
        const source = s.recovery!.remainders.find(r => r.id === used.id)!;
        expect(used).toMatchObject({ value: source.value, before: source.state, after: source.state });
      }
      if (s.regional?.cap) for (const final of s.regional.cap.remainders) {
        expect(s.recovery!.remainders.find(r => r.collegeId === null && r.subject.kind === final.subject.kind && r.subject.id === final.subject.id))
          .toMatchObject({ state: final.state });
      }
    }
  });
  it.each([30, 42, 45, 50, 60])('reconciles components once at majority %s and preserves equivalent downstream seats', majority => {
    const attributed = completeScrutinyFixture(majority), raw = structuredClone(attributed);
    if (raw.renewal.kind !== 'joint') throw new Error();
    for (const c of [raw.renewal.camera, raw.renewal.senato]) {
      for (const row of c.ordinaryVotes) {
        if (row.votes.kind !== 'attributed') throw new Error();
        const { listVotes, provenance } = row.votes;
        const ordinary = c.colleges.find(p => p.id === row.collegeId)!.kind === 'ordinary';
        row.votes = { kind: 'components', directListVotes: listVotes.map(l => ({ ...l, votes: l.votes - (ordinary && l.listId === 'A' ? 100 : 0) })),
          bonusOnlyVotes: ordinary ? [{ subject: { kind: 'list', id: 'A' }, votes: 100 }] : [], provenance };
      }
      for (const row of c.specialVotes) {
        if (row.votes.kind !== 'attributed') throw new Error();
        const { listVotes, candidateTotals, provenance } = row.votes;
        row.votes = { kind: 'components', directListVotes: listVotes.map(l => ({ ...l, votes: l.votes - 100 })),
          candidateTotals, candidateOnlyVotes: candidateTotals.map(p => ({ candidateId: p.candidateId, votes: 100 })), provenance };
      }
    }
    const r = runPoliticsReformScrutiny(raw), final = runPoliticsReformScrutiny(attributed);
    expect(r.status).toBe('complete'); expect(r.seats).toEqual(final.seats); expect(r.draws).toEqual(final.draws);
    expect(r.conditions.some(c => c.kind === 'unknown-ballot-composition')).toBe(false);
    expect(r.chambers.map(c => c.figures)).toEqual(final.chambers.map(c => c.figures));
  });
  it('blocks positive coalition-only attribution without direct support before publishing national figures', () => {
    const input = coalitionScrutinyFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    const row = input.renewal.camera.ordinaryVotes.find(r => r.collegeId === 'P00')!;
    if (row.votes.kind !== 'attributed') throw new Error();
    row.votes = { kind: 'components', provenance: row.votes.provenance,
      directListVotes: row.votes.listVotes.map(l => ({ ...l, votes: l.listId === 'B' ? l.votes : 0 })),
      bonusOnlyVotes: [{ subject: { kind: 'coalition', id: 'G' }, votes: 100 }] };
    // Direct-list preference budget must also reflect the changed source.
    for (const s of input.renewal.camera.ordinarySlates.filter(s => s.collegeId === row.collegeId)) s.preferences = { kind: 'missing', reason: 'no preferences supplied' };
    const r = runPoliticsReformScrutiny(input);
    expect(r.status).toBe('non-computable'); expect(r.chambers[0].figures).toBeUndefined();
    expect(r.stages).toContainEqual({ norm: 'N04', chamber: 'camera', status: 'non-computable', reason: 'zero-direct-support', affectedIds: [row.collegeId] });
    expect(r.stages).toContainEqual({ norm: 'N06', chamber: 'camera', status: 'not-run', reason: 'attribution-non-computable', affectedIds: [] });
  });
  it('preserves I07 cross-region warnings and entitlement locations in a complete composed renewal', () => {
    const input = completeScrutinyFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    const c = input.renewal.senato;
    c.regions.push({ id: 'R', autonomous: false });
    c.territories.find(t => t.id === 'O')!.bonusSeats = 19;
    c.territories.push({ id: 'R', regionId: 'R', kind: 'ordinary', bonusSeats: 16 });
    for (const p of c.colleges) if (p.kind === 'ordinary' && Number(p.id.slice(1)) >= 16) p.territoryId = 'R';
    c.bonusSlates = c.bonusSlates.flatMap(s => ['O', 'R'].map(territoryId => ({ ...s, territoryId,
      candidates: s.candidates.filter(p => c.ordinarySlates.some(slate => slate.candidateIds.includes(p.candidateId) &&
        c.colleges.find(college => college.id === slate.collegeId)!.territoryId === territoryId)) })));
    const unavailable = new Set(c.ordinarySlates.filter(s => s.listId === 'A' && c.colleges.find(p => p.id === s.collegeId)!.territoryId === 'O').flatMap(s => s.candidateIds));
    for (const p of input.candidates) if (unavailable.has(p.id)) p.availability = { kind: 'unavailable', reason: 'post-filing regional shortage' };
    const r = runPoliticsReformScrutiny(input);
    expect(r.status).toBe('complete'); expect(r.seats).toHaveLength(588);
    const cross = r.recoveries.filter(t => t.interpretationId === 'I07');
    expect(cross.length).toBeGreaterThan(0);
    expect(r.warnings.filter(w => w.interpretationId === 'I07')).toHaveLength(cross.length);
    expect(cross.every(t => t.originalEntitlement.territoryId === 'O' && t.electedAt?.territoryId === 'R')).toBe(true);
    expect(r.interpretations).toContainEqual({ id: 'I07', version: 'approved-2026-10-16' });
    expect(cross.every(t => r.seats.some(s => s.seatId === t.seatId && s.entitlement.territoryId === 'O'))).toBe(true);
    expect(new Set(cross.map(t => r.seats.find(s => s.seatId === t.seatId)!.category))).toEqual(new Set(['ordinary', 'bonus']));
  });
  it('fills the formerly blocked coalition-college recovery using provisional I10 member evidence', () => {
    const input = coalitionScrutinyFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    const c = input.renewal.camera;
    const unavailable = new Set(c.ordinarySlates.filter(s => s.listId === 'A' || s.listId === 'C' && s.collegeId === 'P00').flatMap(s => s.candidateIds));
    for (const p of input.candidates) if (unavailable.has(p.id)) p.availability = { kind: 'unavailable', reason: 'post-filing shortage' };
    const r = runPoliticsReformScrutiny(input);
    expect(r.status).toBe('complete'); expect(r.seats).toHaveLength(588);
    expect(r.seats.some(s => s.status !== 'filled')).toBe(false);
    const camera = r.chambers[0];
    expect(camera.recovery!.remainders.some(r => r.collegeId === 'P01' && r.subject.kind === 'coalition')).toBe(false);
    expect(camera.recovery!.recoveries.some(t => t.route === 'coalition-local-colleges' && t.interpretations.includes('I10'))).toBe(true);
    expect(r.interpretations).toContainEqual({ id: 'I10', version: 'I10-v1' });
    expect(r.warnings.some(w => w.interpretationId === 'I10')).toBe(true);
  });
  it.each([1, 2])('I11 excludes zero-territorial-entitlement recovery in both single and multiple colleges (%s colleges)', count => {
    const input = completeScrutinyFixture(45, false);
    if (input.renewal.kind !== 'joint') throw new Error();
    const c = input.renewal.senato;
    c.regions.push({ id: 'R', autonomous: false });
    c.territories.find(t => t.id === 'O')!.bonusSeats -= count;
    c.territories.push({ id: 'R', regionId: 'R', kind: 'ordinary', bonusSeats: count });
    const collegeIds = new Set(['P31', ...(count === 2 ? ['P30'] : [])]);
    for (const p of c.colleges) if (collegeIds.has(p.id)) p.territoryId = 'R';
    c.bonusSlates = c.bonusSlates.flatMap(s => ['O', 'R'].map(territoryId => ({ ...s, territoryId,
      candidates: s.candidates.filter(p => c.ordinarySlates.some(slate => slate.candidateIds.includes(p.candidateId) &&
        c.colleges.find(college => college.id === slate.collegeId)!.territoryId === territoryId)) })));
    for (const row of c.ordinaryVotes.filter(r => collegeIds.has(r.collegeId))) {
      if (row.votes.kind !== 'attributed') throw new Error();
      row.votes.listVotes = [{ listId: 'A', votes: 1 }, { listId: 'B', votes: 69999 }, { listId: 'C', votes: 30000 }];
    }
    for (const s of c.ordinarySlates.filter(s => collegeIds.has(s.collegeId) && s.listId === 'A')) {
      if (s.preferences.kind === 'counts') for (const p of s.preferences.counts) p.count = 0;
    }
    const unavailable = new Set(c.ordinarySlates.filter(s => s.listId === 'A' && !collegeIds.has(s.collegeId)).flatMap(s => s.candidateIds));
    for (const p of input.candidates) if (unavailable.has(p.id)) p.availability = { kind: 'unavailable', reason: 'post-filing shortage' };
    const r = runPoliticsReformScrutiny(input), senate = r.chambers[1];
    expect(r.status).toBe('partial'); // Exhausted permitted candidates; zero targets cannot receive recovery.
    expect(senate.senateTerritorial!.lists.status).toBe('complete');
    if (senate.senateTerritorial!.lists.status !== 'complete') throw new Error();
    expect(senate.senateTerritorial!.lists.targets.find(t => t.regionId === 'R' && t.listId === 'A')!.ordinarySeats).toBe(0);
    const missing = senate.recovery!.seats.filter(s => s.status === 'unresolved' && s.reason === 'missing-recovery-remainder');
    expect(missing).toEqual([]);
    expect(senate.recovery!.remainders.some(r => collegeIds.has(r.collegeId ?? '') && r.subject.id === 'A')).toBe(false);
    expect(senate.recovery!.seats.some(s => s.status === 'filled' && s.electedAt.territoryId === 'R' && s.electedAt.listId === 'A')).toBe(false);
    expect(senate.recovery!.seats.some(s => s.status === 'unfilled' && s.reason === 'permitted-candidates-exhausted')).toBe(true);
    expect(senate.recovery!.excludedLists).toContainEqual({ territoryId: 'R', listId: 'A', reason: 'zero-territorial-entitlement' });
    expect(r.interpretations).toContainEqual({ id: 'I11', version: 'I11-v1' });
    expect(r.seats).toHaveLength(588);
  });
  it('retains explicit failures and known special entitlements for an all-zero election', () => {
    const input = completeScrutinyFixture();
    if (input.renewal.kind !== 'joint') throw new Error();
    for (const c of [input.renewal.camera, input.renewal.senato]) {
      for (const row of [...c.ordinaryVotes, ...c.specialVotes]) if (row.votes.kind === 'attributed') for (const l of row.votes.listVotes) l.votes = 0;
      for (const row of c.specialVotes) for (const p of row.votes.candidateTotals) p.votes = 0;
      for (const s of c.ordinarySlates) s.preferences = { kind: 'missing', reason: 'zero ballot fixture' };
    }
    const r = runPoliticsReformScrutiny(input);
    expect(r.status).toBe('non-computable'); expect(r.seats.filter(s => s.category === 'special-uninominal')).toHaveLength(12);
    expect(r.bonus.status).toBe('non-computable');
    expect(r.chambers[1].regional?.status).toBe('non-computable');
    expect(r.stages).toContainEqual({ norm: 'N22', chamber: 'senato', status: 'not-run',
      reason: 'regional-allocation-non-computable', affectedIds: [] });
    expect(r.stages.some(s => s.status === 'non-computable')).toBe(true);
    expect(r.conditions.some(c => c.kind === 'missing-preferences')).toBe(true);
  });
});
it('I12 preserves national Senate admission for a special-only group without assigning it ordinary regional seats', () => {
  const input = completeScrutinyFixture(60);
  if (input.renewal.kind !== 'joint') throw new Error();
  const c = input.renewal.senato;
  const vda = c.territories.find(t => t.kind === 'vda')!;
  const college = c.colleges.find(p => p.territoryId === vda.id)!;
  c.lists.push({ id: 'D', name: 'Special-only group', coalitionId: null, protectedMinority: null });
  const candidate = c.specialCandidatures.find(s => s.collegeId === college.id && s.affiliationListId === 'B')!;
  candidate.affiliationListId = 'D'; candidate.linkedListIds = ['D'];
  const row = c.specialVotes.find(r => r.collegeId === college.id)!;
  if (row.votes.kind !== 'attributed') throw new Error();
  row.votes.listVotes.find(l => l.listId === 'B')!.listId = 'D';
  const result = runPoliticsReformScrutiny(input);
  expect(result.status).toBe('complete');
  expect(result.seats).toHaveLength(588);
  const senate = result.chambers.find(c => c.chamber === 'senato')!;
  expect(senate.admission?.status === 'complete' && senate.admission.lists.find(l => l.listId === 'D')).toMatchObject({ admitted: true, reasons: ['regional-20-percent'] });
  expect(senate.regional?.status === 'complete' && senate.regional.targets.filter(t => t.subject.id === 'D').every(t => t.ordinarySeats === 0)).toBe(true);
  expect(result.interpretations).toContainEqual({ id: 'I12', version: 'I12-v1' });
  expect(result.warnings).toContainEqual(expect.objectContaining({ code: 'I12_TERRITORIAL_ELIGIBILITY', chamber: 'senato' }));
});
