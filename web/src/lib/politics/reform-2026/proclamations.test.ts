import { describe, expect, it } from 'vitest';
import { proclaimCandidates } from './proclamations';
import { proclamationFixture } from './proclamations.fixture';
import { jointFixture, supplied } from './contracts.fixture';
import { aggregateElectoralFigures } from './figures';
import { determineSpecialWinners } from './special-winners';
import { proclaimSpecialUninominalWinners, summarizeSpecialSeats } from './special-seats';
import { determineCameraAdmission, determineSenateAdmission } from './admission';
import { determineGovernabilityBonus } from './bonus';
import { allocateCameraTaaSeats } from './taa-allocation';
import { allocateCameraNationalSeats } from './camera-national';
import { allocateCameraConstituencies, allocateCameraCoalitionListsInConstituencies, allocateCameraColleges } from './camera-territorial';
import { allocateSenateRegions } from './senate-regional';
import { allocateSenateListsAndColleges } from './senate-territorial';
import { validateReformScrutinyInput } from './validation';
import { createSeededDrawContext } from './ties';
import { resolveMultipleElections } from './multiple-elections';
import { recoverInitialSeats } from './recovery';
import type { ScrutinyExecutionContext } from './result';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
describe('N23 supplied preferences and preserved entitlements', () => {
  it.each(['camera', 'senato'] as const)('elects %s capolista first, then preferences with filed-order ties', side => {
    const r = proclaimCandidates(proclamationFixture(side));
    expect(r.status).toBe('complete');
    expect(r.elections.map(e => e.seat.candidateId)).toEqual(['P1-0', 'P1-2', 'P1-3', 'P2-0', 'P2-2', 'P2-3']);
    expect(r.elections.map(e => e.role)).toEqual(['capolista', 'preference', 'preference', 'capolista', 'preference', 'preference']);
    expect(r.elections[1].individualPercentage).toEqual({ status: 'defined', value: { numerator: '20', denominator: '1' } });
  });
  it('keeps known capolista identities when preferences are missing', () => {
    const input = proclamationFixture(); input.chamber.ordinarySlates[0].preferences = { kind: 'missing', reason: 'No supplied preferences' };
    const r = proclaimCandidates(input);
    expect(r.status).toBe('partial'); expect(r.seats).toHaveLength(6);
    expect(r.seats.flatMap(s => s.status === 'unresolved' ? [s.reason] : [])).toEqual(['missing-preferences', 'missing-preferences']);
    expect(r.elections[0].seat.candidateId).toBe('P1-0');
  });
  it('explicit zero preferences elect in filed order', () => {
    const input = proclamationFixture();
    for (const s of input.chamber.ordinarySlates) if (s.preferences.kind === 'counts') for (const p of s.preferences.counts) p.count = 0;
    expect(proclaimCandidates(input).elections.map(e => e.seat.candidateId)).toEqual(['P1-0', 'P1-1', 'P1-2', 'P2-0', 'P2-1', 'P2-2']);
  });
  it('does not promote around unavailable candidates or hide exhaustion', () => {
    const input = proclamationFixture('camera', [8, 0]); input.candidates[0].availability = { kind: 'unavailable', reason: 'withdrawn after filing' };
    const r = proclaimCandidates(input);
    expect(r.seats).toHaveLength(8); expect(r.elections).toHaveLength(6);
    expect(r.seats.flatMap(s => s.status === 'unresolved' ? [s.reason] : [])).toEqual(['candidate-unavailable-requires-recovery', 'candidate-exhaustion-requires-recovery']);
    expect(r.elections[0].seat.candidateId).toBe('P1-2');
  });
  it('does not need figures or preferences for zero seats', () => {
    const input = proclamationFixture('senato', [0, 0]); input.figures.colleges = [];
    expect(proclaimCandidates(input)).toMatchObject({ status: 'complete', seats: [], elections: [] });
  });
  it.each(['ordinary', 'bonus'] as const)('blocks a non-computable %s communication', stage => {
    const input = proclamationFixture();
    if (stage === 'ordinary' && input.ordinary.chamber === 'camera') input.ordinary = { ...input.ordinary, status: 'non-computable', reason: 'zero-quota', affectedId: null };
    else input.bonus = { ...input.bonus, status: 'non-computable', awards: [], blockers: [] };
    expect(proclaimCandidates(input)).toMatchObject({ status: 'non-computable', seats: [], elections: [] });
  });
  it.each(['simulation', 'chamber', 'source', 'bonus-run', 'special-run', 'duplicate', 'capacity', 'slate', 'figure', 'candidate', 'subject', 'votes'] as const)('rejects invalid %s communications', kind => {
    const input = proclamationFixture();
    if (kind === 'simulation') input.ordinary.simulationId++;
    if (kind === 'chamber') input.chamber.chamber = 'senato';
    if (kind === 'source') input.ordinary.seatTable.version = 'changed';
    if (kind === 'bonus-run') input.bonus.simulationId++;
    if (kind === 'special-run') input.special.simulationId++;
    if (input.ordinary.status !== 'complete') throw new Error();
    if (kind === 'duplicate' && input.ordinary.chamber === 'camera') input.ordinary.targets.push(input.ordinary.targets[0]);
    if (kind === 'capacity') input.ordinary.targets[0].ordinarySeats++;
    if (kind === 'slate') input.chamber.ordinarySlates[0].candidateIds.pop();
    if (kind === 'figure') input.figures.colleges.pop();
    if (kind === 'candidate') input.candidates.shift();
    if (kind === 'subject') input.ordinary.targets[0].subject.id = 'B';
    if (kind === 'votes') input.ordinary.targets[0].votes++;
    expect(() => proclaimCandidates(input)).toThrow(RangeError);
  });
  it('owns JSON-safe output and preserves source permutation replay', () => {
    const input = proclamationFixture(), before = structuredClone(input), r = proclaimCandidates(input);
    expect(input).toEqual(before); expect(JSON.parse(JSON.stringify(r))).toEqual(r);
    expect(r.seats[0].entitlement).not.toBe(input.ordinary.status === 'complete' && input.ordinary.targets[0]);
    input.chamber.colleges.reverse(); input.chamber.ordinarySlates.reverse(); input.candidates.reverse(); input.figures.colleges.reverse();
    if (input.ordinary.status !== 'complete') throw new Error(); input.ordinary.targets.reverse();
    expect(proclaimCandidates(input)).toEqual(r);
  });
  it('rejects incompatible filed bonus/special candidatures before proclaiming any winner', () => {
    const input = proclamationFixture();
    input.chamber.bonusSlates.push({ territoryId: 'R', subject: { kind: 'list', id: 'A' }, candidates: [{ candidateId: 'P1-0', affiliationListId: 'A' }] });
    input.chamber.specialCandidatures.push({ collegeId: 'V', candidateId: 'P1-0', linkedListIds: ['A'], affiliationListId: 'A' });
    expect(() => proclaimCandidates(input)).toThrow(/Incompatible special\/bonus/);
  });
});

describe('validated joint envelopes through N23/N24', () => {
  it.each([[false, false], [true, false], [false, true]])('preserves bonus affiliations, special seats and TAA proclamations (missing=%s, coalition=%s)', (missing, coalition) => {
    const full = jointFixture(); if (full.renewal.kind !== 'joint') throw new Error();
    // Two lists prevent the cap's minority allocation from being empty.
    for (const c of [full.renewal.camera, full.renewal.senato]) {
      c.lists.push({ ...c.lists[0], id: 'B', name: 'B' });
      const ids = new Set([...c.ordinarySlates.flatMap(s => s.candidateIds), ...c.specialCandidatures.map(s => s.candidateId)]);
      full.candidates.push(...full.candidates.filter(p => ids.has(p.id)).map(p => ({ ...p, id: `${p.id}-B` })));
      c.ordinarySlates.push(...c.ordinarySlates.map(s => ({ ...s, listId: 'B', candidateIds: s.candidateIds.map(id => `${id}-B`) })));
      c.bonusSlates.push(...c.bonusSlates.map(s => ({ ...s, subject: { kind: 'list' as const, id: 'B' }, candidates: s.candidates.map(p => ({ candidateId: `${p.candidateId}-B`, affiliationListId: 'B' })) })));
      c.specialCandidatures.push(...c.specialCandidatures.map(s => ({ ...s, candidateId: `${s.candidateId}-B`, linkedListIds: ['B'], affiliationListId: 'B' })));
      for (const row of [...c.ordinaryVotes, ...c.specialVotes]) {
        row.electorate.electors = 200000; if (row.votes.kind !== 'attributed') throw new Error();
        row.votes.listVotes[0].votes = 60000; row.votes.listVotes.push({ listId: 'B', votes: 40000 });
      }
      for (const row of c.specialVotes) { row.votes.candidateTotals[0].votes = 60000;
        row.votes.candidateTotals.push({ candidateId: `${row.votes.candidateTotals[0].candidateId}-B`, votes: 40000 }); }
      if (c.chamber === 'camera') {
        // A valid same-badge special winner is also TAA capolista.
        c.ordinarySlates.find(s => s.collegeId === 'TP' && s.listId === 'A')!.candidateIds[0] =
          c.specialCandidatures.find(s => s.collegeId === 'V0' && s.affiliationListId === 'A')!.candidateId;
      }
      if (!missing) for (const s of c.ordinarySlates) s.preferences = { kind: 'counts',
        counts: s.candidateIds.slice(1).map((candidateId, i) => ({ candidateId, count: (6 - i) * 100 })),
        provenance: supplied, validity: { kind: 'adjudicated', reference: 'valid supplied counts' } };
      if (coalition) {
        c.lists[0].coalitionId = 'G'; c.lists.push({ ...c.lists[0], id: 'D', name: 'D' });
        c.coalitions.push({ id: 'G', listIds: ['A', 'D'] });
        const aSlates = c.ordinarySlates.filter(s => s.listId === 'A');
        const aPeople = new Set(aSlates.flatMap(s => s.candidateIds));
        full.candidates.push(...full.candidates.filter(p => aPeople.has(p.id)).map(p => ({ ...p, id: `${p.id}-D` })));
        c.ordinarySlates.push(...aSlates.map(s => ({ ...s, listId: 'D', candidateIds: s.candidateIds.map(id => `${id}-D`),
          preferences: s.preferences.kind === 'counts' ? { ...s.preferences, counts: s.preferences.counts.map(p => ({ ...p, candidateId: `${p.candidateId}-D` })) } : s.preferences })));
        const winning = c.bonusSlates.find(s => s.subject.id === 'A')!;
        winning.subject = { kind: 'coalition', id: 'G' };
        // Whole seven-slot blocks preserve each bonus candidate's legal prefix.
        const firstD = c.chamber === 'camera' ? 35 : 21;
        winning.candidates = winning.candidates.map((p, i) => i < firstD ? p : { candidateId: `${p.candidateId}-D`, affiliationListId: 'D' });
        for (const row of [...c.ordinaryVotes, ...c.specialVotes]) {
          if (row.votes.kind !== 'attributed') throw new Error(); row.votes.listVotes.push({ listId: 'D', votes: 20000 });
        }
        for (const row of c.specialVotes) row.votes.candidateTotals[0].votes = 80000;
        for (const slot of c.specialCandidatures.filter(s => s.affiliationListId === 'A')) slot.linkedListIds.push('D');
      }
    }
    if (coalition) full.bonusLinks = [{ camera: { kind: 'coalition', id: 'G' }, senato: { kind: 'coalition', id: 'G' }, cameraDeclares: true, senatoDeclares: true }];
    const checked = validateReformScrutinyInput(full); expect(checked.issues).toEqual([]); if (!checked.valid) throw new Error();
    const input = checked.input; if (input.renewal.kind !== 'joint') throw new Error();
    const stages = [input.renewal.camera, input.renewal.senato].map(chamber => {
      const args = { chamber, candidates: input.candidates, simulationId: input.simulationId };
      const ordinary = chamber.ordinaryVotes.filter(r => chamber.colleges.find(c => c.id === r.collegeId)!.kind === 'ordinary').map(r => {
        if (r.votes.kind !== 'attributed') throw new Error(); return { collegeId: r.collegeId, listVotes: r.votes.listVotes };
      });
      const specialVotes = chamber.specialVotes.map(r => { if (r.votes.kind !== 'attributed') throw new Error(); return { collegeId: r.collegeId, listVotes: r.votes.listVotes }; });
      const figures = aggregateElectoralFigures(chamber, ordinary, specialVotes);
      const preliminary = determineSpecialWinners(args, noDraw);
      const special = proclaimSpecialUninominalWinners({ ...args, preliminary });
      const admission = chamber.chamber === 'camera' ? determineCameraAdmission({ ...args, figures, specialWinners: preliminary }, noDraw) : determineSenateAdmission({ ...args, figures }, noDraw);
      const taa = chamber.chamber === 'camera' ? allocateCameraTaaSeats({ ...args, figures, admission, collegeId: 'TP' }, noDraw) : undefined;
      return { ...args, figures, admission, special, taa, specialSeats: summarizeSpecialSeats(special, taa) };
    });
    const bonus = determineGovernabilityBonus({ simulationId: input.simulationId, renewedChambers: ['camera', 'senato'],
      chambers: stages.map(s => ({ chamber: s.chamber.chamber, admission: s.admission, national: s.figures.national })), bonusLinks: input.bonusLinks }, noDraw);
    expect(bonus.status).toBe('awarded');
    for (const s of stages) {
      const execution = createSeededDrawContext('proclamations', 'legal');
      let ordinary;
      if (s.chamber.chamber === 'camera') {
        const national = allocateCameraNationalSeats({ ...s, bonus }, execution);
        const args = { ...s, national };
        const territories = allocateCameraConstituencies(args, execution);
        const lists = allocateCameraCoalitionListsInConstituencies(args, territories, execution);
        ordinary = allocateCameraColleges(args, lists, execution);
      } else {
        const regional = allocateSenateRegions({ ...s, bonus }, execution);
        ordinary = allocateSenateListsAndColleges({ ...s, regional }, execution).colleges;
      }
      const proclamations = proclaimCandidates({ ...s, ordinary, bonus });
      const r = resolveMultipleElections(proclamations, execution);
      expect(proclamations.status).toBe('partial'); // Seven-slot lists cannot fill these deliberately oversized colleges.
      expect(r.seats).toHaveLength(s.chamber.chamber === 'camera' ? 392 : 196);
      expect(proclamations.elections.filter(e => e.role === 'bonus')).toHaveLength(s.chamber.chamber === 'camera' ? 70 : 35);
      expect(r.retained.filter(e => e.role === 'bonus')).toHaveLength(s.chamber.chamber === 'camera' ? 70 : 35);
      if (coalition) {
        const electedBonus = r.retained.filter(e => e.role === 'bonus');
        expect(new Set(electedBonus.map(e => e.affiliationListId))).toEqual(new Set(['A', 'D']));
        expect(electedBonus.every(e => e.seat.entitlement.listId === null && e.seat.entitlement.subject.kind === 'coalition')).toBe(true);
      }
      expect(r.released.every(e => ['bonus-priority', 'special-uninominal-priority'].includes(r.decisions.find(d => d.candidateId === e.candidateId)!.reason))).toBe(true);
      if (s.chamber.chamber === 'camera') expect(r.decisions.some(d => d.reason === 'special-uninominal-priority')).toBe(true);
      expect(new Set(r.retained.map(e => e.seat.candidateId)).size).toBe(r.retained.length);
      expect(r.seats.filter(e => e.category === 'special-uninominal')).toEqual(s.special.seats);
      // R26-17 accepts the real N23/N24 ledger. These deliberately oversized
      // one-territory colleges exhaust every permitted destination; therefore
      // no fabricated quotient is needed to prove their exhausted endpoint.
      if (ordinary.status !== 'complete') throw new Error();
      const eligibleLists = [...new Map([
        ...ordinary.targets.map(t => ({ territoryId: t.territoryId, listId: t.listId })),
        ...(s.taa && s.taa.status !== 'non-computable' ? s.taa.listEntitlements.map(t => ({ territoryId: s.taa!.territoryId, listId: t.listId })) : [])
      ].map(t => [JSON.stringify(t), t])).values()];
      const recovered = recoverInitialSeats({ ...s, initial: r, eligibleLists, remainders: [] }, execution);
      expect(recovered.seats.map(t => [t.seatId, t.category, t.entitlement])).toEqual(r.seats.map(t => [t.seatId, t.category, t.entitlement]));
      expect(recovered.seats.filter(t => t.category === 'special-uninominal')).toEqual(s.special.seats);
      expect(new Set(recovered.retained.map(e => e.seat.candidateId)).size).toBe(recovered.retained.length);
      expect(recovered.seats.some(t => t.status === 'unfilled')).toBe(true);
      expect(recovered.status).toBe('partial');
      expect(recovered.retained.filter(e => e.role === 'bonus')).toHaveLength(s.chamber.chamber === 'camera' ? 70 : 35);
      if (s.taa?.status !== 'non-computable') expect(proclamations.seats.filter(e => e.category === 'taa-proportional')).toEqual(s.taa?.seats ?? []);
      const args = { ...s, ordinary, bonus };
      const unavailableId = proclamations.elections.find(e => e.role === 'bonus')!.seat.candidateId;
      const unavailable = proclaimCandidates({ ...args, candidates: args.candidates.map(p => p.id === unavailableId ?
        { ...p, availability: { kind: 'unavailable', reason: 'post-filing unavailability' } } : p) });
      expect(unavailable.seats).toHaveLength(proclamations.seats.length);
      expect(unavailable.seats.some(e => e.category === 'bonus' && e.status === 'unresolved' && e.reason === 'candidate-unavailable-requires-recovery')).toBe(true);
      expect(unavailable.elections.some(e => e.seat.candidateId === unavailableId)).toBe(false);
      expect(() => proclaimCandidates({ ...args, chamber: { ...args.chamber, bonusSlates: [] } })).toThrow(/winning bonus slate/);
      if (s.taa) {
        const taa = s.taa;
        expect(() => proclaimCandidates({ ...args, taa: undefined })).toThrow(/TAA communication/);
        expect(() => proclaimCandidates({ ...args, taa: { ...taa, simulationId: 99 } })).toThrow(/matching/);
        expect(proclaimCandidates({ ...args, taa: { ...taa, status: 'non-computable', reason: 'zero-quota' } })).toMatchObject({ status: 'non-computable', blockers: ['taa-allocation-non-computable'] });
      }
    }
  });
});

