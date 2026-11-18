import { describe, expect, it } from 'vitest';
import { determineCameraAdmission, determineSenateAdmission } from './admission';
import { jointFixture, singleFixture } from './contracts.fixture';
import { aggregateElectoralFigures } from './figures';
import { attributeSpecialVotes } from './special-attribution';
import { proclaimSpecialUninominalWinners, summarizeSpecialSeats } from './special-seats';
import { determineSpecialWinners } from './special-winners';
import { taaFixture } from './special-territories.fixture';
import { allocateCameraTaaSeats } from './taa-allocation';
import { createSeededDrawContext } from './ties';
import { validateReformScrutinyInput } from './validation';
import type { Candidate, ChamberInput } from './input';
import type { ScrutinyExecutionContext } from './result';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };

describe('special initial proclamations and cap ledger', () => {
  it.each(['camera', 'senato'] as const)('runs the validated %s special path with all special seats counted once', side => {
    const f = singleFixture(side);
    const validated = validateReformScrutinyInput(f);
    expect(validated.issues).toEqual([]);
    if (!validated.valid || validated.input.renewal.kind !== 'single') throw new Error();
    const chamber = validated.input.renewal.chamber;
    const input = { chamber, candidates: validated.input.candidates, simulationId: f.simulationId };
    const preliminary = determineSpecialWinners(input, noDraw);
    const special = chamber.specialVotes.map(r => {
      const attributed = attributeSpecialVotes({ ...input, collegeId: r.collegeId }, noDraw);
      if (attributed.status !== 'complete') throw new Error();
      return attributed;
    });
    const ordinary = chamber.ordinaryVotes.filter(r => chamber.colleges.find(c => c.id === r.collegeId)!.kind === 'ordinary')
      .map(r => {
        if (r.votes.kind !== 'attributed') throw new Error();
        return { collegeId: r.collegeId, listVotes: r.votes.listVotes };
      });
    const figures = aggregateElectoralFigures(chamber, ordinary, special);
    const admissionInput = { ...input, figures, specialWinners: preliminary };
    const admission = side === 'camera' ? determineCameraAdmission(admissionInput, noDraw) : determineSenateAdmission(admissionInput, noDraw);
    const uninominal = proclaimSpecialUninominalWinners({ ...input, preliminary });
    const taa = side === 'camera' ? allocateCameraTaaSeats({ ...input, figures, admission, collegeId: 'TP' }, noDraw) : undefined;
    const totals = summarizeSpecialSeats(uninominal, taa);
    expect(totals).toMatchObject({ status: 'complete', uninominalSeats: side === 'camera' ? 5 : 7,
      taaProportionalSeats: side === 'camera' ? 3 : 0, totalSeats: side === 'camera' ? 8 : 7 });
    expect(totals.subjects).toEqual([{ subject: { kind: 'list', id: 'A' }, uninominalSeats: side === 'camera' ? 5 : 7,
      taaProportionalSeats: side === 'camera' ? 3 : 0, totalSeats: side === 'camera' ? 8 : 7 }]);
    expect(figures.national.validVotes - figures.ordinaryAllocation.validVotes).toBe(side === 'camera' ? 500 : 700);
    expect(uninominal.seats.every(s => s.category === 'special-uninominal')).toBe(true);
    expect(new Set([...uninominal.seats, ...(taa && taa.status !== 'non-computable' ? taa.seats : [])].map(s => s.seatId)).size).toBe(totals.totalSeats);
    // Missing preferences make candidate proclamations partial, not seat counts.
    if (taa) expect(taa.status).toBe('partial');
  });
  it('proclaims special winners linked to nationally excluded lists and counts their seats', () => {
    const f = taaFixture({ A: 40, X: 60 }, 3, {}, { A: 100000, X: 0 });
    const preliminary = determineSpecialWinners(f, noDraw);
    const admission = determineCameraAdmission({ ...f, specialWinners: preliminary }, noDraw);
    if (admission.status !== 'complete') throw new Error();
    expect(admission.lists.find(l => l.listId === 'X')!.admitted).toBe(false);
    const uni = proclaimSpecialUninominalWinners({ ...f, preliminary });
    expect(uni.seats.every(s => s.status === 'filled' && s.entitlement.subject.id === 'X')).toBe(true);
    const taa = allocateCameraTaaSeats({ ...f, admission }, noDraw);
    expect(summarizeSpecialSeats(uni, taa).subjects).toEqual([
      { subject: { kind: 'list', id: 'A' }, uninominalSeats: 0, taaProportionalSeats: 3, totalSeats: 3 },
      { subject: { kind: 'list', id: 'X' }, uninominalSeats: 4, taaProportionalSeats: 0, totalSeats: 4 }
    ]);
  });
  it('reuses equal-date winners with no second draw or repeated top-level warning event', () => {
    const f = taaFixture({ A: 50, B: 50 });
    const delegate = createSeededDrawContext('same-date', 'N09');
    let calls = 0;
    const execution: ScrutinyExecutionContext = { draw(request) { calls++; return delegate.draw(request); } };
    const preliminary = determineSpecialWinners(f, execution);
    expect(calls).toBe(4);
    const uni = proclaimSpecialUninominalWinners({ ...f, preliminary });
    expect(calls).toBe(4);
    expect(uni.preliminary).toEqual(preliminary);
    expect(uni.seats.map(s => s.status === 'filled' ? s.candidateId : null)).toEqual(preliminary.winners.map(w => w.candidateId));
    expect(preliminary.warnings).toHaveLength(4);
    expect(uni).not.toHaveProperty('draws');
    expect(uni).not.toHaveProperty('warnings');
  });
  it('keeps coalition linkage distinct from personal list affiliation in cap totals', () => {
    const f = taaFixture({ A: 40, B: 60 }, 3, { C: ['A', 'B'] });
    const preliminary = determineSpecialWinners(f, noDraw);
    const uni = proclaimSpecialUninominalWinners({ ...f, preliminary });
    expect(uni.seats.every(s => s.entitlement.listId === null)).toBe(true);
    expect(uni.preliminary.winners.every(w => w.affiliationListId === 'A' && w.linkedListIds.length === 2)).toBe(true);
    const admission = determineCameraAdmission({ ...f, specialWinners: preliminary }, noDraw);
    const totals = summarizeSpecialSeats(uni, allocateCameraTaaSeats({ ...f, admission }, noDraw));
    expect(totals.subjects).toEqual([{ subject: { kind: 'coalition', id: 'C' },
      uninominalSeats: 4, taaProportionalSeats: 3, totalSeats: 7 }]);
  });
  it('retains an unavailable uninominal winner entitlement without inventing a replacement', () => {
    const f = taaFixture({ A: 100 });
    const preliminary = determineSpecialWinners(f, noDraw);
    const candidates: Candidate[] = f.candidates.map(c => c.id === preliminary.winners[0].candidateId
      ? { ...c, availability: { kind: 'unavailable', reason: 'constructed' } } : c);
    const uni = proclaimSpecialUninominalWinners({ ...f, candidates, preliminary });
    expect(uni.status).toBe('partial');
    expect(uni.seats[0]).toMatchObject({ status: 'unresolved', reason: 'candidate-unavailable-requires-recovery' });
    expect(summarizeSpecialSeats(uni)).toMatchObject({ status: 'partial', uninominalSeats: 4, taaProportionalSeats: null, totalSeats: null });
  });
  it('keeps non-computable TAA totals unknown and rejects mismatched simulation or chamber', () => {
    const f = taaFixture({ A: 0 }, 3, {}, { A: 100 });
    const preliminary = determineSpecialWinners(f, noDraw);
    const uni = proclaimSpecialUninominalWinners({ ...f, preliminary });
    const admission = determineCameraAdmission({ ...f, specialWinners: preliminary }, noDraw);
    const taa = allocateCameraTaaSeats({ ...f, admission }, noDraw);
    expect(summarizeSpecialSeats(uni, taa)).toMatchObject({ status: 'partial', uninominalSeats: 4, totalSeats: null });
    expect(() => summarizeSpecialSeats(uni, { ...taa, simulationId: -1 })).toThrow(RangeError);
    expect(() => summarizeSpecialSeats({ ...uni, chamber: 'senato' }, taa)).toThrow(RangeError);
  });
  it('rejects missing, duplicate, wrong-simulation and foreign preliminary winners', () => {
    const f = taaFixture({ A: 100 });
    const preliminary = determineSpecialWinners(f, noDraw);
    const variants = [{ ...preliminary, winners: [] }, { ...preliminary, simulationId: 77 },
      { ...preliminary, chamber: 'senato' as const },
      { ...preliminary, winners: preliminary.winners.map(() => preliminary.winners[0]) },
      { ...preliminary, winners: preliminary.winners.map(w => ({ ...w, votes: 900 })) }];
    for (const p of variants) expect(() => proclaimSpecialUninominalWinners({ ...f, preliminary: p })).toThrow(RangeError);
  });
  it('returns owned JSON-safe proclamations without modifying the preliminary ledger', () => {
    const f = taaFixture({ A: 100 });
    const preliminary = determineSpecialWinners(f, noDraw);
    const before = structuredClone(preliminary);
    const r = proclaimSpecialUninominalWinners({ ...f, preliminary });
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
    expect(r.preliminary).not.toBe(preliminary);
    expect(preliminary).toEqual(before);
  });
  it('preserves downstream admission, seats and identities across raw and attributed representations with isolated draw streams', () => {
    const f = taaFixture({ A: 1, B: 5 }, 3, { C: ['A', 'B'] });
    const row = f.chamber.specialVotes.find(r => r.collegeId === 'T0')!;
    const id = row.votes.candidateTotals[0].candidateId;
    const raw: ChamberInput = { ...f.chamber, specialVotes: f.chamber.specialVotes.map(r => r.collegeId === 'T0'
      ? { ...r, votes: { kind: 'components', directListVotes: [{ listId: 'A', votes: 1 }, { listId: 'B', votes: 5 }],
          candidateOnlyVotes: [{ candidateId: id, votes: 3 }], candidateTotals: [{ candidateId: id, votes: 9 }], provenance: r.votes.provenance } } : r) };
    const attributionExecution = createSeededDrawContext('representation', 'attribution');
    const attributed = raw.specialVotes.map(r => {
      const out = attributeSpecialVotes({ ...f, chamber: raw, collegeId: r.collegeId }, attributionExecution);
      if (out.status !== 'complete') throw new Error();
      return out;
    });
    expect(attributed.flatMap(r => r.draws)).toHaveLength(1);
    const final: ChamberInput = { ...raw, specialVotes: raw.specialVotes.map((r, i) => ({ ...r,
      votes: { kind: 'attributed', listVotes: attributed[i].listVotes,
        candidateTotals: attributed[i].candidateTotals, provenance: r.votes.provenance } })) };
    const downstream = (chamber: ChamberInput) => {
      const input = { ...f, chamber };
      const execution = createSeededDrawContext('representation', 'downstream');
      const preliminary = determineSpecialWinners(input, execution);
      const figures = aggregateElectoralFigures(chamber, f.ordinary, attributed);
      const admission = determineCameraAdmission({ ...input, figures, specialWinners: preliminary }, execution);
      const uni = proclaimSpecialUninominalWinners({ ...input, preliminary });
      const taa = allocateCameraTaaSeats({ ...input, figures, admission }, execution);
      return { figures, admission, uni, taa, totals: summarizeSpecialSeats(uni, taa) };
    };
    expect(downstream(raw)).toEqual(downstream(final));
  });
});

describe('special candidature compatibility under C93-octies and S20-bis', () => {
  it('allows a same-badge Camera special candidate in the TAA proportional slate', () => {
    const f = singleFixture('camera');
    if (f.renewal.kind !== 'single') throw new Error();
    const c = f.renewal.chamber;
    const id = c.specialCandidatures.find(s => s.collegeId === 'T1')!.candidateId;
    c.ordinarySlates.find(s => s.collegeId === 'TP')!.candidateIds[0] = id;
    expect(validateReformScrutinyInput(f).valid).toBe(true);
    const chamber: ChamberInput = c;
    const preliminary = determineSpecialWinners({ chamber, candidates: f.candidates, simulationId: f.simulationId }, noDraw);
    const uni = proclaimSpecialUninominalWinners({ chamber, candidates: f.candidates, simulationId: f.simulationId, preliminary });
    expect(uni.seats.some(s => s.status === 'filled' && s.candidateId === id)).toBe(true);
    // Multiple-election precedence/released-seat refill remains R26-16/17.
  });
  it.each(['camera', 'senato'] as const)('rejects the same candidate in two %s special colleges', side => {
    const f = singleFixture(side);
    if (f.renewal.kind !== 'single') throw new Error();
    f.renewal.chamber.specialCandidatures[1].candidateId = f.renewal.chamber.specialCandidatures[0].candidateId;
    expect(validateReformScrutinyInput(f).issues.some(i => i.message.includes('special/bonus'))).toBe(true);
  });
  it('rejects a TAA proportional candidate in an ordinary bonus slate', () => {
    const f = jointFixture();
    if (f.renewal.kind !== 'joint') throw new Error();
    const c = f.renewal.camera;
    c.ordinarySlates.find(s => s.collegeId === 'TP')!.candidateIds[0] = c.bonusSlates[0].candidates[0].candidateId;
    expect(validateReformScrutinyInput(f).issues.some(i => i.message.includes('special/bonus'))).toBe(true);
  });
  it('rejects malformed seven-slot or alternating-gender TAA lists', () => {
    for (const mode of ['length', 'gender']) {
      const f = singleFixture('camera');
      if (f.renewal.kind !== 'single') throw new Error();
      const s = f.renewal.chamber.ordinarySlates.find(s => s.collegeId === 'TP')!;
      if (mode === 'length') s.candidateIds.pop();
      else [s.candidateIds[0], s.candidateIds[1]] = [s.candidateIds[1], s.candidateIds[0]];
      expect(validateReformScrutinyInput(f).issues.some(i => i.code === 'INVALID_CANDIDATURE')).toBe(true);
    }
  });
  it('validates TAA preferences against territorial special ballots, not the duplicate view or candidate-only ballots', () => {
    const f = singleFixture('camera');
    if (f.renewal.kind !== 'single') throw new Error();
    const c = f.renewal.chamber;
    const slate = c.ordinarySlates.find(s => s.collegeId === 'TP')!;
    slate.preferences = { kind: 'counts', counts: slate.candidateIds.slice(1).map((candidateId, i) => ({ candidateId, count: i === 0 ? 200 : 0 })),
      provenance: { kind: 'supplied', reference: 'territorial adjudicated preferences' },
      validity: { kind: 'adjudicated', reference: 'valid counts' } };
    // Four special colleges have 100 attributed votes each, the old TP view 100.
    expect(validateReformScrutinyInput(f).valid).toBe(true);
    for (const row of c.specialVotes.filter(r => r.collegeId.startsWith('T'))) {
      row.votes = { kind: 'components', directListVotes: [{ listId: 'A', votes: 10 }],
        candidateOnlyVotes: [{ candidateId: row.votes.candidateTotals[0].candidateId, votes: 90 }],
        candidateTotals: row.votes.candidateTotals, provenance: row.votes.provenance };
    }
    expect(validateReformScrutinyInput(f).issues.some(i => i.message.includes('ballot budget'))).toBe(true);
    if (slate.preferences.kind !== 'counts') throw new Error();
    slate.preferences.counts[0].count = 40;
    expect(validateReformScrutinyInput(f).valid).toBe(true);
  });
});
