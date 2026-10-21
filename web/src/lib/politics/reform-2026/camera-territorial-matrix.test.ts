import { describe, expect, it } from 'vitest';
import { allocateTerritorialMatrix, type TerritorialMatrixInput } from './territorial-matrix';
import { compareExact, divideExact, exactFraction, truncateToSixDecimals } from './exact';
import type { ScrutinyExecutionContext } from './result';
import { createSeededDrawContext } from './ties';

const noDraw: ScrutinyExecutionContext = { draw() { throw new Error('Unexpected draw'); } };
const first: ScrutinyExecutionContext = { draw(r) { return { ...r, chosenId: r.alternativeIds[0], streamId: 'controlled' }; } };
function fixture(targets: number[] = [3, 3], areas = [[180, 120], [170, 130]], capacities = [3, 3]): TerritorialMatrixInput {
  const names = targets.map((_, i) => String.fromCharCode(65 + i));
  const national = names.map((_, i) => areas.reduce((s, a) => s + a[i], 0));
  return { chamber: 'camera', simulationId: 13, contextId: 'worked-matrix', norm: 'N15', lawReference: 'C83(1)(h)',
    correction: 'constituencies', initialTieFigure: 'national-electoral-figure',
    targets: names.map((id, i) => ({ id, seats: targets[i], votes: national[i] })),
    areas: areas.map((a, i) => ({ id: `T${i}`, seats: capacities[i], entries: a.map((votes, j) => ({ id: names[j], votes, tieVotes: national[j] })) })) };
}
function run(f = fixture(), execution = noDraw) { return allocateTerritorialMatrix(f, execution); }
function complete(f = fixture(), execution = noDraw) {
  const r = run(f, execution);
  if (r.status !== 'complete') throw new Error(`${r.phase}: ${r.reason}`);
  return r;
}
const counts = (r: ReturnType<typeof run>) => r.cells.map(c => [c.areaId, c.targetId, c.seats]);

describe('Camera territorial integer/remainder matrices and legal corrections', () => {
  it('retains an already reconciled matrix without transfers or draws', () => {
    // q=100 in both areas: (2.1,.9) -> (2,1), (.9,1.1) -> (1,1).
    const r = complete(fixture([3, 2], [[210, 90], [90, 110]], [3, 2]));
    expect(counts(r)).toEqual([['T0', 'A', 2], ['T0', 'B', 1], ['T1', 'A', 1], ['T1', 'B', 1]]);
    expect(r.transfers).toEqual([]); expect(r.draws).toEqual([]);
    expect(r.areas.map(a => a.quota)).toEqual([exactFraction(100), exactFraction(100)]);
  });

  it('uses the smallest used donor decimal and largest unused recipient in the same constituency', () => {
    // q=100: provisional (2,1),(2,1). A has one excess, B one deficit.
    // Remove T1 A's .7 (before T0 .8), assign T1 B's unused .3.
    const r = complete();
    expect(counts(r)).toEqual([['T0', 'A', 2], ['T0', 'B', 1], ['T1', 'A', 1], ['T1', 'B', 2]]);
    expect(r.transfers).toEqual([{ fromCellId: '["T1","A"]', toCellId: '["T1","B"]', route: 'same-area',
      fromSeatsBefore: 2, toSeatsBefore: 1, drawContextIds: [], remainders: [
        { id: '["T1","A"]', value: exactFraction(7, 10), before: 'used', after: 'consumed' },
        { id: '["T1","B"]', value: exactFraction(3, 10), before: 'unused', after: 'used' }] }]);
    expect(r.areas.map(a => a.finalSeats)).toEqual([3, 3]);
    expect(r.cells.find(c => c.id === '["T1","A"]')).toMatchObject({ provisionalSeats: 2, seats: 1, remainderState: 'consumed' });
  });

  it('skips smaller donor decimals without a local recipient before using the cross-area fallback', () => {
    // q=50: T0 and T1 use A/B, T2 uses A/C. Only T2 has unused B.
    const r = complete(fixture([2, 3, 1], [[40, 35, 25], [45, 40, 15], [47, 22, 31]], [2, 2, 2]));
    expect(r.transfers.map(t => [t.fromCellId, t.toCellId, t.route])).toEqual([
      ['["T2","A"]', '["T2","B"]', 'same-area']]);
    expect(r.areas.map(a => a.finalSeats)).toEqual([2, 2, 2]);
  });

  it('uses the prescribed cross-constituency fallback and records changed local totals', () => {
    // T0/T1 q=50 use A/B. T2 q=100 has integer A/C, uses C .55.
    // A total=3 vs target2; B total=2 vs target3. Both donor areas already
    // used B. Remove T0 A .8, assign T2 B .45: local totals 1,2,4.
    const f = fixture([2, 3, 2], [[40, 35, 25], [45, 40, 15], [100, 45, 155]], [2, 2, 3]);
    const r = complete(f);
    expect(r.transfers[0]).toMatchObject({ fromCellId: '["T0","A"]', toCellId: '["T2","B"]', route: 'cross-area' });
    expect(r.areas.map(a => [a.capacity, a.finalSeats])).toEqual([[2, 1], [2, 2], [3, 4]]);
    expect(f.targets.map(t => r.cells.filter(c => c.targetId === t.id).reduce((s, c) => s + c.seats, 0))).toEqual([2, 3, 2]);
  });

  it('excludes reached integer targets from residual seats in every area', () => {
    // A already has its two seats in integer parts. Its .9 in T0 is excluded.
    const r = complete(fixture([2, 1, 1], [[190, 10, 0], [110, 50, 40]], [2, 2]));
    expect(r.cells.filter(c => c.targetId === 'A').map(c => [c.remainderExcluded, c.remainderSeat, c.seats]))
      .toEqual([[true, false, 1], [true, false, 1]]);
    expect(r.cells.find(c => c.id === '["T0","B"]')).toMatchObject({ remainderSeat: true, provisionalSeats: 1, seats: 0 });
  });

  it.each(['N15', 'N18'] as const)('%s uses the prescribed upper-level votes after equal initial decimals', norm => {
    const figure = norm === 'N15' ? 'national-electoral-figure' : 'constituency-electoral-figure';
    const r = complete({ ...fixture([2, 3], [[150, 150], [90, 110]], [3, 2]), norm,
      correction: norm === 'N15' ? 'constituencies' : 'colleges', initialTieFigure: figure });
    expect(r.cells.find(c => c.id === '["T0","B"]')?.remainderSeat).toBe(true);
    expect(r.resolutions[0].steps.map(s => s.criterion)).toEqual(['decimal-part', figure]);
  });

  it('uses constituency votes for initial coalition-list ties', () => {
    // T0 q=100: A=1.5, B=.5; local A wins, national B would win.
    const g = fixture([2, 2], [[150, 50], [10, 190]], [2, 2]);
    const r = complete({ ...g, initialTieFigure: 'constituency-electoral-figure', norm: 'N16',
      areas: g.areas.map(a => ({ ...a, entries: a.entries.map(e => ({ ...e, tieVotes: e.votes })) })) });
    expect(r.cells.find(c => c.id === '["T0","A"]')?.remainderSeat).toBe(true);
    expect(r.resolutions[0].steps.map(s => s.criterion)).toEqual(['decimal-part', 'constituency-electoral-figure']);
  });

  it('prioritizes the larger surplus before the national electoral figure', () => {
    const f = fixture([1, 3, 4], [[80, 90, 30], [80, 90, 30], [80, 90, 30], [80, 90, 30]], [2, 2, 2, 2]);
    const r = complete(f, first); // B has more national votes but fewer excess seats.
    expect(r.transfers.slice(0, 3).every(t => t.fromCellId?.endsWith(',"A"]'))).toBe(true);
    expect(r.resolutions.find(r => r.steps.some(s => s.criterion === 'surplus-seats'))?.steps.map(s => s.criterion)).toEqual(['surplus-seats']);
  });

  it('uses warned I03 draws for tied cross-area destinations', () => {
    const f = fixture([3, 3, 4], [[40, 35, 25], [45, 40, 15], [100, 45, 155], [100, 45, 155]], [2, 2, 3, 3]);
    const r = complete(f, first);
    const draw = r.draws.find(d => d.contextId.endsWith('/recipient'));
    expect(draw).toMatchObject({ rule: 'I03', alternativeIds: ['["T2","B"]', '["T3","B"]'] });
    expect(r.transfers[0].drawContextIds).toContain(draw?.contextId);
  });

  it('records statutory initial draws separately from warned I03 correction draws', () => {
    const r = complete(fixture([3, 3], [[150, 150], [150, 150]], [3, 3]), first);
    expect(r.draws.some(d => d.rule === 'statutory')).toBe(true);
    expect(r.draws.some(d => d.rule === 'I03')).toBe(true);
    expect(r.warnings).toHaveLength(r.draws.filter(d => d.rule === 'I03').length);
    expect(r.warnings.every(w => w.interpretationId === 'I03' && w.severity === 'warning')).toBe(true);
  });

  it('ranks constituency surplus subjects by excess then national votes', () => {
    const f = fixture([2, 2, 4], [[80, 70, 50], [80, 70, 50], [80, 70, 50], [80, 70, 50]], [2, 2, 2, 2]);
    // Every area q100 gives A/B one residual each: surpluses A=2,B=2.
    const r = complete(f, first);
    expect(r.resolutions.find(r => r.steps.some(s => s.criterion === 'national-electoral-figure'))?.chosenId).toBe('A');
    expect(r.transfers.slice(0, 2).every(t => t.fromCellId?.endsWith(',"A"]'))).toBe(true);
    expect(r.cells.filter(c => c.targetId === 'C').reduce((s, c) => s + c.seats, 0)).toBe(4);
  });

  it.each(['N15', 'N16'] as const)('%s uses national votes for tied same-constituency correction recipients', norm => {
    // q100, T0 A .8 wins, B/C both .1 unused; other donor decimals are .9.
    const f = fixture([3, 2, 1], [[80, 10, 10], [140, 50, 10], [190, 5, 5], [90, 5, 5]], [1, 2, 2, 1]);
    const r = complete({ ...f, norm, initialTieFigure: norm === 'N16' ? 'constituency-electoral-figure' : 'national-electoral-figure',
      areas: f.areas.map(a => ({ ...a, entries: a.entries.map(e => ({ ...e, tieVotes: norm === 'N16' ? e.votes : e.tieVotes })) })) }, first);
    const choice = r.resolutions.find(r => r.steps.some(s => s.criterion === 'recipient-decimal-part'));
    expect(choice?.steps.map(s => s.criterion)).toEqual(['recipient-decimal-part', 'national-electoral-figure']);
    expect(r.transfers[0]).toMatchObject({ fromCellId: '["T0","A"]', toCellId: '["T0","B"]' });
  });

  it('draws equal surplus counts and national figures under I03', () => {
    const f = fixture([2, 2, 4], [[80, 80, 40], [80, 80, 40], [80, 80, 40], [80, 80, 40]], [2, 2, 2, 2]);
    const r = complete(f, first);
    expect(r.draws.find(d => d.contextId.endsWith('/surplus-1'))).toMatchObject({ rule: 'I03', alternativeIds: ['A', 'B'], chosenId: 'A' });
    expect(new Set(r.transfers.map(t => t.fromCellId)).size).toBe(r.transfers.length);
    expect(new Set(r.transfers.map(t => t.toCellId)).size).toBe(r.transfers.length);
  });

  it('selects college donors/recipients independently, allowing college totals to change', () => {
    const f = fixture([2, 3, 1], [[40, 35, 25], [45, 40, 15], [47, 22, 31]], [2, 2, 2]);
    const r = complete({ ...f, norm: 'N18', correction: 'colleges', initialTieFigure: 'constituency-electoral-figure' });
    // Unlike h, b(3) removes A's smallest .8 from T0, assigns B's .44 in T2.
    expect(r.transfers[0]).toMatchObject({ fromCellId: '["T0","A"]', toCellId: '["T2","B"]', route: 'cross-area' });
    expect(r.areas.map(a => a.finalSeats)).toEqual([1, 2, 3]);
  });

  it('keeps cap-group transfers in the same college', () => {
    const f = fixture([1, 3], [[60, 140], [60, 140]], [2, 2]);
    // indices .6/1.4 normalized to 2 seats: both areas A1/B1.
    const r = complete({ ...f, correction: 'college-groups', norm: 'N18',
      indexQuotas: [{ id: 'A', quota: exactFraction(100) }, { id: 'B', quota: exactFraction(100) }] }, first);
    expect(r.transfers[0]).toMatchObject({ route: 'same-area', fromCellId: '["T0","A"]', toCellId: '["T0","B"]' });
    expect(r.areas.map(a => a.finalSeats)).toEqual([2, 2]);
  });

  const independent = (f: TerritorialMatrixInput): TerritorialMatrixInput => ({ ...f, chamber: 'camera', norm: 'N18',
    lawReference: 'C83-bis(1)(b)(1)-(3)', correction: 'independent-colleges', initialTieFigure: 'constituency-electoral-figure' });

  it('adds a missing ordinary seat without a donor, retaining the decree quota', () => {
    const f = independent(fixture([2, 1], [[60, 40]], [2]));
    const before = structuredClone(f), r = complete(f);
    expect(counts(r)).toEqual([['T0', 'A', 2], ['T0', 'B', 1]]);
    expect(r.areas).toEqual([{ id: 'T0', capacity: 2, rawQuota: exactFraction(50), quota: exactFraction(50), finalSeats: 3 }]);
    expect(r.transfers).toEqual([{ fromCellId: null, toCellId: '["T0","A"]', route: 'addition',
      fromSeatsBefore: null, toSeatsBefore: 1,
      remainders: [{ id: '["T0","A"]', value: exactFraction(1, 5), before: 'unused', after: 'used' }], drawContextIds: [] }]);
    expect(f).toEqual(before);
  });

  it('removes the smallest used ordinary remainder without a recipient', () => {
    const r = complete(independent(fixture([1, 2], [[40, 60], [45, 55]], [2, 2])));
    expect(counts(r)).toEqual([['T0', 'A', 0], ['T0', 'B', 1], ['T1', 'A', 1], ['T1', 'B', 1]]);
    expect(r.transfers).toEqual([{ fromCellId: '["T0","A"]', toCellId: null, route: 'removal',
      fromSeatsBefore: 1, toSeatsBefore: null,
      remainders: [{ id: '["T0","A"]', value: exactFraction(4, 5), before: 'used', after: 'consumed' }], drawContextIds: [] }]);
    expect(r.areas.map(a => [a.capacity, a.finalSeats])).toEqual([[2, 1], [2, 2]]);
  });

  it('corrects ordinary surpluses and deficits independently even when both exist', () => {
    const r = complete(independent(fixture([2, 3, 1], [[40, 35, 25], [45, 40, 15], [47, 22, 31]], [2, 2, 2])));
    expect(r.transfers.map(t => [t.route, t.fromCellId, t.toCellId])).toEqual([
      ['removal', '["T0","A"]', null], ['addition', null, '["T2","B"]']]);
    expect(r.areas.map(a => a.finalSeats)).toEqual([1, 2, 3]);
  });

  it('keeps initial impossible residuals blocked', () => {
    expect(run(independent(fixture([1, 1], [[100, 100]], [3]))))
      .toMatchObject({ status: 'non-computable', reason: 'impossible-residual' });
  });

  it('I16 reuses a single ordinary recipient repeatedly after its unused addition', () => {
    const r = complete(independent(fixture([6, 1], [[60, 40]], [2])));
    expect(r.cells[0]).toMatchObject({ integerSeats: 1, provisionalSeats: 1, seats: 6, remainderState: 'used', decimalPart: exactFraction(1, 5) });
    expect(r.transfers).toHaveLength(5);
    expect(r.transfers[0].interpretationId).toBeUndefined();
    expect(r.transfers.slice(1).every(t => t.interpretationId === 'I16' && t.remainders[0].before === 'used' && t.remainders[0].after === 'used')).toBe(true);
    expect(r.warnings.filter(w => w.interpretationId === 'I16')).toHaveLength(4);
  });

  it('I16 exhausts unused recipients before larger used decimals and stops at each target', () => {
    const r = complete(independent(fixture([4, 3, 1], [[400, 90, 10]], [5])));
    expect(r.transfers.map(t => t.toCellId)).toEqual(['["T0","C"]', '["T0","B"]', '["T0","B"]']);
    expect(r.transfers.map(t => t.interpretationId)).toEqual([undefined, 'I16', 'I16']);
    expect(r.cells.map(c => c.seats)).toEqual([4, 3, 1]);
  });

  it('I16 repeats descending passes across colleges rather than repeatedly choosing just the greatest decimal', () => {
    const r = complete(independent(fixture([1, 7], [[120, 80], [130, 70]], [2, 2])));
    expect(r.transfers.filter(t => t.interpretationId === 'I16').map(t => t.toCellId))
      .toEqual(['["T0","B"]', '["T1","B"]', '["T0","B"]', '["T1","B"]', '["T0","B"]']);
    expect(r.cells.filter(c => c.targetId === 'B').map(c => c.seats)).toEqual([4, 3]);
  });

  it('I16 preserves explicit failure when deficient lists have no eligible computed decimal', () => {
    const f = independent(fixture([0, 4], [[400, 0]], [4]));
    const absent = { ...f, areas: f.areas.map(a => ({ ...a, entries: a.entries.map(c => ({ ...c, eligible: c.id !== 'B' })) })) };
    expect(run(absent)).toMatchObject({ status: 'non-computable', reason: 'no-eligible-recipient-remainder' });
  });

  it('I16 replays tied reuse across repeated passes and permutations with unchanged JSON evidence', () => {
    const f = independent(fixture([2, 6], [[140, 60], [140, 60]], [2, 2]));
    const seeded = (input: TerritorialMatrixInput) => complete(input, createSeededDrawContext('I16-replay', 'legal'));
    const r = seeded(f);
    expect(r.draws.some(d => d.rule === 'I03')).toBe(true);
    expect(r.warnings.filter(w => w.interpretationId === 'I16')).toHaveLength(4);
    expect(seeded({ ...f, targets: [...f.targets].reverse(), areas: [...f.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) })).toEqual(r);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });

  it.each([['camera', 'N18'], ['senato', 'N21'], ['senato', 'N22']] as const)(
    'I16 repeatedly pairs %s %s recipients with I13 donors without overshooting targets', (chamber, norm) => {
      const r = complete({ ...fixture([1, 7], [[360, 40], [350, 50]], [4, 4]), chamber, norm,
        correction: 'colleges', initialTieFigure: chamber === 'camera' ? 'constituency-electoral-figure' : 'college-electoral-figure' });
      expect(r.transfers).toHaveLength(5);
      expect(r.transfers.every(t => t.interpretationId === 'I13' && JSON.stringify(t.interpretationIds) === '["I13","I16"]')).toBe(true);
      expect(r.transfers.map(t => t.toCellId)).toEqual(['["T1","B"]', '["T0","B"]', '["T1","B"]', '["T0","B"]', '["T1","B"]']);
      expect(r.cells.every(c => c.seats >= 0)).toBe(true);
      expect(r.cells.filter(c => c.targetId === 'B').reduce((n, c) => n + c.seats, 0)).toBe(7);
      expect(r.warnings.filter(w => w.interpretationId === 'I16')).toHaveLength(5);
    });

  it.each(['independent-colleges', 'colleges'] as const)('I14 repeats initial list remainders in %s, including cap-internal lists', correction => {
    const f = { ...independent(fixture([1, 1, 2], [[40, 35, 25], [100, 100, 0]], [2, 2])), correction };
    const r = complete(f);
    expect(r.cells.find(c => c.areaId === 'T0' && c.targetId === 'C')).toMatchObject({
      integerSeats: 0, provisionalSeats: 2, seats: 2, remainderSeat: true, remainderState: 'used', decimalPart: exactFraction(1, 2) });
    expect(r.areas.map(a => a.finalSeats)).toEqual([2, 2]);
    expect(r.warnings).toEqual([expect.objectContaining({ code: 'I14_REPEATED_REMAINDER_PASS', interpretationId: 'I14', severity: 'warning' })]);
    expect(r.transfers).toEqual([]);
    expect(r.draws).toEqual([]);
  });

  it('I14 supports more than two passes and preserves the existing JSON contract', () => {
    const f = independent(fixture([1, 1, 1, 3], [[25, 25, 25, 25], [100, 100, 100, 0]], [3, 3]));
    const r = complete(f);
    expect(r.cells.find(c => c.areaId === 'T0' && c.targetId === 'D')).toMatchObject({ provisionalSeats: 3, seats: 3, remainderState: 'used' });
    expect(r.warnings.filter(w => w.interpretationId === 'I14')).toHaveLength(2);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });

  it('I14 excludes newly reached targets before another pass', () => {
    const f = independent(fixture([1, 2, 1, 1, 1], [[21, 19, 20, 20, 20], [0, 0, 100, 100, 100]], [3, 3]));
    const r = complete(f);
    expect(r.cells.filter(c => c.areaId === 'T0').map(c => c.seats)).toEqual([1, 2, 0, 0, 0]);
    expect(r.transfers).toEqual([]);
  });

  it('I14 retains established tie order across passes and permutations without redrawing', () => {
    const f = independent(fixture([2, 1, 1, 1, 1], [[20, 20, 20, 20, 20], [0, 0, 100, 100, 100]], [3, 3]));
    const r = complete(f, first);
    expect(r.draws).toHaveLength(1); // A/B tie is drawn only in the first pass.
    expect(r.cells.filter(c => c.areaId === 'T0').map(c => c.seats)).toEqual([2, 1, 0, 0, 0]);
    const permuted = { ...f, targets: [...f.targets].reverse(), areas: [...f.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) };
    expect(complete(permuted, first)).toEqual(r);
  });

  it('I14 removes each repeated initial award before integer fallback and preserves final recovery state', () => {
    const f = independent(fixture([1, 4, 1, 1, 1, 1], [
      [20, 19, 16, 15, 15, 15], [20, 19, 16, 15, 15, 15], [20, 19, 16, 15, 15, 15], [0, 0, 100, 100, 100, 100]
    ], [3, 3, 3, 4]));
    const r = complete(f, first);
    const removals = r.transfers.filter(t => t.interpretationId === 'I14');
    expect(removals).toHaveLength(2);
    expect(removals.map(t => t.fromCellId)).toEqual(['["T0","B"]', '["T0","B"]']);
    expect(removals.map(t => t.remainders[0])).toMatchObject([
      { before: 'used', after: 'used' }, { before: 'used', after: 'consumed' }]);
    expect(r.cells.find(c => c.areaId === 'T0' && c.targetId === 'B')).toMatchObject({
      integerSeats: 0, provisionalSeats: 2, seats: 0, remainderState: 'consumed' });
    expect(r.warnings.some(w => w.interpretationId === 'I13')).toBe(false);
    expect(f.targets.map(t => r.cells.filter(c => c.targetId === t.id).reduce((s, c) => s + c.seats, 0))).toEqual([1, 4, 1, 1, 1, 1]);
  });

  it('I14 stops if every list reaches its communicated target before college capacity is filled', () => {
    const r = run(independent(fixture([1, 1, 1, 1], [[21, 19, 30, 30], [0, 0, 100, 100]], [3, 2])));
    expect(r).toMatchObject({ status: 'non-computable', phase: 'remainders', reason: 'impossible-residual' });
    expect(r.warnings.some(w => w.interpretationId === 'I14')).toBe(false);
  });

  it('does not emit I14 when one ordinary remainder pass suffices', () => {
    expect(complete(independent(fixture())).warnings.some(w => w.interpretationId === 'I14')).toBe(false);
  });

  it('retains strict capacity checks outside independent Camera list corrections', () => {
    const f = fixture([2, 1], [[60, 40]], [2]);
    for (const correction of ['constituencies', 'colleges', 'college-groups'] as const) {
      expect(() => run({ ...f, correction })).toThrow(/capacities must sum/);
    }
    expect(() => run({ ...independent(f), chamber: 'senato' })).toThrow(/Camera N18/);
    expect(() => run({ ...independent(f), norm: 'N17' })).toThrow(/Camera N18/);
  });

  it('replays independent correction draws, remainder states and nullable traces after permutations', () => {
    const f = independent(fixture([1, 3], [[60, 140], [60, 140]], [2, 2]));
    const r = complete(f, createSeededDrawContext('independent-replay', 'legal'));
    expect(r.draws).toHaveLength(2);
    expect(r.draws.every(d => d.rule === 'I03')).toBe(true);
    for (const t of r.transfers) expect(t.drawContextIds).toHaveLength(1);
    const permuted = { ...f, targets: [...f.targets].reverse(), areas: [...f.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) };
    expect(complete(permuted, createSeededDrawContext('independent-replay', 'legal'))).toEqual(r);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });

  it('truncates every active index at six decimals before local normalization', () => {
    const f = fixture([1, 1], [[1000000, 2000000], [500001, 1000003]], [1, 1]);
    const r = complete({ ...f, norm: 'N17', indexQuotas: [
      { id: 'A', quota: exactFraction(1500001) }, { id: 'B', quota: exactFraction(3000003) }] }, first);
    expect(r.cells[0]).toMatchObject({ rawIndex: exactFraction(1000000, 1500001), index: exactFraction(666666, 1000000), quotient: exactFraction(1, 2) });
    expect(r.cells[1].index).toEqual(exactFraction(666666, 1000000));
    // Untruncated raw indices would award T0 to A. Truncation ties them and B
    // wins on the larger national electoral figure: a changed provisional seat.
    expect(compareExact(r.cells[0].rawIndex!, r.cells[1].rawIndex!)).toBe(1);
    expect(r.cells.find(c => c.id === '["T0","B"]')?.remainderSeat).toBe(true);
  });

  it('applies minority truncation symmetrically rather than just truncating the majority', () => {
    const f = fixture([3, 1], [[1000000, 2000000], [3500003, 1000003]], [1, 3]);
    const g = { ...f, norm: 'N17' as const, indexQuotas: [{ id: 'A', quota: exactFraction(1500001) }, { id: 'B', quota: exactFraction(3000003) }] };
    const r = complete(g, first);
    const [a, b] = r.cells;
    expect(compareExact(a.index!, b.rawIndex!)).toBe(-1); // majority-only would lose
    expect(a.index).toEqual(b.index); // symmetric truncation: votes break the tie
    expect(a.remainderSeat).toBe(true); // A national 4,500,003 > B 3,000,003
    expect(truncateToSixDecimals(exactFraction(1999999, 1000000))).toEqual(exactFraction(1999999, 1000000));
    expect(truncateToSixDecimals(exactFraction(19999999, 10000000))).toEqual(exactFraction(1999999, 1000000));
    expect(divideExact(a.index!, b.index!)).toEqual(exactFraction(1));
  });

  it('skips the quota of a zero-target cap group', () => {
    const r = complete({ ...fixture([2, 0], [[100, 50]], [2]), correction: 'college-groups',
      indexQuotas: [{ id: 'A', quota: exactFraction(50) }, { id: 'B', quota: null }] });
    expect(r.cells[1]).toMatchObject({ rawIndex: null, index: exactFraction(0), seats: 0 });
  });

  it.each([
    ['zero-vote-total', fixture([1, 1], [[0, 0]], [2])],
    ['zero-quota', fixture([2, 1], [[1, 1]], [3])],
    ['initial-overallocation', fixture([3, 2, 2], [[10, 1, 1]], [7])],
  ])('reports I04 %s before consuming draws', (reason, input) => {
    expect(run(input as TerritorialMatrixInput)).toMatchObject({ status: 'non-computable', phase: 'initial', reason, draws: [] });
  });

  it('preflights impossible residual pools before any draw', () => {
    const r = run(fixture([1, 1, 2], [[40, 35, 25], [100, 100, 0]], [2, 2]));
    expect(r).toMatchObject({ status: 'non-computable', phase: 'remainders', reason: 'impossible-residual', draws: [] });
  });

  it('blocks positive target groups with a zero quota', () => {
    const r = run({ ...fixture([1, 1], [[1, 1]], [2]), indexQuotas: [{ id: 'A', quota: exactFraction(0) }, { id: 'B', quota: exactFraction(1) }] });
    expect(r).toMatchObject({ status: 'non-computable', reason: 'zero-index-quota', draws: [] });
  });

  it('I13 completes the Basilicata integer-donor gap without consuming its unused decimal', () => {
    const r = complete(independent(fixture([1, 1, 1, 1], [[88446, 45804, 28436, 7630]], [4])));
    expect(r.cells.map(c => c.seats)).toEqual([1, 1, 1, 1]);
    expect(r.areas[0]).toMatchObject({ quota: exactFraction(42579), finalSeats: 4 });
    expect(r.transfers[0]).toMatchObject({ route: 'removal', interpretationId: 'I13',
      remainders: [{ before: 'unused', after: 'unused' }] });
    expect(r.transfers[1]).toMatchObject({ route: 'addition', toCellId: '["T0","D"]' });
    expect(r.warnings).toEqual([expect.objectContaining({ code: 'I13_INTEGER_SEAT_REMOVAL', severity: 'warning', interpretationId: 'I13' })]);
  });

  it('I13 can repeatedly remove integer seats from one college without negative seats', () => {
    const r = complete(independent(fixture([1], [[400]], [4])));
    expect(r.cells[0]).toMatchObject({ integerSeats: 4, seats: 1, remainderState: 'unused' });
    expect(r.transfers.map(t => t.fromSeatsBefore)).toEqual([4, 3, 2]);
    expect(r.warnings).toHaveLength(3);
    expect(r.areas[0]).toMatchObject({ capacity: 4, finalSeats: 1 });
  });

  it.each(['camera', 'senato'] as const)('I13 supports the %s cap intra-group list rule', chamber => {
    const r = complete({ ...independent(fixture([1, 1, 1, 1], [[88446, 45804, 28436, 7630]], [4])),
      chamber, norm: chamber === 'camera' ? 'N18' : 'N22', correction: 'colleges' });
    expect(r.transfers[0]).toMatchObject({ interpretationId: 'I13', fromCellId: '["T0","A"]', toCellId: '["T0","D"]' });
    expect(r.cells.map(c => c.seats)).toEqual([1, 1, 1, 1]);
  });

  it('I13 supports cap-group corrections while preserving each college total', () => {
    const f: TerritorialMatrixInput = { ...fixture([1, 3], [[140, 60], [130, 70]], [2, 2]), norm: 'N18', correction: 'college-groups',
      initialTieFigure: 'constituency-electoral-figure',
      indexQuotas: [{ id: 'A', quota: exactFraction(100) }, { id: 'B', quota: exactFraction(100) }] };
    const r = complete(f);
    expect(r.transfers[0]).toMatchObject({ fromCellId: '["T1","A"]', toCellId: '["T1","B"]', interpretationId: 'I13', route: 'same-area' });
    expect(r.areas.map(a => a.finalSeats)).toEqual([2, 2]);
    expect(r.cells.filter(c => c.targetId === 'A').map(c => c.seats)).toEqual([1, 0]);
    expect(r.warnings.filter(w => w.code === 'I13_INTEGER_SEAT_REMOVAL')).toHaveLength(1);
  });

  it('I13 exhausts strict donors first, then ranks integer donors, preserving consumed evidence', () => {
    // A has an integer surplus; B has two remainder seats for a target of one.
    // C has room for both corrections. B's used decimal must be removed first.
    const f = independent(fixture([1, 1, 4], [[250, 40, 10], [0, 60, 140]], [3, 3]));
    const r = complete(f);
    expect(r.transfers.filter(t => t.route === 'removal').map(t => [t.fromCellId, t.interpretationId]))
      .toEqual([['["T0","B"]', undefined], ['["T0","A"]', 'I13']]);
    expect(r.warnings.filter(w => w.interpretationId === 'I13')).toHaveLength(1);
  });

  it('keeps constituency donors strict in either chamber', () => {
    const f = fixture([1, 3], [[140, 60], [130, 70]], [2, 2]);
    for (const input of [f, { ...f, chamber: 'senato' as const, norm: 'N21' as const }]) {
      const r = run(input);
      expect(r).toMatchObject({ status: 'non-computable', reason: 'no-eligible-donor-remainder' });
      expect(r.warnings.some(w => w.interpretationId === 'I13')).toBe(false);
    }
  });

  it('replays I13 tied integer removals with unchanged decimal states after permutations', () => {
    const f = independent(fixture([1], [[200], [200]], [2, 2]));
    const runSeeded = (input: TerritorialMatrixInput) => complete(input, createSeededDrawContext('I13-replay', 'legal'));
    const r = runSeeded(f);
    expect(r.warnings.some(w => w.interpretationId === 'I13')).toBe(true);
    expect(r.draws.every(d => d.rule === 'I03')).toBe(true);
    expect(runSeeded({ ...f, targets: [...f.targets].reverse(), areas: [...f.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) })).toEqual(r);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });

  it('replays with canonical traces after area, subject and figure permutations', () => {
    const f = fixture([3, 3], [[150, 150], [150, 150]], [3, 3]);
    const r = complete(f, createSeededDrawContext('matrix-replay', 'legal'));
    const permuted = { ...f, targets: [...f.targets].reverse(), areas: [...f.areas].reverse().map(a => ({ ...a, entries: [...a.entries].reverse() })) };
    expect(complete(permuted, createSeededDrawContext('matrix-replay', 'legal'))).toEqual(r);
    expect(JSON.parse(JSON.stringify(r))).toEqual(r);
  });

  it('owns trace arrays and never mutates input figures', () => {
    const f = fixture(); const before = structuredClone(f); const r = complete(f);
    expect(f).toEqual(before);
    expect(r.targets).not.toBe(f.targets); expect(r.targets[0]).not.toBe(f.targets[0]);
  });
});
