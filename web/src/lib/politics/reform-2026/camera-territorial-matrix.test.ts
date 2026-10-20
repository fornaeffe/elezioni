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

  it.each([
    ['no-eligible-donor-remainder', fixture([1], [[35]], [2])],
    ['no-eligible-recipient-remainder', fixture([3, 1], [[60, 40]], [2])],
    ['impossible-residual', fixture([1, 1, 2], [[40, 35, 25], [100, 100, 0]], [2, 2])],
  ])('keeps independent ordinary %s cases blocked without fabricated targets', (reason, f) => {
    const r = run(independent(f as TerritorialMatrixInput));
    expect(r).toMatchObject({ status: 'non-computable', reason });
    if (reason === 'no-eligible-recipient-remainder') {
      expect(r.transfers).toHaveLength(1); // A's sole unused decimal cannot be used twice.
      expect(r.cells[0]).toMatchObject({ seats: 2, remainderState: 'used' });
    }
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

  it('does not fabricate an integer-seat donor if corrections have no lawful progress', () => {
    const r = run({ ...fixture([1, 3], [[140, 60], [140, 60]], [2, 2]), correction: 'college-groups',
      indexQuotas: [{ id: 'A', quota: exactFraction(100) }, { id: 'B', quota: exactFraction(100) }] }, first);
    expect(r).toMatchObject({ status: 'non-computable', phase: 'correction', reason: 'no-eligible-donor-remainder' });
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
