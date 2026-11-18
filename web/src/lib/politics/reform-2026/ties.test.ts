import { describe, expect, it, vi } from 'vitest';
import { exactFraction as f } from './exact';
import type { DrawRequest, ScrutinyExecutionContext } from './result';
import { createSeededDrawContext, resolveTie, type TieAlternative, type TieContext, type TieCriterion } from './ties';

const context: TieContext = { simulationId: 7, request: {
  contextId: 'N13/constructed', chamber: 'camera', norm: 'N13', rule: 'statutory', lawReference: 'C83(1)(f)'
} };
const alternative = (id: string, ...values: number[]): TieAlternative => ({ id, values: values.map((value) => f(value)) });
const descending: TieCriterion = { name: 'electoral-figure', direction: 'descending' };
const neverDraw: ScrutinyExecutionContext = { draw: () => { throw new Error('Unexpected draw'); } };

describe('explicit successive tie criteria', () => {
  it('uses remainder then the specified electoral figure, consuming no draw', () => {
    const result = resolveTie([
      alternative('a', 3, 20), alternative('b', 3, 30), alternative('c', 2, 99)
    ], [{ name: 'remainder', direction: 'descending' }, descending], context, neverDraw);
    expect(result.chosenId).toBe('b');
    expect(result.steps).toEqual([
      { criterion: 'remainder', remainingIds: ['a', 'b'] },
      { criterion: 'electoral-figure', remainingIds: ['b'] }
    ]);
    expect(result.draws).toEqual([]);
    expect(result.warnings).toEqual([]);
  });

  it('respects list presentation order before any fallback draw', () => {
    const result = resolveTie([alternative('z', 10, 1), alternative('a', 10, 2)],
      [descending, { name: 'presentation-order', direction: 'ascending' }],
      { ...context, request: { ...context.request, norm: 'N05', rule: 'I03' } }, neverDraw);
    expect(result.chosenId).toBe('z');
    expect(result.warnings).toEqual([]);
  });

  it('supports exact ascending percentages and opposite age priorities', () => {
    expect(resolveTie([{ id: 'a', values: [f(1, 3)] }, { id: 'b', values: [f(2, 5)] }],
      [{ name: 'list-percentage', direction: 'ascending' }], context, neverDraw).chosenId).toBe('a');
    const dates = [alternative('older', 19700101), alternative('younger', 19800101)];
    expect(resolveTie(dates, [{ name: 'birth-date', direction: 'descending' }], context, neverDraw).chosenId).toBe('younger');
    expect(resolveTie(dates, [{ name: 'birth-date', direction: 'ascending' }], context, neverDraw).chosenId).toBe('older');
  });

  it('draws only from final tied alternatives with a canonical injected request', () => {
    const draw = vi.fn((request: DrawRequest) => ({ ...request, chosenId: 'b', streamId: 'injected' }));
    const result = resolveTie([alternative('b', 3), alternative('z', 2), alternative('a', 3)], [descending], context, { draw });
    expect(draw).toHaveBeenCalledTimes(1);
    expect(draw.mock.calls[0][0].alternativeIds).toEqual(['a', 'b']);
    expect(result.draws[0]).toMatchObject({ ...context.request, chosenId: 'b', streamId: 'injected' });
    expect(result.warnings).toEqual([]); // An express statutory draw is not I03.
  });

  it('replays seeded draws under every input permutation', () => {
    const rows = [alternative('c'), alternative('a'), alternative('b')];
    const permutations = [rows, [rows[0], rows[2], rows[1]], [rows[1], rows[0], rows[2]],
      [rows[1], rows[2], rows[0]], [rows[2], rows[0], rows[1]], [...rows].reverse()];
    const results = permutations.map((rows) => resolveTie(rows, [], context, createSeededDrawContext('seed', 'legal-v1')));
    for (const result of results) expect(result).toEqual(results[0]);
  });

  it('warns and records I03 in each affected simulation; resolved criteria do not warn', () => {
    for (const simulationId of [1, 2, 7]) {
      const result = resolveTie([alternative('a', 3), alternative('b', 3)], [descending], {
        simulationId, request: { ...context.request, rule: 'I03' }
      }, createSeededDrawContext('seed', 'legal-v1'));
      expect(result.warnings).toEqual([expect.objectContaining({
        code: 'I03_DRAW', severity: 'warning', simulationId, interpretationId: 'I03',
        drawContextId: context.request.contextId, chamber: 'camera'
      })]);
      expect(result.draws[0].rule).toBe('I03');
    }
  });

  it('keeps independent seeded streams isolated from other draws', () => {
    const a = createSeededDrawContext('seed', 'legal-v1');
    const b = createSeededDrawContext('seed', 'legal-v1');
    const unrelated = createSeededDrawContext('other-seed', 'other');
    for (let index = 0; index < 10; index += 1) {
      resolveTie([alternative('x'), alternative('y')], [], context, unrelated);
      expect(resolveTie([alternative('a'), alternative('b')], [], context, a))
        .toEqual(resolveTie([alternative('a'), alternative('b')], [], context, b));
    }
  });

  it('rejects an invalid or mutated injected draw trace', () => {
    const rows = [alternative('a'), alternative('b')];
    const draw = createSeededDrawContext('seed', 'legal-v1').draw;
    for (const override of [{ chosenId: 'outsider' }, { contextId: 'wrong' }, { rule: 'I03' }, { streamId: '' }, { alternativeIds: ['b', 'a'] }]) {
      expect(() => resolveTie(rows, [], context, { draw: (request) => ({ ...draw(request), ...override }) as ReturnType<typeof draw> })).toThrow('inconsistent trace');
    }
    expect(() => resolveTie(rows, [], context, { draw: (request) => {
      (request.alternativeIds as string[]).reverse();
      return { ...request, chosenId: 'a', streamId: 'mutated' };
    } })).toThrow('inconsistent trace');
  });

  it('rejects malformed alternatives and criterion values', () => {
    for (const rows of [[], [alternative('')], [alternative('a'), alternative('a')]]) {
      expect(() => resolveTie(rows, [], context, neverDraw)).toThrow(RangeError);
    }
    expect(() => resolveTie([alternative('a')], [descending], context, neverDraw)).toThrow(RangeError);
    expect(() => resolveTie([alternative('a', 1)], [], context, neverDraw)).toThrow(RangeError);
    expect(() => createSeededDrawContext('seed', '')).toThrow(RangeError);
  });
});
