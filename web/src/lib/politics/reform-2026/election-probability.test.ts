import { expect, it } from 'vitest';
import { completeScrutinyFixture } from './scrutiny.fixture';
import { generationFixture } from './generation.fixture';
import { runPoliticsReformScrutiny, type ComposedReformScrutinyResult } from './scrutiny';
import { electionPositionRows, positionAccumulator, positionColumns, probabilityLabel } from './election-probability';

it('counts the preference rank and final election location, including a recovered seat but excluding a winner elsewhere', () => {
  const input = completeScrutinyFixture(60), { snapshot } = generationFixture();
  if (input.renewal.kind !== 'joint') throw new Error();
  const slate = input.renewal.camera.ordinarySlates[0];
  const counts = slate.candidateIds.slice(1).map((candidateId, i) => ({ candidateId, count: i + 1 }));
  const result = runPoliticsReformScrutiny(input);
  if (slate.preferences.kind === 'missing') throw new Error();
  slate.preferences = { ...slate.preferences, counts };
  const first = slate.candidateIds.at(-1)!;
  const winner = result.seats.find(s => s.status === 'filled' && s.entitlement.chamber === 'camera')!;
  const seat = { ...winner, status: 'filled' as const, candidateId: first, category: 'bonus' as const,
    electedAt: { ...winner.entitlement, collegeId: slate.collegeId, listId: slate.listId } };
  let modified: ComposedReformScrutinyResult = { ...result, seats: [seat] };
  const find = () => electionPositionRows(input, modified, snapshot).find(r => r.Chamber === 'camera' && r.College === slate.collegeId && r.List === slate.listId)!;
  expect(find()['Preference 1']).toBe(1);
  expect(find().Capolista).toBe(0);
  seat.electedAt.collegeId = 'another-college';
  expect(find()['Preference 1']).toBe(0);
  modified = { ...modified, seats: [{ ...seat, status: 'unresolved', reason: 'missing evidence' }] };
  expect(find()['Preference 1']).toBeNull();
  expect(find().Capolista).toBeNull();
});

it('keeps absent ranks, missing preferences and partial-run denominators explicit', () => {
  const input = completeScrutinyFixture(60), { snapshot } = generationFixture();
  if (input.renewal.kind !== 'joint') throw new Error();
  const slate = input.renewal.camera.ordinarySlates[0];
  slate.preferences = { kind: 'missing', reason: 'No preferences' };
  const rows = electionPositionRows(input, runPoliticsReformScrutiny(input), snapshot);
  const row = rows.find(r => r.Chamber === 'camera' && r.College === slate.collegeId && r.List === slate.listId)!;
  expect(row['Preference 1']).toBeNull();
  const accumulator = positionAccumulator(3);
  accumulator.add({ ...row, Simulation: 1, Capolista: 1, 'Preference 1': null, 'Preference 6': 'absent' });
  accumulator.add({ ...row, Simulation: 2, Capolista: 0, 'Preference 1': 0, 'Preference 6': 'absent' });
  const cells = accumulator.rows()[0].positions;
  expect(cells).toHaveLength(7);
  expect(probabilityLabel(cells[0], 3)).toBe('≥ 33,3%');
  expect(probabilityLabel(cells[1], 3)).toBe('≥ 0%');
  expect(probabilityLabel(cells[6], 2)).toBe('—');
  expect(probabilityLabel({ wins: 0, known: 0, absent: 0 }, 3)).toBe('n.d.');
  expect(positionColumns).toHaveLength(7);
  expect(() => accumulator.add({ ...row, Simulation: 2 })).toThrow(/Duplicate/);
  expect(() => positionAccumulator(3).add({ ...row, Simulation: 4 })).toThrow(/invalid/);
  expect(() => positionAccumulator(3).add({ ...row, Simulation: 1, Capolista: 2 })).toThrow(/Invalid election position outcome/);
});
