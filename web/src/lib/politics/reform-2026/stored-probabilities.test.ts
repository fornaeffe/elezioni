import { describe, expect, it } from 'vitest';
import { emptySummary } from '../../results/statistics';
import { aggregateBonusPairProbabilities } from './stored-probabilities';

const series = (chamber: 'camera' | 'senato', owner: string, name: string, color: string) =>
  emptySummary({ id: `${chamber}:${owner}`, chamber, kind: 'subject', owner, college: null, territory: null, name, color, order: 0 });

describe('stored bonus-pair probabilities', () => {
  it('counts one election-wide award per simulation and shows matching names once', () => {
    const result = aggregateBonusPairProbabilities([
      series('camera', 'coalition:G', 'Lega', '#123456'),
      series('senato', 'coalition:G', 'Lega', '#654321'),
      series('camera', 'coalition:X', 'Partito Uno', '#111111'),
      series('senato', 'coalition:Y', 'Partito Due', '#222222')
    ], [
      { Simulation: 1, Bonus: 'awarded' },
      { Simulation: 2, Bonus: 'not-awarded' },
      { Simulation: 3, Bonus: 'awarded' }
    ], [
      { Simulation: 1, Chamber: 'camera', Subject: 'coalition:G', Status: 'awarded' },
      { Simulation: 1, Chamber: 'senato', Subject: 'coalition:G', Status: 'awarded' },
      { Simulation: 3, Chamber: 'camera', Subject: 'coalition:X', Status: 'awarded' },
      { Simulation: 3, Chamber: 'senato', Subject: 'coalition:Y', Status: 'awarded' }
    ]);

    expect(result).toHaveLength(2);
    expect(result.find(row => row.name === 'Lega')).toMatchObject({ wins: 1, known: 3, color: '#123456' });
    expect(result.find(row => row.name === 'Partito Uno ↔ Partito Due')).toMatchObject({ wins: 1, known: 3 });
  });

  it('rejects duplicate chamber eligibility instead of double-counting an award', () => {
    expect(() => aggregateBonusPairProbabilities([
      series('camera', 'coalition:G', 'Lega', '#123456'),
      series('senato', 'coalition:G', 'Lega', '#654321')
    ], [{ Simulation: 1, Bonus: 'awarded' }], [
      { Simulation: 1, Chamber: 'camera', Subject: 'coalition:G', Status: 'awarded' },
      { Simulation: 1, Chamber: 'camera', Subject: 'coalition:G', Status: 'awarded' }
    ])).toThrow('Duplicate chamber in stored bonus eligibility');
  });
});
