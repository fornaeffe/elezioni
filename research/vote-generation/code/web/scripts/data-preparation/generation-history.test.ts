import { existsSync } from 'node:fs';
import { resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { describe, expect, it } from 'vitest';
import { aggregateGenerationEvent, generationRegionId, loadGenerationHistory, matchGenerationEvents, type GenerationVoteRecord } from './generation-history';
import { readCalibrationResults } from './poll-calibration-sources';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const panel = { pd: ['PD'], m5s: ['M5S'], lega: ['LEGA'] };
const record = (overrides: Partial<GenerationVoteRecord> = {}): GenerationVoteRecord => ({ region: 'LOMBARDIA 1', province: 'MILANO', name: 'MILANO',
  ballot: 'college-1', electors: 100, voters: 80, list: 'PD', votes: 25, ...overrides });

describe('generation municipality-history boundary', () => {
  it('counts repeated denominators once per ballot and retains every original list', () => {
    const r = aggregateGenerationEvent([record(), record({ list: 'M5S', votes: 10 }), record({ list: 'LEGA', votes: 20 }),
      record({ list: 'new-list', votes: 15 }), record({ ballot: 'college-2', list: 'PD', electors: 20, voters: 15, votes: 10 })], panel);
    expect(r.municipalities[0]).toMatchObject({ electors: 120, voters: 95, validVotes: 80, votes: [35, 10, 20, 15] });
    expect(r.municipalities[0].lists).toHaveLength(4);
    expect(r.mapping.find(m => m.name === 'new-list')).toEqual({ name: 'new-list', party: 'other', method: 'changing-complement' });
    expect(r.national.validVotes).toBe(80);
  });
  it('rejects duplicate list rows and inconsistent repeated denominators', () => {
    expect(() => aggregateGenerationEvent([record(), record()], { pd: ['PD'], m5s: [], lega: [] })).toThrow('Duplicate');
    expect(() => aggregateGenerationEvent([record(), record({ list: 'other', electors: 101 })], { pd: ['PD'], m5s: [], lega: [] })).toThrow('Inconsistent');
    expect(() => aggregateGenerationEvent([record()], panel)).toThrow('Missing declared');
    expect(() => aggregateGenerationEvent([record()], { pd: ['PD'], m5s: ['PD'], lega: [] })).toThrow('Overlapping');
  });
  it('retains anomalous official totals nationally while excluding invalid local turnout', () => {
    const r = aggregateGenerationEvent([record({ voters: 110, votes: 105 })], { pd: ['PD'], m5s: [], lega: [] });
    expect(r.national).toMatchObject({ electors: 100, voters: 110, validVotes: 105 });
    expect(r.municipalities[0].matchable).toBe(false);
  });
  it('does not guess province for ambiguous names or allocate new-party votes before existence', () => {
    const r = aggregateGenerationEvent([record({ province: null })], { pd: ['PD'], m5s: [], lega: [] }, new Set(['03:milano']));
    expect(r.municipalities[0].matchable).toBe(false);
    expect(r.municipalities[0].votes).toEqual([25, 0, 0, 0]);
    expect(generationRegionId('TRENTINO-ALTO ADIGE/SUDTIROL')).toBe('04');
    expect(generationRegionId('EMILIA ROMAGNA')).toBe('08');
  });
});

const archives = ['camera-20080413', 'camera-20130224', 'camera-20180304', 'camera-20220925', 'europee-20090607', 'europee-20140525', 'europee-20190526', 'europee-20240609'];
describe.skipIf(archives.some(id => !existsSync(resolve(root, `data/sources/poll-history/${id}.zip`))))('official historical generation panel (requires all pinned election archives)', () => {
  it('conserves pinned election totals and audits every municipality lost between events', async () => {
    const h = await loadGenerationHistory(root), calibration = await readCalibrationResults(root);
    expect(h.events.map(e => e.id)).toEqual(['camera-2008', 'european-2009', 'camera-2013', 'european-2014', 'camera-2018', 'european-2019', 'camera-2022', 'european-2024']);
    expect(h.events.find(e => e.id === 'camera-2018')!.national).toMatchObject({ electors: 46505350, voters: 33923321, validVotes: 32841705 });
    expect(h.events.find(e => e.id === 'camera-2018')!.audit.nationalReference.panelVoteDifferences).toEqual([0, 0, 142, -142]);
    for (const e of h.events) {
      expect(e.national.votes.reduce((a, b) => a + b, 0)).toBe(e.national.validVotes);
      expect(e.municipalities.every(m => m.lists.reduce((n, l) => n + l.votes, 0) === m.validVotes)).toBe(true);
      const official = calibration.results.find(r => r.sourceId === e.sourceId)!;
      expect(e.national.validVotes + e.audit.excluded.filter(x => x.reason.startsWith('Fuori-sede')).reduce((n, x) => n + x.validVotes, 0)).toBe(official.total);
    }
    expect(h.events.find(e => e.id === 'camera-2008')!.national.votes[1]).toBe(0);
    for (let i = 1; i < h.events.length; i++) {
      const { pairs, audit } = matchGenerationEvents(h.events[i - 1], h.events[i]);
      expect(pairs.length + audit.baselineOnly.length).toBe(audit.baselineMunicipalities);
      expect(pairs.length + audit.targetOnly.length).toBe(audit.targetMunicipalities);
      expect(audit.targetValidVoteFraction).toBeGreaterThan(.99);
      expect(pairs.every(p => p.target.matchable && p.baseline.matchable)).toBe(true);
    }
  }, 30000);
});
