import { describe, expect, it } from 'vitest';
import { getBundledReformSeatTables } from './bundled-data';
import { buildProvisionalSeatTables, chamberGeography, seatTableMessages, validateSeatTableBundle, type SeatTableBundle } from './endowments';
import { validateGeographySummary, validateTerritoryCatalog, type TerritoryCatalog } from './geography';
import { createSeededDrawContext } from './ties';
import { jointFixture } from './contracts.fixture';
import { validateReformScrutinyInput } from './validation';

type Mutable<T> = T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
const bundle = () => getBundledReformSeatTables() as Mutable<SeatTableBundle>;
const regenerate = (g = bundle().geography) => {
  const b = bundle();
  return buildProvisionalSeatTables(g, b.version, b.seed!, createSeededDrawContext(b.seed!, 'N03-seat-tables-v1'));
};

describe('versioned real geography and I08 endowments', () => {
  it('ships complete provisional tables independently of ignored archives', () => {
    const b = bundle();
    expect(validateSeatTableBundle(b)).toEqual([]);
    expect(b.geography.municipalityCount).toBe(7904);
    expect(b.geography.population).toBe(59030133);
    expect(b.method).toBe('I08-v1');
    expect(b.geography.censusDate).toBe('2021-12-31');
    expect(regenerate()).toEqual(b);
  });
  it('conserves domestic, special, ordinary and bonus ledgers independently', () => {
    for (const c of bundle().chambers) {
      const ordinary = c.colleges.filter(x => x.kind === 'ordinary');
      const special = c.colleges.filter(x => x.kind !== 'ordinary');
      const sum = (rows: typeof c.colleges, field: 'seatsWithBonus' | 'seatsWithoutBonus') => rows.reduce((s, x) => s + x[field], 0);
      expect([sum(c.colleges, 'seatsWithoutBonus'), sum(special, 'seatsWithoutBonus'), sum(ordinary, 'seatsWithoutBonus'), sum(ordinary, 'seatsWithBonus'), c.territories.reduce((s, x) => s + x.bonusSeats, 0)])
        .toEqual(c.chamber === 'camera' ? [392, 8, 384, 314, 70] : [196, 7, 189, 154, 35]);
    }
  });
  it('checks independent population-quota examples and the constrained Senate floor', () => {
    const b = bundle();
    // 2021 Piemonte: 4,256,350; Senate fixed seats = 9, Basilicata freezes at 3.
    // 59,030,133 - (123,360 + 1,073,574 + 292,150 + 541,168) = 56,999,881.
    const trace = b.traces.find(t => t.id === 'senato-regions-1')!;
    expect(trace.seats).toBe(184);
    expect(trace.figures.reduce((s, f) => s + f.population, 0)).toBe(56999881);
    const piemonte = trace.allocation.assignments.find(a => a.id === 'senato:territory:01')!;
    expect(piemonte.integerSeats).toBe(13);
    expect(piemonte.seats).toBe(14);
    const senate = b.chambers.find(c => c.chamber === 'senato')!;
    const regional = (region: string) => senate.colleges.filter(c => senate.territories.find(t => t.id === c.territoryId)?.regionId === region).reduce((s, c) => s + c.seatsWithoutBonus, 0);
    expect(['02', '04', '14', '17'].map(regional)).toEqual([1, 6, 2, 3]);
    // Exact national Camera quota: 59,030,133 / 392, not an integer-truncated divisor.
    expect(b.traces.find(t => t.id === 'camera-constituencies')!.allocation.quota)
      .toEqual({ numerator: '59030133', denominator: '392' });
  });
  it('keeps TAA provincial special contests and the Camera proportional pool distinct', () => {
    const b = bundle();
    const taa = b.geography.colleges.filter(c => c.territoryId === 'senato:territory:04');
    expect(taa.filter(c => c.provinceId === '021')).toHaveLength(3);
    expect(taa.filter(c => c.provinceId === '022')).toHaveLength(3);
    const camera = b.chambers.find(c => c.chamber === 'camera')!;
    expect(camera.colleges.filter(c => c.kind === 'taa-proportional').map(c => c.seatsWithoutBonus)).toEqual([3]);
  });
  it('preserves reconstruction disclosures in the legal adapter and owns its returned data', () => {
    const b = bundle(), first = chamberGeography(b, 'camera');
    expect(first.seatTable.populationNotes?.[0]).toContain('continuità dei confini assunta');
    expect(seatTableMessages(b).some(m => m.severity === 'warning' && m.message.includes('Roma'))).toBe(true);
    expect(seatTableMessages(b)[0].severity).toBe('info');
    (first.colleges as Mutable<typeof first.colleges>)[0].seatsWithoutBonus = 999;
    expect(chamberGeography(b, 'camera').colleges[0].seatsWithoutBonus).not.toBe(999);
  });
  it('accepts the additive population-note field without changing old constructed inputs', () => {
    const f = jointFixture();
    f.renewal.kind === 'joint' && (f.renewal.camera.seatTable.populationNotes = ['Popolazione stimata: caso costruito.']);
    const checked = validateReformScrutinyInput(f);
    expect(checked.valid).toBe(true);
    if (checked.valid && checked.input.renewal.kind === 'joint') expect(checked.input.renewal.camera.seatTable.populationNotes).toEqual(['Popolazione stimata: caso costruito.']);
  });
  it.each(['missing-college', 'foreign-region', 'wrong-minimum', 'negative-bonus', 'capacity-growth', 'mismatched-version', 'missing-notice'])(
    'rejects inconsistent tables: %s', kind => {
      const b = bundle(), c = b.chambers[0];
      if (kind === 'missing-college') c.colleges.pop();
      if (kind === 'foreign-region') c.regions[0].id = 'foreign';
      if (kind === 'wrong-minimum') {
        const s = b.chambers[1];
        const college = s.colleges.find(x => s.territories.find(t => t.id === x.territoryId)?.regionId === '17')!;
        college.seatsWithoutBonus = 2;
      }
      if (kind === 'negative-bonus') c.territories[0].bonusSeats = -1;
      if (kind === 'capacity-growth') c.colleges[0].seatsWithBonus = c.colleges[0].seatsWithoutBonus + 1;
      if (kind === 'mismatched-version') c.seatTable.version = 'other';
      if (kind === 'missing-notice') delete c.seatTable.populationNotes;
      expect(validateSeatTableBundle(b).length).toBeGreaterThan(0);
    });
  it('rejects a conflicting special endowment rather than forcing Camera 314', () => {
    const g = bundle().geography;
    const t = g.territories.find(t => t.chamber === 'camera' && t.kind === 'taa')!;
    const delta = 1000000;
    t.population += delta;
    for (const c of g.colleges.filter(c => c.territoryId === t.id)) c.population += c.kind === 'taa-proportional' ? delta : delta / 4;
    const other = g.territories.find(t => t.chamber === 'camera' && t.regionId === '03')!;
    other.population -= delta;
    g.colleges.find(c => c.territoryId === other.id)!.population -= delta;
    expect(() => regenerate(g)).toThrow('Statutory special-seat conflict');
  });
  it('reports incompatible alternative capacities instead of silently changing I08', () => {
    const g = bundle().geography;
    // Independent Hare example: 29 seats -> [14,1,14]; 24 seats -> [11,2,11].
    // Reducing the house can increase the small college's entitlement (Alabama paradox).
    const populations = [2084260, 251193, 2089913];
    g.colleges.filter(c => c.territoryId === 'camera:territory:11').forEach((c, i) => { c.population = populations[i]; });
    expect(() => regenerate(g)).toThrow('Incompatible alternative college capacities');
  });
  it('freezes and traces seeded I03 draws in population ties', () => {
    const b = bundle(), g = b.geography;
    const territory = g.territories.find(t => t.kind === 'ordinary' && t.population % 2 === 0 &&
      g.colleges.filter(c => c.territoryId === t.id).length === 2 &&
      b.chambers.find(c => c.chamber === t.chamber)!.colleges.filter(c => c.territoryId === t.id).reduce((s, c) => s + c.seatsWithoutBonus, 0) % 2 === 1)!;
    expect(territory).toBeDefined();
    g.colleges.filter(c => c.territoryId === territory.id).forEach(c => { c.population = territory.population / 2; });
    const tables = regenerate(g);
    expect(tables.draws.length).toBeGreaterThan(0);
    expect(tables.warnings.every(w => w.interpretationId === 'I03' && w.simulationId === 0)).toBe(true);
    expect(regenerate(g)).toEqual(tables);
  });
  it('validates official replacement tables without assuming the I08 distribution is statutory', () => {
    const b = bundle();
    b.version = 'constructed-official-replacement-test'; b.method = 'official-decree'; b.seed = null;
    b.traces = []; b.draws = []; b.warnings = [];
    for (const c of b.chambers) c.seatTable = { kind: 'official-decree', version: b.version, reference: 'Constructed decree fixture, not a real decree', populationNotes: c.seatTable.populationNotes };
    expect(validateSeatTableBundle(b)).toEqual([]);
    expect(bundle().method).toBe('I08-v1');
  });
  it('rejects incomplete regional, special and source geography', () => {
    const g = bundle().geography;
    g.regions.pop();
    g.sourceHashes[0].sha256 = 'invalid';
    const taa = g.colleges.find(c => c.chamber === 'senato' && c.provinceId === '021')!;
    taa.provinceId = '022';
    expect(validateGeographySummary(g)).toEqual(expect.arrayContaining(['Incomplete domestic regional catalog', 'Invalid source fingerprints', 'Senate autonomous provinces require three contests each']));
  });
  it('detects duplicate and unknown catalog membership separately from seat tables', () => {
    const g = bundle().geography;
    const minimal: TerritoryCatalog = { ...g, municipalities: [], memberships: [] };
    expect(validateTerritoryCatalog(minimal)).toContain('Municipal census coverage mismatch');
  });
});
