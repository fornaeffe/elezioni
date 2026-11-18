import type { Chamber, ChamberInput, College, Territory } from './input';
import type { DrawTrace, ReformWarning } from './result';

export interface PopulationNotice {
  readonly id: string;
  readonly severity: 'info' | 'warning';
  readonly method: 'census-crosswalk' | 'population-estimate' | 'source-reconciliation';
  readonly municipalityCodes: readonly string[];
  readonly collegeIds: readonly string[];
  /** User-facing Italian explanation, also carried into the legal input's seat source. */
  readonly message: string;
}
export interface CatalogTerritory {
  readonly chamber: Chamber;
  readonly id: string;
  readonly name: string;
  readonly regionId: string;
  readonly kind: Territory['kind'];
  readonly population: number;
}
export interface CatalogCollege {
  readonly chamber: Chamber;
  readonly id: string;
  readonly name: string;
  readonly territoryId: string;
  readonly kind: College['kind'];
  readonly population: number;
  readonly provinceId: string | null;
}
export interface GeographySummary {
  readonly schemaVersion: 1;
  readonly version: string;
  readonly censusDate: string;
  readonly censusReference: string;
  readonly sourceManifest: string;
  readonly sourceHashes: readonly { readonly id: string; readonly sha256: string }[];
  readonly regions: readonly { readonly id: string; readonly name: string; readonly autonomous: boolean }[];
  readonly population: number;
  readonly municipalityCount: number;
  readonly territories: readonly CatalogTerritory[];
  readonly colleges: readonly CatalogCollege[];
  readonly notices: readonly PopulationNotice[];
  readonly populationReconciliations: readonly {
    readonly municipalityCode: string;
    readonly chamber: Chamber;
    readonly method: string;
    readonly estimated: boolean;
    readonly parts: readonly { readonly id: string; readonly observed: number | null; readonly historicalWeight: number }[];
    readonly populations: readonly { readonly id: string; readonly population: number }[];
    readonly draws: readonly DrawTrace[];
    readonly warnings: readonly ReformWarning[];
  }[];
}
export interface TerritoryCatalog extends GeographySummary {
  readonly municipalities: readonly {
    readonly code: string; readonly name: string; readonly regionId: string; readonly provinceId: string; readonly population: number;
  }[];
  readonly memberships: readonly {
    readonly chamber: Chamber; readonly collegeId: string; readonly municipalityCode: string;
    readonly unitId: string; readonly population: number;
  }[];
}

const count = (n: number): boolean => Number.isSafeInteger(n) && n >= 0;
const sum = (values: readonly number[]): number => values.reduce((a, b) => a + b, 0);
function unique<T>(rows: readonly T[], key: (row: T) => string, label: string, errors: string[]): void {
  const keys = rows.map(key);
  if (keys.some(x => !x) || new Set(keys).size !== keys.length) errors.push(`Duplicate/missing ${label}`);
}

/** Real-data validation is separate from R26-05's deliberately constructed legal fixtures. */
export function validateGeographySummary(g: GeographySummary): string[] {
  const errors: string[] = [];
  if (g.schemaVersion !== 1 || !g.version || !g.sourceManifest || !g.censusReference || !/^\d{4}-\d{2}-\d{2}$/.test(g.censusDate)) errors.push('Invalid geography provenance');
  unique(g.regions, x => x.id, 'region', errors);
  unique(g.sourceHashes, x => x.id, 'source', errors);
  if (!g.sourceHashes.length || g.sourceHashes.some(x => !/^[a-f0-9]{64}$/.test(x.sha256))) errors.push('Invalid source fingerprints');
  if (g.regions.length !== 20 || g.regions.some(x => !/^(0[1-9]|1[0-9]|20)$/.test(x.id))) errors.push('Incomplete domestic regional catalog');
  if (g.regions.some(r => !r.name || r.autonomous !== ['02', '04', '06', '19', '20'].includes(r.id))) errors.push('Invalid regional autonomy metadata');
  if (!count(g.population) || !g.population || !count(g.municipalityCount) || !g.municipalityCount) errors.push('Invalid census coverage');
  unique(g.territories, t => t.id, 'territory', errors);
  unique(g.colleges, c => c.id, 'college', errors);
  unique(g.notices, n => n.id, 'population notice', errors);
  const territories = new Map(g.territories.map(t => [t.id, t]));
  for (const t of g.territories) {
    if (!['camera', 'senato'].includes(t.chamber) || !g.regions.some(r => r.id === t.regionId) || !count(t.population) || !t.population) errors.push(`Invalid territory ${t.id}`);
    if (t.kind !== (t.regionId === '02' ? 'vda' : t.regionId === '04' ? 'taa' : 'ordinary')) errors.push(`Wrong special territory ${t.id}`);
    const colleges = g.colleges.filter(c => c.territoryId === t.id);
    if (!colleges.length) errors.push(`No colleges in ${t.id}`);
    // Camera TAA's proportional college overlaps its four special contests.
    const physical = colleges.filter(c => c.kind !== 'taa-proportional');
    if (sum(physical.map(c => c.population)) !== t.population) errors.push(`College population mismatch in ${t.id}`);
    if (t.kind === 'ordinary' && colleges.some(c => c.kind !== 'ordinary')) errors.push(`Special college in ${t.id}`);
    if (t.kind === 'vda' && (colleges.length !== 1 || colleges[0].kind !== 'special-uninominal')) errors.push('Invalid VdA structure');
    if (t.kind === 'taa') {
      const uni = colleges.filter(c => c.kind === 'special-uninominal');
      const proportional = colleges.filter(c => c.kind === 'taa-proportional');
      if (uni.length !== (t.chamber === 'camera' ? 4 : 6) || proportional.length !== (t.chamber === 'camera' ? 1 : 0) ||
          proportional.some(c => c.population !== t.population)) errors.push(`Invalid TAA structure: ${t.chamber}`);
      // K57: «Nessuna Regione o Provincia autonoma ... inferiore a tre»; S1(4): «sei collegi uninominali».
      if (t.chamber === 'senato' && (uni.filter(c => c.provinceId === '021').length !== 3 || uni.filter(c => c.provinceId === '022').length !== 3)) errors.push('Senate autonomous provinces require three contests each');
    }
  }
  for (const c of g.colleges) {
    const t = territories.get(c.territoryId);
    if (!t || t.chamber !== c.chamber || !count(c.population) || !c.population || !['ordinary', 'taa-proportional', 'special-uninominal'].includes(c.kind)) errors.push(`Invalid college ${c.id}`);
  }
  for (const chamber of ['camera', 'senato'] as const) {
    const ts = g.territories.filter(t => t.chamber === chamber);
    if (ts.length !== (chamber === 'camera' ? 28 : 20) || sum(ts.map(t => t.population)) !== g.population ||
        g.regions.some(r => !ts.some(t => t.regionId === r.id))) errors.push(`Incomplete ${chamber} coverage`);
    // C1(3)/S1(2) retain the 2020 A2/B2 colleges, plus the specified special contests.
    if (g.colleges.filter(c => c.chamber === chamber).length !== (chamber === 'camera' ? 54 : 33)) errors.push(`Incomplete ${chamber} college catalog`);
    if (chamber === 'senato') unique(ts, t => t.regionId, 'Senate region', errors);
    for (const r of g.regions) {
      const expected = chamber === 'senato' ? 1 : r.id === '03' ? 4 : ['01', '05', '12', '15', '19'].includes(r.id) ? 2 : 1;
      if (ts.filter(t => t.regionId === r.id).length !== expected) errors.push(`Wrong constituency coverage ${chamber}:${r.id}`);
    }
  }
  for (const n of g.notices) {
    if (!n.message || !['info', 'warning'].includes(n.severity) || !['census-crosswalk', 'population-estimate', 'source-reconciliation'].includes(n.method) ||
        n.collegeIds.some(id => !g.colleges.some(c => c.id === id))) errors.push(`Invalid population notice ${n.id}`);
  }
  return errors;
}

export function validateTerritoryCatalog(g: TerritoryCatalog): string[] {
  const errors = validateGeographySummary(g);
  unique(g.municipalities, m => m.code, 'municipality', errors);
  unique(g.memberships, m => JSON.stringify([m.chamber, m.collegeId, m.unitId]), 'membership', errors);
  const ms = new Map(g.municipalities.map(m => [m.code, m]));
  const cs = new Map(g.colleges.map(c => [c.id, c]));
  const collegePopulation = new Map<string, number>();
  const municipalPopulation = new Map<string, number>();
  const physicalUnits = new Set<string>();
  if (g.municipalities.length !== g.municipalityCount || sum(g.municipalities.map(m => m.population)) !== g.population) errors.push('Municipal census coverage mismatch');
  for (const m of g.municipalities) if (!/^\d{6}$/.test(m.code) || !/^\d{3}$/.test(m.provinceId) || !count(m.population) || !g.regions.some(r => r.id === m.regionId)) errors.push(`Invalid municipality ${m.code}`);
  for (const m of g.memberships) {
    const c = cs.get(m.collegeId), municipality = ms.get(m.municipalityCode);
    if (!c || !municipality || c.chamber !== m.chamber || !m.unitId || !count(m.population)) { errors.push(`Invalid membership ${m.unitId}`); continue; }
    const t = g.territories.find(t => t.id === c.territoryId);
    if (!t) { errors.push(`Unknown membership territory ${c.territoryId}`); continue; }
    if (t.regionId !== municipality.regionId) errors.push(`Wrong regional membership ${m.unitId}`);
    if (c.kind === 'special-uninominal' && t.kind === 'taa' && c.provinceId !== municipality.provinceId) errors.push(`Autonomous-province mismatch ${m.unitId}`);
    collegePopulation.set(c.id, (collegePopulation.get(c.id) ?? 0) + m.population);
    if (c.kind !== 'taa-proportional') {
      const key = JSON.stringify([m.chamber, m.unitId]);
      if (physicalUnits.has(key)) errors.push(`Overlapping electoral unit ${m.unitId}`);
      physicalUnits.add(key);
      const mk = `${m.chamber}:${m.municipalityCode}`;
      municipalPopulation.set(mk, (municipalPopulation.get(mk) ?? 0) + m.population);
    }
  }
  for (const c of g.colleges) if (collegePopulation.get(c.id) !== c.population) errors.push(`Membership population mismatch ${c.id}`);
  for (const m of g.municipalities) for (const chamber of ['camera', 'senato']) {
    if (municipalPopulation.get(`${chamber}:${m.code}`) !== m.population) errors.push(`Missing/duplicated census population ${chamber}:${m.code}`);
  }
  return errors;
}

export type ChamberGeography = Pick<ChamberInput, 'chamber' | 'regions' | 'territories' | 'colleges' | 'seatTable'>;
