import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { archiveMember, annexText, bolzanoAnnex4, dbf, decode, delimited, electoralSpreadsheet, normalizeName } from './formats';
import { readSource, sha256, type SourceManifest } from './sources';
import { validateTerritoryCatalog, type CatalogCollege, type CatalogTerritory, type PopulationNotice, type TerritoryCatalog } from '../../src/lib/politics/reform-2026/geography';
import { reconcilePopulation } from '../../src/lib/politics/reform-2026/population';
import { createSeededDrawContext } from '../../src/lib/politics/reform-2026/ties';
import type { Chamber } from '../../src/lib/politics/reform-2026/input';

const padded = (n: string, width: number): string => String(Number(n)).padStart(width, '0');
const number = (v: string): number => {
  if (!/^\d+$/.test(v) || !Number.isSafeInteger(Number(v))) throw new Error(`Invalid source integer ${v}`);
  return Number(v);
};
const sum = <T>(rows: readonly T[], f: (r: T) => number): number => rows.reduce((s, r) => s + f(r), 0);
const order = (a: string, b: string): number => a < b ? -1 : a > b ? 1 : 0;
function index<T>(rows: readonly T[], key: (r: T) => string): Map<string, T> {
  const result = new Map(rows.map(r => [key(r), r]));
  if (result.size !== rows.length) throw new Error('Ambiguous source join');
  return result;
}

function annex5Members(html: string): { college: string; name: string }[] {
  const result: { college: string; name: string }[] = [];
  for (const block of annexText(html).split(/^\s*={5,}\s*$/m)) {
    const college = block.match(/U0[456]/)?.[0];
    if (!college) continue;
    let fragments: string[] = [];
    const flush = () => {
      if (fragments.length) result.push({ college, name: fragments.join(' ').split('/')[0].replace(/-\s+/g, '-').replace(/\s+/g, ' ').trim() });
      fragments = [];
    };
    for (const line of block.split('\n')) {
      if (/^\s*\+/.test(line)) flush();
      const cells = line.trim().split('|');
      if (cells.length === 5 && cells[3].trim()) fragments.push(cells[3].trim());
    }
    flush();
  }
  return result;
}

export async function prepareTerritoryCatalog(root: string, manifest: SourceManifest, directory = resolve(root, 'data/sources')): Promise<{ catalog: TerritoryCatalog; audit: unknown }> {
  const usedSources = new Set<string>();
  const source = async (id: string) => {
    const entry = manifest.sources.find(s => s.id === id);
    if (!entry) throw new Error(`Missing manifest source ${id}`);
    const bytes = await readSource(directory, entry);
    usedSources.add(id);
    return bytes;
  };
  const optionalSubmunicipal = async () => {
    try { return await source('submunicipal-geography-2021'); }
    catch (error) { if ((error as NodeJS.ErrnoException).code === 'ENOENT') return undefined; throw error; }
  };
  const [geoBytes, populationBytes, municipalityBytes, subBytes, annex4, annex5] = await Promise.all([
    source('electoral-geography-2022'), source('legal-population-2021'), source('municipalities-2021'),
    optionalSubmunicipal(), source('bolzano-annex-4'), source('bolzano-annex-5')
  ]);
  const rows = dbf(archiveMember(geoBytes, 'UT_Collegi2020/UT_Collegi2020.dbf'));
  const populations = index(delimited(populationBytes).filter(r => /^\d{6}$/.test(r.REF_AREA) && r.TIME_PERIOD === '2021' && r.INDICATOR === 'RESPOP_AV'), r => r.REF_AREA);
  const rawMunicipalities = JSON.parse(municipalityBytes.toString('utf8')).resultset as Record<string, string>[];
  const municipalities = rawMunicipalities.map(m => {
    const p = populations.get(m.PRO_COM_T);
    if (!p) throw new Error(`Missing legal census ${m.PRO_COM_T}`);
    return { code: m.PRO_COM_T, name: m.COMUNE, regionId: m.COD_REG, provinceId: m.COD_PROV_STORICO, population: number(p.OBS_VALUE) };
  }).sort((a, b) => order(a.code, b.code));
  const municipalIndex = index(municipalities, m => m.code);
  if (populations.size !== municipalities.length || sum(municipalities, m => m.population) !== 59030133) throw new Error('Pinned 2021 legal census coverage changed');
  const regions = [...new Set(municipalities.map(m => m.regionId))].sort().map(id => ({ id,
    name: rawMunicipalities.find(m => m.COD_REG === id)!.DEN_REG, autonomous: ['02', '04', '06', '19', '20'].includes(id) }));
  const italianNames = new Map<string, string>();
  for (const m of rawMunicipalities.filter(m => m.COD_PROV_STORICO === '021')) italianNames.set(normalizeName(m.COMUNE_IT), m.PRO_COM_T);
  // Official rename after the census date; this is an explicit alias, not fuzzy matching.
  italianNames.set(normalizeName('Montagna sulla strada del vino'), '021053');
  const resolveBolzano = (name: string): string => {
    const code = italianNames.get(normalizeName(name));
    if (!code) throw new Error(`Unknown statutory Bolzano municipality ${name}`);
    return code;
  };
  const revised = bolzanoAnnex4(annex4.toString('utf8')).map(r => ({ code: resolveBolzano(r.name), college: r.college }));
  const secondAnnex = annex5Members(annex5.toString('utf8')).map(r => ({ code: resolveBolzano(r.name), college: r.college }));
  const canonical = (rs: typeof revised) => rs.map(r => `${r.code}:${r.college}`).sort().join('|');
  if (revised.length !== 116 || new Set(revised.map(r => r.code)).size !== 116 || canonical(revised) !== canonical(secondAnnex)) throw new Error(`Bolzano annex mismatch: ${revised.length}/${secondAnnex.length}; missing ${canonical(revised.filter(r => !secondAnnex.some(s => s.code === r.code && s.college === r.college)))}; extra ${canonical(secondAnnex.filter(r => !revised.some(s => s.code === r.code && s.college === r.college)))}`);
  const bolzano = index(revised, r => r.code);
  for (const r of rows) {
    const code = padded(r.PRO_COM, 6);
    if (!municipalIndex.has(code)) throw new Error(`Geography has unknown census municipality ${code}`);
    if (padded(r.COD_PRO, 3) === '021') {
      const college = bolzano.get(code)?.college;
      if (!college) throw new Error(`Bolzano coverage missing ${code}`);
      r.SU20_COD = `4000${college.slice(-1)}`;
      r.SU20_DEN = `Trentino-Alto Adige/Südtirol - ${college}`;
    }
  }
  const spreadsheets = await Promise.all((['camera-a1', 'camera-a2', 'senato-b1', 'senato-b2'] as const).map(async id => {
    const members = await electoralSpreadsheet(await source(id), id.endsWith('1') ? 'uni' : 'pluri');
    return { id, members };
  }));
  const expectedColleges = [147, 49, 74, 26];
  spreadsheets.forEach((s, i) => {
    if (new Set(s.members.map(m => m.collegeName)).size !== expectedColleges[i]) throw new Error(`Incomplete spreadsheet extraction ${s.id}`);
  });
  // The definitive A2/B2 relations must match the machine-readable 2022 geography.
  for (const [i, uniField, pluriField] of [[1, 'CU20_DEN', 'CP20_DEN'], [3, 'SU20_DEN', 'SP20_DEN']] as const) {
    const mappings = new Map(rows.filter(r => r[pluriField]).map(r => [normalizeName(r[uniField]), normalizeName(r[pluriField])]));
    for (const m of spreadsheets[i].members) {
      if (mappings.get(normalizeName(m.member)) !== normalizeName(m.collegeName)) throw new Error(`Spreadsheet/DBF plurinominal mismatch ${m.member}`);
    }
  }
  const areaRows = subBytes ? delimited(archiveMember(subBytes, 'Tab_ASC/ASC_Liv_3_2021.csv'), '\t').filter(r => padded(r.PRO_COM, 6) === '058091') : [];
  const areaPopulations = index(areaRows, r => padded(r.COM_ASC3, 9));
  const notices: PopulationNotice[] = [{ id: 'source-2022-update', severity: 'info', method: 'source-reconciliation',
    municipalityCodes: ['081025', '099030', '099031'], collegeIds: [],
    message: 'Geografia ufficiale aggiornata al 2022: inclusi Misiliscemi e il passaggio di Montecopiolo e Sassofeltrio all’Emilia-Romagna. I codici catastali e amministrativi non sostituiscono i confini elettorali.' }];
  const populationReconciliations: TerritoryCatalog['populationReconciliations'][number][] = [];
  const territories = new Map<string, CatalogTerritory>();
  const colleges = new Map<string, CatalogCollege>();
  const memberships: TerritoryCatalog['memberships'][number][] = [];
  const groups = new Map<string, typeof rows>();
  for (const r of rows) {
    const code = padded(r.PRO_COM, 6);
    const group = groups.get(code) ?? [];
    group.push(r); groups.set(code, group);
  }
  if (groups.size !== municipalities.length) throw new Error('Not all census municipalities have electoral geography');
  for (const chamber of ['camera', 'senato'] as const) for (const m of municipalities) {
    const units = groups.get(m.code)!;
    const special = m.regionId === '02' || m.regionId === '04';
    const territoryId = `${chamber}:territory:${chamber === 'camera' ? padded(units[0].CIRC_COD, 2) : m.regionId}`;
    const kind = m.regionId === '02' ? 'vda' : m.regionId === '04' ? 'taa' : 'ordinary';
    const old = territories.get(territoryId);
    territories.set(territoryId, { chamber, id: territoryId, name: chamber === 'camera' ? units[0].CIRC_DEN : regions.find(r => r.id === m.regionId)!.name,
      regionId: m.regionId, kind, population: (old?.population ?? 0) + m.population });
    const prefix = chamber === 'camera' ? 'C' : 'S';
    const field = `${prefix}${special ? 'U' : 'P'}20_COD`, label = `${prefix}${special ? 'U' : 'P'}20_DEN`;
    const grouped = new Map<string, typeof units>();
    for (const u of units) {
      if (!u[field] || u[field] === '0') throw new Error(`Missing active college ${field}:${m.code}`);
      const existing = grouped.get(u[field]) ?? []; existing.push(u); grouped.set(u[field], existing);
    }
    const collegeId = (code: string, proportional = false) => `${chamber}:${special && !proportional ? 'U' : 'P'}:${padded(code, 6)}`;
    const allocations = grouped.size === 1 ? [{ id: [...grouped.keys()][0], population: m.population }] : (() => {
      const parts = [...grouped].map(([id, us]) => {
        const areaData = m.code === '058091' ? us.map(u => areaPopulations.get(padded(u.ASC_COD, 9))) : [];
        return { id, observed: areaData.length && areaData.every(Boolean) ? sum(areaData, a => number(a!.POP21.replaceAll('.', ''))) : null,
          historicalWeight: sum(us, u => number(u.POP_2011)) };
      });
      const reconciled = reconcilePopulation(m.population, parts, createSeededDrawContext('population-2021-v1', `${chamber}/${m.code}`), `${chamber}/${m.code}`);
      populationReconciliations.push({ municipalityCode: m.code, chamber, parts, ...reconciled });
      notices.push({ id: `${chamber}-${m.code}-population`, severity: 'warning', method: reconciled.estimated ? 'population-estimate' : 'census-crosswalk',
        municipalityCodes: [m.code], collegeIds: [...grouped.keys()].map(id => collegeId(id)), message: reconciled.estimated
          ? `Popolazione dei collegi di ${m.name} stimata (${reconciled.method}), conservando il totale comunale del censimento 2021.`
          : `Popolazione dei collegi di ${m.name} ricostruita dai dati censuari 2021 per zona urbanistica. Corrispondenza per codice delle zone con la geografia elettorale 2022; continuità dei confini assunta, non verificata geometricamente. Il totale comunale è conservato.` });
      return reconciled.populations;
    })();
    const add = (code: string, name: string, population: number, proportional = false) => {
      const id = collegeId(code, proportional), previous = colleges.get(id);
      if (previous && previous.territoryId !== territoryId) throw new Error(`Ambiguous college ${id}`);
      const collegeKind = proportional ? 'taa-proportional' : special ? 'special-uninominal' : 'ordinary';
      colleges.set(id, { id, chamber, name, territoryId, kind: collegeKind, population: (previous?.population ?? 0) + population,
        provinceId: special && m.regionId === '04' && !proportional ? m.provinceId : null });
      memberships.push({ chamber, collegeId: id, municipalityCode: m.code, unitId: `${m.code}:${special ? 'U' : 'P'}:${code}`, population });
    };
    for (const a of allocations) add(a.id, grouped.get(a.id)![0][label], a.population);
    if (chamber === 'camera' && m.regionId === '04') add(units[0].CP20_COD, units[0].CP20_DEN, m.population, true);
  }
  const catalog: TerritoryCatalog = { schemaVersion: 1, version: 'r26-geography-2022-bolzano-2026-census-2021-v1', censusDate: manifest.censusDate,
    censusReference: manifest.legalReference, sourceManifest: manifest.id, sourceHashes: manifest.sources.filter(s => usedSources.has(s.id)).map(({ id, sha256 }) => ({ id, sha256 })),
    regions, population: sum(municipalities, m => m.population), municipalityCount: municipalities.length,
    territories: [...territories.values()].sort((a, b) => order(a.id, b.id)), colleges: [...colleges.values()].sort((a, b) => order(a.id, b.id)),
    municipalities, memberships, notices, populationReconciliations };
  // Available fine-geography inputs and reconstruction assumptions are part of identity.
  const versionedCatalog = { ...catalog, version: `${catalog.version}-${sha256(JSON.stringify(catalog)).slice(0, 12)}` };
  const errors = validateTerritoryCatalog(versionedCatalog);
  if (errors.length) throw new Error(errors.join('\n'));
  return { catalog: versionedCatalog, audit: { spreadsheetExtraction: spreadsheets.map((s, i) => ({ id: s.id, memberships: s.members.length, colleges: expectedColleges[i] })),
    primarySource: 'electoral-geography-2022:UT_Collegi2020/UT_Collegi2020.dbf', attributeEncoding: decode(archiveMember(geoBytes, 'UT_Collegi2020/UT_Collegi2020.dbf')).encoding,
    bolzanoMunicipalities: revised.length, romeUrbanZones: areaRows.length, romeCensusPopulation: sum(areaRows, a => number(a.POP21.replaceAll('.', ''))),
    municipalityCount: municipalities.length, population: catalog.population } };
}

export async function readManifest(root: string): Promise<SourceManifest> {
  const manifest = JSON.parse(await readFile(resolve(root, 'data/source-manifest.json'), 'utf8')) as SourceManifest;
  if (manifest.schemaVersion !== 1 || !manifest.id || !manifest.sources.length || new Set(manifest.sources.map(s => s.id)).size !== manifest.sources.length) throw new Error('Invalid source manifest');
  return manifest;
}
