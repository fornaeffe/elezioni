import { archiveMember, delimited, electoralSpreadsheet, normalizeName } from './formats';
import { readSource, sha256, type SourceManifest } from './sources';
import type { DefinitiveGeography } from './definitive-geography';
import { reconcilePopulation } from '../../src/lib/politics/reform-2026/population';
import { createSeededDrawContext } from '../../src/lib/politics/reform-2026/ties';
import { hareNiemeyer, hareNiemeyerWithMinimum } from '../../src/lib/core/allocation';
import { createSeededRng } from '../../src/lib/core/rng';
import type { PoliticsBaseDataRow, PoliticsStaticDataRamoSnapshot } from '../../src/lib/politics/types';

export async function prepareWorkspaceGeography(geo: DefinitiveGeography, manifest: SourceManifest, directory: string, electorate: Map<string, number>) {
  const source = async (id: string) => {
    const entry = manifest.sources.find(s => s.id === id);
    if (!entry) throw new Error(`Missing source ${id}`);
    return readSource(directory, entry);
  };
  // All physical uninominal memberships come from the definitive DBF, before Bolzano overlays.
  const supersededSpreadsheetMemberships: object[] = [];
  for (const chamber of ['camera', 'senato'] as const) {
    const prefix = chamber === 'camera' ? 'C' : 'S';
    for (const kind of ['uni', 'pluri'] as const) {
      const table = await electoralSpreadsheet(await source(`${chamber === 'camera' ? 'camera-a' : 'senato-b'}${kind === 'uni' ? '1' : '2'}`), kind);
      const memberships = new Set(geo.rows.map(u => `${normalizeName(u[`${prefix}${kind === 'uni' ? 'U' : 'P'}20_DEN`])}/${kind === 'uni' ? u.PRO_COM.padStart(6, '0') : normalizeName(u[`${prefix}U20_DEN`])}`));
      for (const r of table) {
        const found = memberships.has(`${normalizeName(r.collegeName)}/${kind === 'uni' ? r.municipalityCode : normalizeName(r.member)}`);
        if (!found) {
          // The 2020 spreadsheets predate the two Marche -> ER transfers carried
          // by the definitive 2022 archive; retain the source discrepancy in audit.
          const transferred = ({ '041033': '099030', '041060': '099031' } as Record<string, string>)[r.municipalityCode ?? ''];
          if (kind !== 'uni' || !transferred || !geo.rows.some(u => u.PRO_COM.padStart(6, '0') === transferred)) throw new Error(`Definitive ${chamber} ${kind} membership mismatch ${r.collegeName}/${r.member}`);
          supersededSpreadsheetMemberships.push({ chamber, oldCode: r.municipalityCode, revisedCode: transferred, sourceCollege: r.collegeName });
        }
      }
    }
  }
  const fine = new Map(delimited(archiveMember(await source('submunicipal-geography-2021'), 'Tab_ASC/ASC_Liv_3_2021.csv'), '\t')
    .filter(r => r.PRO_COM.padStart(6, '0') === '058091').map(r => [r.COM_ASC3.padStart(9, '0'), Number(r.POP21.replaceAll('.', ''))]));
  const municipalities = new Map(geo.municipalities.map(m => [m.code, m]));
  const groups = new Map<string, typeof geo.rows>();
  for (const r of geo.rows) { const code = r.PRO_COM.padStart(6, '0'), g = groups.get(code) ?? []; g.push(r); groups.set(code, g); }
  const population = new Map<string, number>(), electors = new Map<string, number>(), reconstructions: object[] = [];
  for (const [code, rows] of groups) {
    const m = municipalities.get(code), total = electorate.get(code);
    if (!m || !Number.isSafeInteger(total) || total! <= 0) throw new Error(`Missing census/electorate ${code}`);
    const sorted = [...rows].sort((a, b) => Number(a.OBJECTID) - Number(b.OBJECTID));
    const reconstruction = reconcilePopulation(m.population, sorted.map(r => ({ id: r.OBJECTID,
      observed: code === '058091' ? fine.get(r.ASC_COD.padStart(9, '0')) ?? null : sorted.length === 1 ? m.population : null,
      historicalWeight: Number(r.POP_2011) })), createSeededDrawContext('workspace-census-2021-v1', code), code);
    const parts = reconstruction.populations.map(p => p.population);
    const allocated = hareNiemeyer(parts, total!, createSeededRng(`workspace-electors-v1/${code}`));
    sorted.forEach((r, i) => { population.set(r.OBJECTID, parts[i]); electors.set(r.OBJECTID, allocated[i]); });
    if (sorted.length > 1) reconstructions.push({ code, ...reconstruction });
  }
  // Stable legacy namespaces remain readable by saved scenarios; geography contents are revised.
  const constituency = (r: Record<string, string>, chamber: 'camera' | 'senato') => chamber === 'camera'
    ? Number(r.COD_REG) * 100 + Number(r.CIRC_DEN.match(/ ([1-4])$/)?.[1] ?? 1) : Number(r.COD_REG);
  const buildChamber = (chamber: 'camera' | 'senato'): PoliticsStaticDataRamoSnapshot => {
    const prefix = chamber === 'camera' ? 'C' : 'S';
    const unis = new Map<number, { CIRC_COD: number; CIRC_DEN: string; PLURI_COD: number | null; PLURI_DEN: string | null; UNI_COD: number; UNI_DEN: string; population: number }>();
    for (const r of geo.rows) {
      const id = Number(r[`${prefix}U20_COD`]), previous = unis.get(id);
      const row = { CIRC_COD: constituency(r, chamber), CIRC_DEN: chamber === 'camera' ? r.CIRC_DEN : r.DEN_REG,
        PLURI_COD: Number(r[`${prefix}P20_COD`]) || null, PLURI_DEN: r[`${prefix}P20_DEN`] || null,
        UNI_COD: id, UNI_DEN: r[`${prefix}U20_DEN`], population: (previous?.population ?? 0) + population.get(r.OBJECTID)! };
      if (previous && (previous.CIRC_COD !== row.CIRC_COD || previous.PLURI_COD !== row.PLURI_COD)) throw new Error(`Non-nested college ${chamber}/${id}`);
      unis.set(id, row);
    }
    const uni = [...unis.values()].sort((a, b) => a.UNI_COD - b.UNI_COD);
    const territories = [...new Set(uni.map(u => u.CIRC_COD))].sort((a, b) => a - b);
    const territoryPopulation = territories.map(t => uni.filter(u => u.CIRC_COD === t).reduce((s, u) => s + u.population, 0));
    const seed = createSeededRng(`workspace-endowments-census-2021-v1/${chamber}`);
    // Costituzione, art. 56: "quattrocento", "otto" Estero; ripartizione
    // "sulla base dei quozienti interi e dei più alti resti" (392 domestic).
    // Art. 57: "duecento", "quattro" Estero; "Nessuna Regione o Provincia
    // autonoma" meno di tre; "il Molise ne ha due, la Valle d'Aosta uno".
    // This preserves the previous-law preparation formula; it is not a reform I08 table or an official decree.
    let assigned: number[];
    if (chamber === 'camera') assigned = hareNiemeyer(territoryPopulation, 392, seed);
    else {
      const variable = territories.filter(t => ![2, 14].includes(t));
      const allocation = hareNiemeyerWithMinimum(variable.map(t => territoryPopulation[territories.indexOf(t)]), 193, variable.map(t => t === 4 ? 6 : 3), seed);
      assigned = territories.map(t => t === 2 ? 1 : t === 14 ? 2 : allocation[variable.indexOf(t)]);
    }
    const pluri: PoliticsStaticDataRamoSnapshot['pluri'] = [];
    for (const t of territories) {
      const units = uni.filter(u => u.CIRC_COD === t), ids = [...new Set(units.map(u => u.PLURI_COD))].sort((a, b) => (a ?? 0) - (b ?? 0));
      const groups = ids.map(id => units.filter(u => u.PLURI_COD === id));
      const seats = hareNiemeyerWithMinimum(groups.map(g => g.reduce((n, u) => n + u.population, 0)), assigned[territories.indexOf(t)], groups.map(g => g.length), seed);
      groups.forEach((g, i) => {
        const proportional = seats[i] - g.length;
        // C18-bis(3): almeno due, non oltre quattro candidati. S9(4): da due
        // a quattro, ma "nei collegi ... un solo seggio ... un solo candidato".
        pluri.push({ CIRC_COD: t, CIRC_DEN: g[0].CIRC_DEN, PLURI_COD: g[0].PLURI_COD, PLURI_DEN: g[0].PLURI_DEN,
          SEGGI_PLURI: proportional, MAX_CANDIDATI: chamber === 'camera' ? proportional === 0 ? 0 : Math.max(2, Math.min(4, proportional)) : Math.min(4, proportional) });
      });
    }
    if (uni.length !== (chamber === 'camera' ? 147 : 74) || uni.length + pluri.reduce((n, p) => n + p.SEGGI_PLURI, 0) !== (chamber === 'camera' ? 392 : 196)) throw new Error(`Incomplete ${chamber} endowment`);
    return { uni: uni.map(({ population: _, ...u }) => u), pluri };
  };
  const camera = buildChamber('camera'), senato = buildChamber('senato');
  const base_dati: PoliticsBaseDataRow[] = [...geo.rows].sort((a, b) => a.PRO_COM.padStart(6, '0').localeCompare(b.PRO_COM.padStart(6, '0')) || Number(a.OBJECTID) - Number(b.OBJECTID))
    .map(r => ({ CODICE_COMUNE: r.PRO_COM.padStart(6, '0'), CODITA_20N: `definitive-2022:${r.OBJECTID}`, ELETTORI: electors.get(r.OBJECTID)!, CU20_COD: Number(r.CU20_COD), SU20_COD: Number(r.SU20_COD) }));
  const provenance = { method: 'workspace-definitive-2022-census-2021-v1', sourceHashes: manifest.sources.filter(s => ['camera-a1', 'camera-a2', 'senato-b1', 'senato-b2', 'electoral-geography-2022', 'legal-population-2021', 'municipalities-2021', 'submunicipal-geography-2021'].includes(s.id)).map(({ id, sha256 }) => ({ id, sha256 })),
    reconstructions, supersededSpreadsheetMemberships, notes: ['Collegi definitivi 2022 senza sovrapposizione Bolzano della riforma. Popolazione legale 2021; elettori e risultati subcomunali ricostruiti con pesi censuari, non osservati. Dotazioni calcolate provvisorie, non decreto ufficiale.'] };
  return { camera, senato, base_dati, audit: provenance, version: `workspace-geography-v1-${sha256(JSON.stringify({ provenance, camera, senato, base_dati })).slice(0, 12)}` };
}
