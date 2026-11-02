/** Previous-law/ER views retain direct list counts, not reform candidate attribution. */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { gunzipSync } from 'node:zlib';
import { archiveMember, delimited, normalizeName } from './formats';
import { readSource, type Source } from './sources';
import { historicalRegion } from './fast-share-history';
import type { DefinitiveGeography } from './definitive-geography';
import type { PoliticsHistoricalMunicipalListVoteRow } from '../../src/lib/politics/types';
import { hareNiemeyer } from '../../src/lib/core/allocation';
import { createSeededRng } from '../../src/lib/core/rng';

export const electionDates: Record<string, string> = {
  'camera 2018': '2018-03-04', 'camera 2022': '2022-09-25', 'europee 2019': '2019-05-26',
  'europee 2024': '2024-06-08', 'regionali 2020': '2020-01-26', 'regionali 2024': '2024-11-17',
};
export const electionTime = (election: string) => {
  if (!electionDates[election]) throw new Error(`Unknown historical election ${election}`);
  return `${electionDates[election]}T00:00:00.000Z`;
};
export async function workspaceSources(root: string): Promise<Source[]> {
  const files = ['source-manifest.json', 'reform-model-source-manifest.json', 'sources/polls/history/elections-manifest.json', 'workspace-source-manifest.json'];
  const manifests = await Promise.all(files.map(p => readFile(resolve(root, 'data', p), 'utf8').then(JSON.parse)));
  const entries = new Map<string, Source>();
  for (const manifest of manifests) for (const s of manifest.sources as Source[]) {
    if (['municipality-changes-2020-2021', 'submunicipal-population-2021'].includes(s.id)) continue; // unused originals, not production dependencies
    const old = entries.get(s.path);
    if (old && (old.sha256 !== s.sha256 || old.bytes !== s.bytes)) throw new Error(`Conflicting source ${s.path}`);
    entries.set(s.path, s);
  }
  return [...entries.values()];
}
type Change = { PRO_COM_T: string; COMUNE: string; PRO_COM_T_DT_FI: string; COMUNE_DT_FI: string };
const aliases: Record<string, string> = {
  sangiovannidifassa: 'sangiovannidifassasenjan', montagnasullastradadelvino: 'montagna',
  casorzomonferrato: 'casorzo', ionadi: 'jonadi',
  doberdodellago: 'doberdodellagodoberdob', sanflorianodelcollio: 'sanflorianodelcolliosteverjan',
  savognadisonzo: 'savognadisonzosovodnjeobsoci', duinoaurisina: 'duinoaurisinadevinnabrezina',
  monrupino: 'monrupinorepentabor', sgonico: 'sgonicozgonik',
  // Literal Ministry-2018 character substitutions, resolved against pinned ISTAT names.
  snjandifassa: 'sangiovannidifassasenjan', emarse: 'emarese', fnis: 'fenis', verrs: 'verres',
};
const nameKey = (name: string) => { const key = normalizeName(name.split('/')[0]); return aliases[key] ?? key; };

/** Official historical identities supply mergers. Reverse mergers are labelled census estimates. */
export function municipalResolver(geo: DefinitiveGeography, before: Change[], after: Change[]) {
  const byCode = new Map(geo.municipalities.map(m => [m.code, m]));
  const early = new Map<string, Set<string>>(), late = new Map<string, Set<string>>();
  const add = (map: Map<string, Set<string>>, name: string, code: string) => {
    if (!byCode.has(code)) return;
    const k = nameKey(name), codes = map.get(k) ?? new Set(); codes.add(code); map.set(k, codes);
  };
  for (const m of geo.municipalities) { add(early, m.name, m.code); add(late, m.name, m.code); }
  for (const r of before) add(early, r.COMUNE, r.PRO_COM_T_DT_FI);
  const successors = new Map<string, Set<string>>();
  for (const r of after) if (byCode.has(r.PRO_COM_T)) {
    const codes = successors.get(r.PRO_COM_T_DT_FI) ?? new Set(); codes.add(r.PRO_COM_T); successors.set(r.PRO_COM_T_DT_FI, codes);
  }
  // A surviving name can denote the whole post-census merger, not its old component.
  for (const r of after) for (const code of successors.get(r.PRO_COM_T_DT_FI) ?? []) add(late, r.COMUNE_DT_FI, code);
  return (region: string, name: string, year: number) => {
    const regionId = historicalRegion(region), lookup = year > 2021 ? late : early;
    const codes = [...(lookup.get(nameKey(name)) ?? [])];
    const regional = codes.filter(c => byCode.get(c)!.regionId === regionId);
    // Official crosswalks permit the two municipalities transferred from Marche to ER.
    const resolved = (regional.length ? regional : codes).map(c => byCode.get(c)!);
    if (!resolved.length || new Set(resolved.map(m => m.regionId)).size !== 1) throw new Error(`Unresolved historical municipality ${year}/${region}/${name}`);
    if (resolved.length > 1 && (year <= 2021 || !after.some(r => nameKey(r.COMUNE_DT_FI) === nameKey(name) &&
      resolved.every(m => successors.get(r.PRO_COM_T_DT_FI)?.has(m.code)) && successors.get(r.PRO_COM_T_DT_FI)?.size === resolved.length)))
      throw new Error(`Ambiguous historical municipality ${year}/${region}/${name}`);
    return resolved.sort((a, b) => a.code < b.code ? -1 : 1);
  };
}

const integer = (s: string) => {
  if (!/^\d+$/.test(s) || !Number.isSafeInteger(Number(s))) throw new Error(`Invalid historical count ${s}`);
  return Number(s);
};
export async function prepareWorkspaceHistory(root: string, directory: string, geo: DefinitiveGeography) {
  const sources = await workspaceSources(root), used = new Map<string, string>();
  const source = async (id: string) => {
    const s = sources.find(s => s.id === id);
    if (!s) throw new Error(`Missing source ${id}`);
    used.set(id, s.sha256); return readSource(directory, s);
  };
  const changes = async (id: string): Promise<Change[]> => JSON.parse(gunzipSync(await source(id)).toString('utf8')).resultset;
  const before = await changes('municipality-history-1991-2021');
  const specs = [
    ['camera 2018', 'camera-20180304', 'Camera2018_livComune.txt'],
    ['camera 2022', 'camera-2022', 'camera2022_Italia_LivComune.csv'],
    ['europee 2019', 'europee-20190526', 'europee-20190526.txt'],
    ['europee 2024', 'european-2024', 'EUROPEE_SCRUTINI&LISTE_ITALIA_LivComune.csv'],
    ['regionali 2020', 'regional-2020', 'regionali-20200126.txt'],
    ['regionali 2024', 'regional-2024', 'Regionali_EmiliaRomagna_2024_Scrutini.csv'],
  ];
  const output: PoliticsHistoricalMunicipalListVoteRow[] = [], audits: object[] = [];
  for (const [election, id, member] of specs) {
    const bytes = await source(id), rows = delimited(archiveMember(bytes, member), ';'), year = Number(election.slice(-4));
    const date = election === 'europee 2024' ? '20240609' : electionDates[election].replaceAll('-', '');
    const locate = municipalResolver(geo, before, year > 2021 ? await changes(`municipality-history-2021-${date}`) : []);
    const groups = new Map<string, { region: string; name: string; counts: Map<string, number>; ballots: Map<string, number> }>();
    for (const r of rows) {
      const region = r.REGIONE ?? r.DESCREGIONE ?? r['CIRC-REG'] ?? r.CIRCOSCRIZIONE;
      // Regional archives can contain multiple regions. ER is the installed regional workflow.
      if (election.startsWith('regionali') && historicalRegion(region) !== '08') continue;
      const name = r.COMUNE ?? r.DESCCOMUNE, k = `${region}:${name}`;
      const g = groups.get(k) ?? { region, name, counts: new Map(), ballots: new Map() };
      const list = r.LISTA ?? r.DESCLISTA ?? r.DESCRLISTA;
      // 2018 VdA stores candidate totals in place of the absent proportional list counts.
      const votes = integer(election === 'camera 2018' && historicalRegion(region) === '02' ? r.VOTI_CANDIDATO : r.VOTI_LISTA ?? r.VOTILISTA ?? r.NUMVOTI);
      g.counts.set(list, (g.counts.get(list) ?? 0) + votes);
      const ballot = r.SEZIONE ?? r.COLLEGIOUNINOMINALE ?? r.COLLUNINOM ?? 'municipal';
      const electors = integer(r.ELETTORI ?? r.ELETTORITOT ?? r.ELETTORI_TOTALI);
      if (g.ballots.has(ballot) && g.ballots.get(ballot) !== electors) throw new Error(`Inconsistent electorate ${election}/${k}/${ballot}`);
      g.ballots.set(ballot, electors); groups.set(k, g);
    }
    // Camera 2022 VdA is published in separate candidate/group and turnout files.
    if (election === 'camera 2022') {
      const results = delimited(archiveMember(bytes, 'camera2022_VAosta_LivComune.csv'), ';');
      const turnout = delimited(archiveMember(bytes, 'camera2022_VAosta_LivComune_Scrutini.csv'), ';');
      for (const r of results) {
        const k = `${r.REGIONE}:${r.COMUNE}`, g = groups.get(k) ?? { region: r.REGIONE, name: r.COMUNE, counts: new Map(), ballots: new Map() };
        g.counts.set(r.CONTRASSEGNO, integer(r.TOTVOTI)); groups.set(k, g);
      }
      for (const r of turnout) {
        const g = [...groups.values()].find(g => g.name === (r.COMUNE ?? r.DENOMINAZIONE) && historicalRegion(g.region) === '02');
        if (!g) throw new Error('VdA turnout without candidate results');
        g.ballots.set('special', integer(r.NUMELETTORI ?? r.ELETTORITOTALI));
      }
    }
    const estimates: object[] = [], denominatorAnomalies: object[] = [], present = new Set<string>();
    for (const g of groups.values()) {
      let targets = locate(g.region, g.name, year);
      // Misiliscemi (created 2021) is part of the earlier Trapani observation.
      // Only census-weighted reconstruction is possible without historical section votes.
      if (year < 2021 && targets.some(m => m.code === '081021')) targets = [...targets, geo.municipalities.find(m => m.code === '081025')!];
      const electors = [...g.ballots.values()].reduce((a, b) => a + b, 0), valid = [...g.counts.values()].reduce((a, b) => a + b, 0);
      if (!electors) throw new Error(`Missing electorate ${election}/${g.name}`);
      // Do not repair an inconsistent official observation by inventing electors or
      // deleting votes. Preserve the signed legacy residual and expose its source audit.
      if (valid > electors) denominatorAnomalies.push({ municipality: g.name, valid, electors, residual: electors - valid });
      g.counts.set('astensione', electors - valid);
      if (targets.length > 1) estimates.push({ source: g.name, targetCodes: targets.map(m => m.code), method: 'census-weighted-history-reconstruction', electors });
      for (const [list, total] of g.counts) {
        const counts = targets.length === 1 ? [total] : hareNiemeyer(targets.map(m => m.population), Math.abs(total), createSeededRng(`workspace-history-v1/${election}/${g.name}/${list}`)).map(n => total < 0 ? -n : n);
        targets.forEach((m, i) => { present.add(m.code); output.push({ DATA: electionTime(election), ELEZIONE: election, CODICE_COMUNE: m.code, LISTA: list, VOTI: counts[i] }); });
      }
    }
    const expected = geo.municipalities.filter(m => !election.startsWith('regionali') || m.regionId === '08');
    const missing = expected.filter(m => !present.has(m.code));
    // The 2020 ER election predates the transfer of these municipalities from Marche.
    const unavailable = election === 'regionali 2020' ? missing.filter(m => ['099030', '099031'].includes(m.code)) : [];
    if (missing.length !== unavailable.length) throw new Error(`Missing ${election} coverage: ${missing.map(m => `${m.code}:${m.name}`).join(', ')}`);
    audits.push({ election, member, sourceRows: rows.length, observedMunicipalities: present.size, estimatedMappings: estimates, denominatorAnomalies,
      unavailable: unavailable.map(m => ({ code: m.code, reason: 'Outside the ER electorate at the 2020 election; absent history, not zero.' })),
      electors: [...groups.values()].reduce((n, g) => n + [...g.ballots.values()].reduce((a, b) => a + b, 0), 0) });
  }
  // Forward municipal mergers can contribute multiple original rows to the same target.
  const aggregated = new Map<string, PoliticsHistoricalMunicipalListVoteRow>();
  for (const r of output) { const k = `${r.ELEZIONE}/${r.CODICE_COMUNE}/${r.LISTA}`, old = aggregated.get(k); if (old) old.VOTI += r.VOTI; else aggregated.set(k, { ...r }); }
  return { rows: [...aggregated.values()].sort((a, b) => a.DATA.localeCompare(b.DATA) || String(a.CODICE_COMUNE).localeCompare(String(b.CODICE_COMUNE)) || a.LISTA.localeCompare(b.LISTA, 'it')),
    audits, sourceHashes: [...used].map(([id, sha256]) => ({ id, sha256 })) };
}
