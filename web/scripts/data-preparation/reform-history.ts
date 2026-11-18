import type { Chamber } from '../../src/lib/politics/reform-2026/input';
import type { TerritoryCatalog } from '../../src/lib/politics/reform-2026/geography';
import type { HistoricalMunicipality } from '../../src/lib/politics/reform-2026/generation-types';
import { apportion, generationRng, sum } from '../../src/lib/politics/reform-2026/generation-math';
import { archiveMember, delimited, normalizeName } from './formats';
import { sha256 } from './sources';

type Row = Record<string, string>;
export const historyListId = (name: string, special = false) => `${special ? 'group' : 'list'}-${sha256(normalizeName(name)).slice(0, 16)}`;
export function sourceInteger(value: string): number {
  if (!/^\d+$/.test(value) || !Number.isSafeInteger(Number(value))) throw new Error(`Invalid historical integer ${value}`);
  return Number(value);
}
const key = (...parts: string[]) => JSON.stringify(parts);
export function geographyResolver(catalog: TerritoryCatalog) {
  const regions = new Map(catalog.regions.map(r => [normalizeName(r.name.split('/')[0]), r.id]));
  const names = new Map<string, typeof catalog.municipalities[number][]>();
  for (const m of catalog.municipalities) {
    const k = key(m.regionId, normalizeName(m.name.split('/')[0]));
    names.set(k, [...(names.get(k) ?? []), m]);
  }
  // Source 2022 uses the former short name; legal census identity is unchanged.
  const aliases: Record<string, string> = {
    montagnasullastradadelvino: 'montagna', casorzomonferrato: 'casorzo', ionadi: 'jonadi',
    doberdodellago: 'doberdodellagodoberdob',
    sanflorianodelcollio: 'sanflorianodelcolliosteverjan', savognadisonzo: 'savognadisonzosovodnjeobsoci',
    duinoaurisina: 'duinoaurisinadevinnabrezina', monrupino: 'monrupinorepentabor', sgonico: 'sgonicozgonik',
    sangiovannidifassa: 'sangiovannidifassasenjan'
  };
  return (region: string, name: string, chamber?: Chamber, oldPluri?: string) => {
    const regionName = normalizeName(region.replace(/\s+[1-4]$/, '').split('/')[0]);
    const regionId = regionName.startsWith('valledaosta') ? '02' : regions.get(regionName);
    if (!regionId) throw new Error(`Unknown source region ${region}`);
    let n = normalizeName(name.split('/')[0]); n = aliases[n] ?? n;
    let found = names.get(key(regionId, n)) ?? [];
    if (found.length > 1 && chamber && oldPluri) {
      const colleges = catalog.colleges.filter(c => c.chamber === chamber && normalizeName(c.name) === normalizeName(oldPluri));
      found = found.filter(m => catalog.memberships.some(u => u.municipalityCode === m.code && colleges.some(c => c.id === u.collegeId)));
    }
    if (found.length !== 1) throw new Error(`Ambiguous/missing municipality ${region}:${name} (${found.map(m => m.code)})`);
    return found[0];
  };
}

/** Old-law C77(1)(c), S16(1)(c), as amended by L165/2017:
 * «divide il totale dei voti validi ... nel collegio uninominale per il numero
 * dei voti espressi a favore dei soli candidati ...», then integer parts and
 * largest remainders. Source list fields are DIRECT votes. Reconstruct only
 * after aggregating the complete old uninominal college, never municipality.
 * Municipal placement of the additions is an explicitly synthetic reconstruction
 * proportional to the candidate-only ballots there, conserving each list/uni
 * total. These data never replace the legacy parity snapshot. */
export function reconstructHistoricalAttribution(rows: readonly Row[], chamber: Chamber) {
  const groups = new Map<string, Row[]>();
  for (const r of rows) {
    const k = key(r['CIRC-REG'], r.COLLUNINOM, r.COGNOME, r.NOME, r.DATANASCITA);
    groups.set(k, [...(groups.get(k) ?? []), r]);
  }
  const output: { row: Row; listId: string; votes: number }[] = [];
  const traces: { group: string; direct: number; candidateOnly: number; attributed: number; additions: { listId: string; votes: number }[] }[] = [];
  for (const [group, input] of [...groups].sort(([a], [b]) => a < b ? -1 : 1)) {
    const local = new Map<string, Row[]>();
    for (const r of input) local.set(r.COMUNE, [...(local.get(r.COMUNE) ?? []), r]);
    const listNames = [...new Set(input.map(r => r.DESCRLISTA))].sort();
    const municipalities = [...local].sort(([a], [b]) => a < b ? -1 : 1);
    const direct = listNames.map(l => sum(input.filter(r => r.DESCRLISTA === l).map(r => sourceInteger(r.VOTILISTA))));
    const only = municipalities.map(([name, rs]) => {
      if (new Set(rs.map(r => r.DESCRLISTA)).size !== rs.length || new Set(rs.map(r => r.VOTICANDIDATO)).size !== 1 ||
          rs.length !== listNames.length) throw new Error(`Incomplete/duplicate historical candidature ${group}:${name}`);
      const difference = sourceInteger(rs[0].VOTICANDIDATO) - sum(rs.map(r => sourceInteger(r.VOTILISTA)));
      if (difference < 0) throw new Error(`Historical list votes exceed candidate total ${group}:${name}`);
      return difference;
    });
    const extra = apportion(sum(only), direct, generationRng('r26-history-attribution-v1', 0, chamber, group));
    const remainingOnly = [...only];
    for (const [i, name] of listNames.entries()) {
      const additions = apportion(extra[i], remainingOnly, generationRng('r26-history-municipal-reconstruction-v1', 0, chamber, group, name));
      municipalities.forEach(([, rs], j) => {
        remainingOnly[j] -= additions[j];
        const row = rs.find(r => r.DESCRLISTA === name)!;
        output.push({ row, listId: historyListId(name), votes: sourceInteger(row.VOTILISTA) + additions[j] });
      });
    }
    if (remainingOnly.some(n => n !== 0)) throw new Error('Historical municipal candidate-only margins were not conserved');
    traces.push({ group, direct: sum(direct), candidateOnly: sum(only), attributed: sum(direct) + sum(extra),
      additions: listNames.map((name, i) => ({ listId: historyListId(name), votes: extra[i] })) });
  }
  return { output, traces };
}

export function preparePairedHistory(cameraBytes: Uint8Array, senatoBytes: Uint8Array, catalog: TerritoryCatalog) {
  const resolve = geographyResolver(catalog);
  const municipalities = new Map<string, HistoricalMunicipality>();
  const lists = new Map<string, { id: string; name: string; kind: 'list' | 'special-ballot-group' }>();
  const historicalCoalitions: { chamber: Chamber; listIds: string[] }[] = [];
  const audits = [];
  for (const chamber of ['camera', 'senato'] as const) {
    const bytes = chamber === 'camera' ? cameraBytes : senatoBytes;
    const rows = delimited(archiveMember(bytes, chamber === 'camera' ? 'camera2022_Italia_LivComune.csv' : 'Senato_Italia_LivComune.csv'), ';');
    const reconstruction = reconstructHistoricalAttribution(rows, chamber);
    const components: Set<string>[] = [];
    for (const t of reconstruction.traces) {
      const ids = t.additions.map(l => l.listId);
      if (ids.length < 2) continue;
      const joined = components.filter(c => ids.some(id => c.has(id)));
      const merged = new Set([...ids, ...joined.flatMap(c => [...c])]);
      for (const old of joined) components.splice(components.indexOf(old), 1);
      components.push(merged);
    }
    historicalCoalitions.push(...components.map(c => ({ chamber, listIds: [...c].sort() })));
    const ballots = new Map<string, { code: string; electors: number }>();
    const mutable = new Map<string, { electors: number; votes: Map<string, number> }>();
    function record(code: string, listId: string, votes: number) {
      const entry = mutable.get(code) ?? { electors: 0, votes: new Map() };
      entry.votes.set(listId, (entry.votes.get(listId) ?? 0) + votes); mutable.set(code, entry);
    }
    for (const { row, listId, votes } of reconstruction.output) {
      const m = resolve(row['CIRC-REG'], row.COMUNE, chamber, row.COLLPLURI);
      record(m.code, listId, votes);
      lists.set(listId, { id: listId, name: row.DESCRLISTA, kind: 'list' });
      const k = key(m.code, row.COLLUNINOM), electors = sourceInteger(row.ELETTORITOT);
      if (ballots.has(k) && ballots.get(k)!.electors !== electors) throw new Error('Inconsistent repeated electorate');
      ballots.set(k, { code: m.code, electors });
    }
    const special = delimited(archiveMember(bytes, chamber === 'camera' ? 'camera2022_VAosta_LivComune.csv' : 'Senato_VAosta&Trentino_LivComune.csv'), ';');
    for (const r of special) {
      const m = resolve(r.REGIONE, r.COMUNE), id = historyListId(r.CONTRASSEGNO, true);
      lists.set(id, { id, name: r.CONTRASSEGNO, kind: 'special-ballot-group' });
      record(m.code, id, sourceInteger(r.TOTVOTI));
    }
    const turnout = delimited(archiveMember(bytes, chamber === 'camera' ? 'camera2022_VAosta_LivComune_Scrutini.csv' : 'Senato_VAosta&Trentino_livComune_Scrutini.csv'), ';');
    for (const r of turnout) {
      const m = resolve(r.REGIONE ?? "VALLE D'AOSTA", r.COMUNE ?? r.DENOMINAZIONE);
      const k = key(m.code, 'special');
      if (ballots.has(k)) throw new Error('Duplicate special electorate');
      ballots.set(k, { code: m.code, electors: sourceInteger(r.NUMELETTORI ?? r.ELETTORITOTALI) });
    }
    for (const b of ballots.values()) {
      const entry = mutable.get(b.code);
      if (!entry) throw new Error(`Electorate without results ${b.code}`);
      entry.electors += b.electors;
    }
    for (const m of catalog.municipalities) {
      const entry = mutable.get(m.code);
      if (!entry || !entry.electors || sum([...entry.votes.values()]) > entry.electors) throw new Error(`Missing/invalid ${chamber} historical coverage ${m.code}:${m.name}`);
      const result = municipalities.get(m.code) ?? { code: m.code, regionId: m.regionId, provinceId: m.provinceId, name: m.name,
        camera: { electors: 0, votes: [] }, senato: { electors: 0, votes: [] } };
      result[chamber] = { electors: entry.electors, votes: [...entry.votes].sort(([a], [b]) => a < b ? -1 : 1).map(([listId, votes]) => ({ listId, votes })) };
      municipalities.set(m.code, result);
    }
    audits.push({ chamber, rawRows: rows.length, municipalities: mutable.size, electorate: sum([...mutable.values()].map(m => m.electors)),
      directOrdinary: sum(reconstruction.traces.map(t => t.direct)), candidateOnly: sum(reconstruction.traces.map(t => t.candidateOnly)),
      attributedOrdinary: sum(reconstruction.traces.map(t => t.attributed)), specialVotes: sum(special.map(r => sourceInteger(r.TOTVOTI))),
      attribution: reconstruction.traces });
  }
  return { municipalities: [...municipalities.values()].sort((a, b) => a.code < b.code ? -1 : 1), historicalLists: [...lists.values()].sort((a, b) => a.id < b.id ? -1 : 1), historicalCoalitions, audit: audits };
}
