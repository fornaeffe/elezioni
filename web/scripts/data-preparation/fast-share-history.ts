/** Production historical acquisition/normalization. No research exporter dependency. */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import type { TerritoryCatalog } from '../../src/lib/politics/reform-2026/geography';
import type { OriginalRow, SourceEvent, SourceChoice, SourceUnit } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import { archiveMember, delimited, normalizeName } from './formats';
import { geographyResolver, reconstructHistoricalAttribution, sourceInteger } from './reform-history';
import { readManifest, prepareTerritoryCatalog } from './normalize';
import { readSource, sha256, type Source } from './sources';
import { loadSourceMemberships, sourceMembership } from './fast-share-memberships';
import { rowFlags } from '../../src/lib/politics/reform-2026/vote-share/source-codec';

type Row = Record<string, string>;
export async function shareSourceManifests(root: string) {
  const policy = JSON.parse(await readFile(resolve(root, 'data/fast-share-source-manifest.json'), 'utf8'));
  if (policy.schemaVersion !== 1 || policy.id !== 'fast-share-production-sources-v1' || policy.resultAvailability.method !== 'retrospective-thirty-day-lag-v1' || policy.resultAvailability.lagDaysAfterFinalPollingDay !== 30) throw new Error('Unsupported fast-share source policy');
  const paths: string[] = policy.inputManifests;
  if (paths.join('|') !== 'data/source-manifest.json|data/sources/polls/history/elections-manifest.json|data/reform-model-source-manifest.json') throw new Error('Unsupported source manifest composition');
  const bodies = await Promise.all(paths.map(p => readFile(resolve(root, p), 'utf8')));
  const [geo, history, reform] = bodies.map(b => JSON.parse(b)) as { sources: Source[] }[];
  const sources = [...geo.sources, ...history.sources, ...reform.sources.filter(s => ['senato-2018', 'senato-2022', 'camera-2022'].includes(s.id))];
  return { geo, history, reform, sources, policy, versions: ['fast-share-source-manifest:' + sha256(JSON.stringify(policy)), ...bodies.map((b, i) => paths[i] + ':' + sha256(JSON.stringify(JSON.parse(b))))] };
}
const aliases: Record<string, string> = {
  montagnasullastradadelvino: 'montagna', casorzomonferrato: 'casorzo', ionadi: 'jonadi',
  doberdodellago: 'doberdodellagodoberdob', sanflorianodelcollio: 'sanflorianodelcolliosteverjan',
  savognadisonzo: 'savognadisonzosovodnjeobsoci', duinoaurisina: 'duinoaurisinadevinnabrezina',
  monrupino: 'monrupinorepentabor', sgonico: 'sgonicozgonik', sangiovannidifassa: 'sangiovannidifassasenjan'
};
export function historicalRegion(name: string) {
  const n = normalizeName(name.replace(/\s+[1-4]$/, '').split('/')[0]);
  const names = ['piemonte','valledaosta','lombardia','trentinoaltoadige','veneto','friuliveneziagiulia','liguria','emiliaromagna','toscana','umbria','marche','lazio','abruzzo','molise','campania','puglia','basilicata','calabria','sicilia','sardegna'];
  const i = n === 'aosta' ? 1 : names.indexOf(n);
  if (i < 0) throw new Error('Unknown source region: ' + name);
  return String(i + 1).padStart(2, '0');
}
const integer = (s: string) => sourceInteger(s.replace(/,00$/, ''));
export function createUnitResolver(catalog: TerritoryCatalog) {
  const exact = geographyResolver(catalog);
  return (region: string, name: string, province: string | null, chamber?: 'camera' | 'senato', college?: string): SourceUnit => {
    const regionId = historicalRegion(region), raw = normalizeName(name.split('/')[0]), normalized = aliases[raw] ?? raw;
    let target: TerritoryCatalog['municipalities'][number] | undefined;
    try { target = exact(region, name, chamber, college); } catch (e) {
      if (!(e instanceof Error) || !e.message.startsWith('Ambiguous/missing municipality')) throw e;
    }
    return { id: target ? 'target:' + target.code : 'historical:' + [regionId, normalized, province ? normalizeName(province) : college ? normalizeName(college) : 'unresolved'].join(':'),
      name, region: regionId, province, targetCode: target?.code ?? null, match: target ? 'same-place' : 'unmatched' };
  };
}

export async function prepareShareHistory(root: string, directory: string) {
  const manifests = await shareSourceManifests(root), membership = await loadSourceMemberships(root);
  const { catalog } = await prepareTerritoryCatalog(root, await readManifest(root), directory);
  const locate = createUnitResolver(catalog), units = new Map<string, SourceUnit>(), choices = new Map<string, SourceChoice>();
  const events: SourceEvent[] = [], audits: unknown[] = [];
  const specs = [
    ...manifests.history.sources.map(s => ({ source: s as Source & { member: string; additionalMembers?: string[] }, context: s.id.startsWith('camera') ? 'camera' as const : 'european' as const })),
    ...manifests.reform.sources.filter(s => ['senato-2018', 'senato-2022'].includes(s.id)).map(s => ({ source: s as Source & { member?: string; additionalMembers?: string[] }, context: 'senato' as const }))
  ];
  for (const { source: s, context } of specs) {
    const year = Number(s.id.match(/20\d\d/)![0]);
    const id = context + '-' + year, date = s.id.startsWith('senato') ? (year === 2018 ? '2018-03-04' : '2022-09-25') : s.id.slice(-8).replace(/(\d{4})(\d{2})(\d{2})/, '$1-$2-$3');
    const bytes = await readSource(directory, s), members: { name: string; sha256: string; bytes: number }[] = [];
    const member = (name: string) => { const b = archiveMember(bytes, name); members.push({ name, sha256: sha256(b), bytes: b.length }); return delimited(b, ';'); };
    const memberName = context === 'senato' ? year === 2018 ? 'Senato2018_livComune.txt' : 'Senato_Italia_LivComune.csv' : s.id === 'camera-20180304' ? 'Camera2018_livComune.txt' : s.member!;
    const raw = member(memberName), byUnit = new Map<string, { row: OriginalRow; ballots: Map<string, { electors: number; voters: number | null }> }>();
    let candidateOnly = 0; const seenRows = new Set<string>();
    function record(r: { region: string; name: string; province?: string | null; ballot: string; college?: string; electors: number; voters: number | null; label: string; count: number; direct?: number; special?: boolean; unlocated?: boolean }) {
      const unit: SourceUnit = r.unlocated ? { id: 'unlocated:' + id + ':' + sha256(JSON.stringify([r.region, r.province, r.name])).slice(0, 16),
        name: r.name, region: 'unlocated', province: r.province ?? null, targetCode: null, match: 'unlocated' } :
        locate(r.region, r.name, r.province ?? null, context === 'european' ? undefined : context, r.college);
      if (!units.has(unit.id)) units.set(unit.id, unit);
      const cid = id + ':' + sha256(JSON.stringify([Boolean(r.special), r.label])).slice(0, 16);
      const rowKey = JSON.stringify([unit.id, r.ballot, cid]);
      if (seenRows.has(rowKey)) throw new Error('Duplicate original municipality/ballot/choice');
      seenRows.add(rowKey);
      if (!choices.has(cid)) choices.set(cid, { id: cid, eventId: id, label: r.label, ...sourceMembership(membership.policy, id, r.label, r.special) });
      const entry: { row: OriginalRow; ballots: Map<string, { electors: number; voters: number | null }> } =
        byUnit.get(unit.id) ?? { row: { unit: unit.id, electors: 0, voters: 0, choices: [] }, ballots: new Map() };
      const old = entry.ballots.get(r.ballot);
      if (old && (old.electors !== r.electors || old.voters !== r.voters)) throw new Error('Inconsistent original ballot denominator: ' + unit.id);
      entry.ballots.set(r.ballot, { electors: r.electors, voters: r.voters });
      const choice = entry.row.choices.find(c => c.id === cid);
      if (choice) { choice.count += r.count; choice.direct += r.direct ?? r.count; }
      else entry.row.choices.push({ id: cid, count: r.count, direct: r.direct ?? r.count });
      byUnit.set(unit.id, entry);
    }
    if (['camera', 'senato'].includes(context) && year >= 2018) {
      const ordinary = raw.filter(r => historicalRegion(r.REGIONE ?? r.CIRCOSCRIZIONE ?? r['CIRC-REG']) !== '02');
      const adapted = ordinary.map(r => year === 2018 ? { ...r, 'CIRC-REG': r.REGIONE ?? r.CIRCOSCRIZIONE, COLLUNINOM: r.COLLEGIOUNINOMINALE, COLLPLURI: r.COLLEGIOPLURINOMINALE,
        DATANASCITA: r.DATA_NASCITA, DESCRLISTA: r.LISTA, VOTILISTA: r.VOTI_LISTA, VOTICANDIDATO: r.VOTI_CANDIDATO } : r);
      const absent = adapted.filter(r => r.COGNOME === 'CANDIDATO NON PRESENTE');
      const attributed = reconstructHistoricalAttribution(adapted.filter(r => r.COGNOME !== 'CANDIDATO NON PRESENTE'), context as 'camera' | 'senato');
      candidateOnly = attributed.traces.reduce((n, t) => n + t.candidateOnly, 0);
      for (const { row: r, votes } of [...attributed.output, ...absent.map(row => ({ row, votes: integer(row.VOTILISTA) }))]) record({ region: r['CIRC-REG'], name: r.COMUNE, ballot: r.COLLUNINOM, college: r.COLLPLURI,
        electors: integer(r.ELETTORITOT ?? r.ELETTORI), voters: integer(r.VOTANTITOT ?? r.VOTANTI), label: r.DESCRLISTA, count: votes, direct: integer(r.VOTILISTA) });
      if (year === 2018) {
        const special = new Map<string, Row[]>();
        for (const r of raw.filter(r => historicalRegion(r.REGIONE ?? r.CIRCOSCRIZIONE) === '02')) {
          const key = JSON.stringify([r.COMUNE, r.COGNOME, r.NOME, r.DATA_NASCITA]);
          special.set(key, [...(special.get(key) ?? []), r]);
        }
        for (const rs of special.values()) { const r = rs[0];
          if (new Set(rs.map(r => r.VOTI_CANDIDATO)).size !== 1) throw new Error('Conflicting special candidate total');
          record({ region: "VALLE D'AOSTA", name: r.COMUNE, ballot: 'special', electors: integer(r.ELETTORI), voters: integer(r.VOTANTI),
            label: [...new Set(rs.map(r => r.LISTA))].sort().join(' + '), count: integer(r.VOTI_CANDIDATO), special: true });
        }
      } else {
        // Camera 2022 and Senate 2022 special ballots have separate turnout members.
        const specialName = context === 'camera' ? 'camera2022_VAosta_LivComune.csv' : 'Senato_VAosta&Trentino_LivComune.csv';
        const turnoutName = context === 'camera' ? 'camera2022_VAosta_LivComune_Scrutini.csv' : 'Senato_VAosta&Trentino_livComune_Scrutini.csv';
        const special = member(specialName), turnout = member(turnoutName);
        const denominators = new Map(turnout.map(r => [JSON.stringify([historicalRegion(r.REGIONE ?? "VALLE D'AOSTA"), r.COMUNE ?? r.DENOMINAZIONE]), r]));
        for (const r of special) { const t = denominators.get(JSON.stringify([historicalRegion(r.REGIONE), r.COMUNE])); if (!t) throw new Error('Missing special turnout');
          record({ region: r.REGIONE, name: r.COMUNE, ballot: 'special', electors: integer(t.NUMELETTORI ?? t.ELETTORITOTALI), voters: integer(t.VOTANTITOT ?? t.VOTANTITOTALI), label: r.CONTRASSEGNO, count: integer(r.TOTVOTI), special: true });
        }
      }
    } else {
      for (const r of raw) record({ region: r.REGIONE ?? r.DESCREGIONE ?? r.CIRCOSCRIZIONE, province: r.PROVINCIA ?? r.DESCPROVINCIA ?? null, name: r.COMUNE ?? r.DESCCOMUNE,
        ballot: r.CIRCOSCRIZIONE ?? r.DESCCIRCEUROPEA, electors: integer(r.ELETTORI), voters: integer(r.VOTANTI), label: r.LISTA ?? r.DESCLISTA, count: integer(r.VOTI_LISTA ?? r.NUMVOTI) });
      if (context === 'camera') for (const r of member('camera_vaosta-' + date.replaceAll('-', '') + '.txt')) record({ region: "VALLE D'AOSTA", name: r.COMUNE, ballot: 'special', electors: integer(r.ELETTORI_TOTALI), voters: integer(r.VOTANTI_TOTALI), label: r.LISTA, count: integer(r.VOTI_LISTA), special: true });
    }
    for (const name of s.additionalMembers ?? []) for (const r of member(name)) record({ region: r.DESCREGIONE, province: r.DESCPROVINCIA, name: r.DESCCOMUNE,
      ballot: r.DESCCIRCEUROPEA, electors: integer(r.ELETTORI), voters: integer(r.VOTANTI), label: r.DESCLISTA, count: integer(r.NUMVOTI), unlocated: true });
    const rows = [...byUnit.values()].map(({ row, ballots }) => ({ ...row, electors: [...ballots.values()].reduce((n, b) => n + b.electors, 0),
      voters: [...ballots.values()].some(b => b.voters === null) ? null : [...ballots.values()].reduce((n, b) => n + b.voters!, 0), choices: row.choices.sort((a, b) => a.id.localeCompare(b.id)) })).sort((a, b) => a.unit.localeCompare(b.unit));
    // Retrospective assumption, NOT an asserted official publication timestamp.
    // Multi-day contests add one final polling day before this conservative lag.
    const lag = ['camera-2008', 'camera-2013'].includes(id) ? 31 : 30;
    const availableDate = new Date(Date.parse(date) + lag * 86400000).toISOString().slice(0, 10);
    events.push({ id, date, availableDate, context, minimumAge: id === 'senato-2018' ? 25 : 18,
      role: context !== 'senato' ? 'reference' : year === 2018 ? 'age-audit' : 'paired-adaptation', sourceId: s.id, sourceUrl: s.url, sourceSha256: s.sha256,
      availabilityBasis: 'Retrospective assumption: 30 days after final polling day; not a verified publication time', rows });
    audits.push({ id, members, rawRows: raw.length, municipalities: rows.length, unmatched: rows.filter(r => units.get(r.unit)!.match === 'unmatched').length,
      invalidDenominators: rows.filter(r => rowFlags(r, units.get(r.unit)!.match) & 1).map(r => r.unit), candidateOnly });
  }
  return { events: events.sort((a, b) => a.date.localeCompare(b.date) || a.id.localeCompare(b.id)), units: [...units.values()].sort((a, b) => a.id.localeCompare(b.id)),
    choices: [...choices.values()].sort((a, b) => a.id.localeCompare(b.id)), catalog, audits, membership,
    sourceVersions: [...manifests.versions, ...specs.map(({ source: s }) => s.id + ':' + s.sha256), ...membership.versions].sort() };
}
