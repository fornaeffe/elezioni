import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { archiveMember, delimited, normalizeName } from './formats';
import { electionManifest, PANEL, readCalibrationResults } from './poll-calibration-sources';
import { reconstructHistoricalAttribution, sourceInteger } from './reform-history';
import { readSource, sha256 } from './sources';

export const GENERATION_PARTIES = PANEL;
export type GenerationParty = typeof GENERATION_PARTIES[number];
export interface GenerationMunicipality {
  /** A historical name identity, not an invented ISTAT code. */
  id: string; regionId: string; provinceId: string | null; name: string;
  electors: number; voters: number; validVotes: number;
  votes: number[]; lists: { name: string; votes: number }[];
  matchable: boolean;
}
export interface GenerationEvent {
  id: string; kind: 'camera' | 'european'; date: string; sourceId: string;
  sourceSha256: string;
  municipalities: GenerationMunicipality[];
  national: { electors: number; voters: number; validVotes: number; votes: number[] };
  mapping: { name: string; party: GenerationParty; method: 'exact-reviewed-source-label' | 'changing-complement' }[];
  audit: {
    members: { name: string; sha256: string; bytes: number }[];
    rawRows: number; candidateOnly: number;
    nationalReference: { source: string; validVotes: number; votes: number[]; panelVoteDifferences: number[]; note: string };
    excluded: { reason: string; rows: number; electors: number; voters: number; validVotes: number }[];
    unmatchable: { id: string; name: string; reason: string; electors: number; voters: number; validVotes: number }[];
  };
}
export interface GenerationHistory {
  schemaVersion: 1; version: string; parties: typeof GENERATION_PARTIES;
  events: GenerationEvent[]; sourceVersions: string[];
  geographyPolicy: string;
  transitions: GenerationMatchAudit[];
}
export interface GenerationMatchAudit {
  from: string; to: string; matched: number;
  baselineMunicipalities: number; targetMunicipalities: number;
  baselineValidVoteFraction: number; targetValidVoteFraction: number;
  baselineOnly: { id: string; name: string; validVotes: number }[];
  targetOnly: { id: string; name: string; validVotes: number }[];
}
type Row = Record<string, string>;
const sum = (ns: readonly number[]) => ns.reduce((n, v) => n + v, 0);
const compare = (a: string, b: string) => a < b ? -1 : a > b ? 1 : 0;
const dateOf = (id: string) => `${id.slice(-8, -4)}-${id.slice(-4, -2)}-${id.slice(-2)}`;

/** Stable region codes; source spelling changes and constituency suffixes have
 * no bearing on identity. No geography, turnout or votes are zero-filled. */
export function generationRegionId(name: string): string {
  const n = normalizeName(name.replace(/\s+[1-4]$/, '').split('/')[0]);
  const names = ['piemonte', 'valledaosta', 'lombardia', 'trentinoaltoadige', 'veneto',
    'friuliveneziagiulia', 'liguria', 'emiliaromagna', 'toscana', 'umbria', 'marche',
    'lazio', 'abruzzo', 'molise', 'campania', 'puglia', 'basilicata', 'calabria', 'sicilia', 'sardegna'];
  const index = n === 'aosta' ? 1 : names.indexOf(n);
  if (index < 0) throw new Error(`Unknown historical region ${name}`);
  return String(index + 1).padStart(2, '0');
}
// Only reviewed same-place spelling changes. Mergers/boundary transfers are
// deliberately unmatched; reconstructing them requires a dated ISTAT crosswalk.
const NAME_ALIASES: Record<string, string> = {
  montagnasullastradadelvino: 'montagna', casorzomonferrato: 'casorzo', ionadi: 'jonadi',
  doberdodellago: 'doberdodellagodoberdob',
  sanflorianodelcollio: 'sanflorianodelcolliosteverjan', savognadisonzo: 'savognadisonzosovodnjeobsoci',
  duinoaurisina: 'duinoaurisinadevinnabrezina', monrupino: 'monrupinorepentabor', sgonico: 'sgonicozgonik',
  sangiovannidifassa: 'sangiovannidifassasenjan'
};
export function generationMunicipalityName(name: string) {
  const n = normalizeName(name.split('/')[0]);
  return NAME_ALIASES[n] ?? n;
}
const rawIdentity = (region: string, name: string) => `${generationRegionId(region)}:${generationMunicipalityName(name)}`;
function decimalInteger(value: string) { return sourceInteger(value.replace(/,00$/, '')); }

export interface GenerationVoteRecord { region: string; province: string | null; name: string; ballot: string;
  electors: number; voters: number; list: string; votes: number; }

/** Pure normalization/conservation boundary, also exercised with synthetic
 * duplicate, invalid-turnout and ambiguous-geography fixtures. */
export function aggregateGenerationEvent(records: readonly GenerationVoteRecord[], panel: Record<'pd' | 'm5s' | 'lega', string[]>,
  ambiguousNames: ReadonlySet<string> = new Set()) {
  const declared = Object.values(panel).flat();
  if (new Set(declared).size !== declared.length) throw new Error('Overlapping generation list mapping');
  const municipalities = new Map<string, { result: GenerationMunicipality; ballots: Map<string, { electors: number; voters: number; lists: Set<string> }> }>();
  const mapping = new Map<string, GenerationParty>();
  for (const r of records) {
    if (!r.name || !r.list || ![r.electors, r.voters, r.votes].every(n => Number.isSafeInteger(n) && n >= 0)) throw new Error(`Invalid historical vote/turnout record ${JSON.stringify(r)}`);
    const base = rawIdentity(r.region, r.name), ambiguous = ambiguousNames.has(base);
    // Camera municipal extracts omit province. An ambiguous name cannot be
    // resolved by guessing which historical college belongs to which province.
    const id = ambiguous ? `${base}:${r.province ? normalizeName(r.province) : `unresolved-${normalizeName(r.ballot)}`}` : base;
    const value: { result: GenerationMunicipality; ballots: Map<string, { electors: number; voters: number; lists: Set<string> }> } = municipalities.get(id) ?? { result: { id, regionId: generationRegionId(r.region),
      provinceId: r.province ? normalizeName(r.province) : null, name: r.name,
      electors: 0, voters: 0, validVotes: 0, votes: PANEL.map(() => 0), lists: [], matchable: !ambiguous || r.province !== null }, ballots: new Map() };
    const ballot = value.ballots.get(r.ballot) ?? { electors: r.electors, voters: r.voters, lists: new Set() };
    if (ballot.electors !== r.electors || ballot.voters !== r.voters) throw new Error(`Inconsistent historical ballot denominator ${id}`);
    if (ballot.lists.has(r.list)) throw new Error(`Duplicate historical municipality/list ${id}:${r.list}`);
    ballot.lists.add(r.list); value.ballots.set(r.ballot, ballot);
    const party = (['pd', 'm5s', 'lega'] as const).find(id => panel[id].includes(r.list)) ?? 'other';
    mapping.set(r.list, party);
    value.result.votes[PANEL.indexOf(party)] += r.votes;
    const list = value.result.lists.find(l => l.name === r.list);
    if (list) list.votes += r.votes; else value.result.lists.push({ name: r.list, votes: r.votes });
    municipalities.set(id, value);
  }
  for (const name of declared) if (!mapping.has(name)) throw new Error(`Missing declared generation mapping ${name}`);
  const result = [...municipalities.values()].map(({ result, ballots }) => {
    result.electors = sum([...ballots.values()].map(b => b.electors));
    result.voters = sum([...ballots.values()].map(b => b.voters));
    result.validVotes = sum(result.votes);
    if (result.electors === 0 || result.voters > result.electors || result.validVotes > result.voters) result.matchable = false;
    result.lists.sort((a, b) => compare(a.name, b.name));
    return result;
  }).sort((a, b) => compare(a.id, b.id));
  return { municipalities: result, mapping: [...mapping].sort(([a], [b]) => compare(a, b)).map(([name, party]) =>
    ({ name, party, method: party === 'other' ? 'changing-complement' as const : 'exact-reviewed-source-label' as const })),
    national: { electors: sum(result.map(m => m.electors)), voters: sum(result.map(m => m.voters)), validVotes: sum(result.map(m => m.validVotes)), votes: PANEL.map((_, i) => sum(result.map(m => m.votes[i]))) } };
}

/** Reports every unmatched unit and both retained masses. Calling code may
 * evaluate matched local geography; national totals always retain all records. */
export function matchGenerationEvents(baseline: GenerationEvent, target: GenerationEvent) {
  const old = new Map(baseline.municipalities.filter(m => m.matchable).map(m => [m.id, m]));
  const pairs = target.municipalities.flatMap(m => m.matchable && old.has(m.id) ? [{ baseline: old.get(m.id)!, target: m }] : []);
  const ids = new Set(pairs.map(p => p.target.id));
  const brief = (m: GenerationMunicipality) => ({ id: m.id, name: m.name, validVotes: m.validVotes });
  const audit: GenerationMatchAudit = { from: baseline.id, to: target.id, matched: pairs.length,
    baselineMunicipalities: baseline.municipalities.length, targetMunicipalities: target.municipalities.length,
    baselineValidVoteFraction: sum(pairs.map(p => p.baseline.validVotes)) / baseline.national.validVotes,
    targetValidVoteFraction: sum(pairs.map(p => p.target.validVotes)) / target.national.validVotes,
    baselineOnly: baseline.municipalities.filter(m => !ids.has(m.id)).map(brief),
    targetOnly: target.municipalities.filter(m => !ids.has(m.id)).map(brief) };
  return { pairs, audit };
}

export async function loadGenerationHistory(root: string): Promise<GenerationHistory> {
  const manifest = await electionManifest(root), events: GenerationEvent[] = [];
  const calibration = await readCalibrationResults(root);
  // Province-bearing sources let us detect homonyms without inferring province
  // for Camera 2018/2022, whose municipal extracts do not provide it.
  const prepared = [];
  for (const s of manifest.sources) {
    const bytes = await readSource(resolve(root, 'data/sources'), s);
    const memberName = s.id === 'camera-20180304' ? 'Camera2018_livComune.txt' : s.member;
    const member = archiveMember(bytes, memberName), rows = delimited(member, ';');
    prepared.push({ s, bytes, memberName, member, rows });
  }
  // Province reorganizations change province labels, not municipality identity.
  // Actual homonyms are those in multiple provinces within ONE event.
  const ambiguousNames = new Set<string>();
  for (const { rows } of prepared) {
    const places = new Map<string, Set<string>>();
    for (const r of rows) {
      const province = r.PROVINCIA ?? r.DESCPROVINCIA; if (!province) continue;
      const identity = rawIdentity(r.REGIONE ?? r.DESCREGIONE ?? r.CIRCOSCRIZIONE, r.COMUNE ?? r.DESCCOMUNE);
      const set = places.get(identity) ?? new Set<string>(); set.add(normalizeName(province)); places.set(identity, set);
    }
    for (const [id, provinces] of places) if (provinces.size > 1) ambiguousNames.add(id);
  }
  for (const { s, memberName, member, rows } of prepared) {
    const kind = s.id.startsWith('camera') ? 'camera' as const : 'european' as const;
    const records: GenerationVoteRecord[] = [], excluded = [], members = [{ name: memberName, sha256: sha256(member), bytes: member.length }];
    let candidateOnly = 0;
    const ordinary = rows.filter(r => generationRegionId(r.REGIONE ?? r.DESCREGIONE ?? r['CIRC-REG'] ?? r.CIRCOSCRIZIONE) !== '02' || kind !== 'camera');
    if (ordinary.length < rows.length) {
      const special = rows.filter(r => generationRegionId(r.REGIONE ?? r.DESCREGIONE ?? r['CIRC-REG'] ?? r.CIRCOSCRIZIONE) === '02');
      const ballots = new Map(special.map(r => [r.COMUNE, r]));
      excluded.push({ reason: 'Camera Valle d Aosta has a different candidate ballot; outside common list-share panel', rows: special.length,
        electors: sum([...ballots.values()].map(r => sourceInteger(r.ELETTORI))), voters: sum([...ballots.values()].map(r => sourceInteger(r.VOTANTI))),
        validVotes: sum(special.map(r => sourceInteger(r.VOTI_CANDIDATO))) });
    }
    if (kind === 'camera' && ['camera-20180304', 'camera-20220925'].includes(s.id)) {
      const adapted = ordinary.map(r => s.id === 'camera-20180304' ? { ...r, 'CIRC-REG': r.CIRCOSCRIZIONE,
        COLLUNINOM: r.COLLEGIOUNINOMINALE, DATANASCITA: r.DATA_NASCITA, DESCRLISTA: r.LISTA,
        VOTILISTA: r.VOTI_LISTA, VOTICANDIDATO: r.VOTI_CANDIDATO } : r);
      // In 2018 a withdrawn/unavailable candidature is explicitly marked
      // CANDIDATO NON PRESENTE: its list votes remain valid, with no candidate
      // total to apportion. Preserve those direct votes instead of converting
      // the source placeholder zero into a negative candidate-only count.
      const absent = adapted.filter(r => r.COGNOME === 'CANDIDATO NON PRESENTE');
      const reconstructed = reconstructHistoricalAttribution(adapted.filter(r => r.COGNOME !== 'CANDIDATO NON PRESENTE'), 'camera');
      candidateOnly = sum(reconstructed.traces.map(t => t.candidateOnly));
      for (const { row: r, votes } of [...reconstructed.output, ...absent.map(row => ({ row, votes: sourceInteger(row.VOTILISTA) }))]) records.push({ region: r['CIRC-REG'], province: null, name: r.COMUNE,
        ballot: r.COLLUNINOM, electors: sourceInteger(r.ELETTORITOT ?? r.ELETTORI), voters: sourceInteger(r.VOTANTITOT ?? r.VOTANTI), list: r.DESCRLISTA, votes });
    } else for (const r of ordinary) records.push({ region: r.REGIONE ?? r.DESCREGIONE ?? r.CIRCOSCRIZIONE,
      province: r.PROVINCIA ?? r.DESCPROVINCIA ?? null, name: r.COMUNE ?? r.DESCCOMUNE,
      ballot: r.CIRCOSCRIZIONE ?? r.DESCCIRCEUROPEA, electors: decimalInteger(r.ELETTORI), voters: decimalInteger(r.VOTANTI),
      list: r.LISTA ?? r.DESCLISTA, votes: decimalInteger(r.VOTI_LISTA ?? r.NUMVOTI) });
    // Fuori-sede ballots identify the voting location, not the municipality of
    // residence: including them in local turnout would invent a denominator.
    for (const name of s.additionalMembers ?? []) {
      const extra = archiveMember(prepared.find(p => p.s.id === s.id)!.bytes, name), rs = delimited(extra, ';');
      members.push({ name, sha256: sha256(extra), bytes: extra.length });
      const ballots = new Map(rs.map(r => [JSON.stringify([r.DESCCIRCEUROPEA, r.DESCPROVINCIA, r.DESCCOMUNE]), r]));
      excluded.push({ reason: 'Fuori-sede votes lack resident-municipality attribution', rows: rs.length,
        electors: sum([...ballots.values()].map(r => sourceInteger(r.ELETTORI))), voters: sum([...ballots.values()].map(r => sourceInteger(r.VOTANTI))),
        validVotes: sum(rs.map(r => sourceInteger(r.NUMVOTI))) });
    }
    const normalized = aggregateGenerationEvent(records, s.panel, ambiguousNames);
    const nationalReference = calibration.results.find(r => r.sourceId === s.id)!;
    events.push({ id: `${kind}-${dateOf(s.id).slice(0, 4)}`, kind, date: dateOf(s.id), sourceId: s.id, sourceSha256: s.sha256, ...normalized,
      audit: { members, rawRows: rows.length, candidateOnly, excluded,
        nationalReference: { source: 'pinned-poll-calibration-results', validVotes: nationalReference.total,
          votes: nationalReference.panel.map(p => p.votes), panelVoteDifferences: normalized.national.votes.map((v, i) => v - nationalReference.panel[i].votes),
          note: s.id === 'camera-20180304' ? 'Municipal candidate-only reconstruction conserves official total; Lega differs by +142 votes and complement by -142 from the separate official national table. No balancing correction is imposed.' :
            s.additionalMembers?.length ? 'Differences are the separately audited fuori-sede ballots excluded from municipality-resident modeling.' : 'Municipal sums reproduce the pinned national four-category totals exactly.' },
        unmatchable: normalized.municipalities.filter(m => !m.matchable).map(m =>
        ({ id: m.id, name: m.name, reason: m.electors === 0 || m.voters > m.electors || m.validVotes > m.voters ? 'Source turnout denominator is zero or vote/turnout counts exceed their denominator' :
          'Same municipality name in several provinces; source omits province', electors: m.electors, voters: m.voters, validVotes: m.validVotes })) } });
  }
  events.sort((a, b) => compare(a.date, b.date));
  const sourceVersions = [`elections-manifest:${sha256(await readFile(resolve(root, 'data/sources/polls/history/elections-manifest.json')))}`,
    ...events.map(e => `${e.sourceId}:${e.sourceSha256}`)];
  return { schemaVersion: 1, version: `municipality-generation-history-v1-${sha256(JSON.stringify({ events, sourceVersions })).slice(0, 16)}`,
    parties: GENERATION_PARTIES, events, sourceVersions,
    geographyPolicy: 'Normalize region/name and reviewed spelling aliases; audit mergers, transfers and ambiguous unprovincialized names as unmatched. National totals retain every included record. Camera excludes candidate-only Valle d Aosta ballots; European excludes unlocated fuori-sede ballots.',
    transitions: events.slice(1).map((event, i) => matchGenerationEvents(events[i], event).audit) };
}
