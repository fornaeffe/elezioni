/** Rosatellum ballot/candidature assembly. The scrutiny core and its R policies stay unchanged. */
import { generatePoliticsCandidates } from '../candidate-generation';
import { adaptGeneratedPoliticsRamo } from '../adapter';
import { plurinominalCandidacyCountSharesToFractions } from '../../scenario/politics';
import { ROSATELLUM, chambers, type PoliticsScenario, type Location, type Chamber } from '../scenario/schema';
import { effectiveFilings, effectivePlacements, renewed } from '../scenario/effective';
import type { DomainContext } from '../scenario/validation';
import type { PoliticsCandidateGenerationSource, PoliticsCandidateGenerationRamoSource,
  PoliticsCollegeUniRow, PoliticsCollegePluriRow, PoliticsGeneratedListRow } from '../types';
import type { PoliticsSimulationRequest } from '../simulation-scrutiny';
import type { GeneratedVotes } from '../generation/types';

export interface RosatellumBallot {
  id: string;
  chamber: Chamber;
  /** Explicit physical-to-legal crosswalk; equal-looking codes never establish a relationship. */
  uninominal: string;
  constituency: string;
  regionId: string;
  /** VdA and Senate TAA have no proportional college: its filing location is the actual uninominal college. */
  plurinominal: string | null;
}
export interface RosatellumSeats {
  schemaVersion: 1;
  kind: 'rosatellum-seats';
  version: string;
  geographyVersion: string;
  ballots: RosatellumBallot[];
  chambers: { chamber: Chamber; totalSeats: number; uninominal: PoliticsCollegeUniRow[];
    plurinominal: (PoliticsCollegePluriRow & { MAX_CANDIDATI: number })[] }[];
}
const ticket = (l: PoliticsScenario['subjects']['lists'][number]) => l.coalitionId === null ? `list:${l.id}` : `coalition:${l.coalitionId}`;
export function ballotLocation(s: PoliticsScenario, b: RosatellumBallot): Location {
  return { law: ROSATELLUM, geographyVersion: s.data.lawAssets[ROSATELLUM].geography.version, chamber: b.chamber,
    level: b.plurinominal === null ? 'uninominal' : 'plurinominal', code: b.plurinominal ?? b.uninominal };
}
export function rosatellumCoverage(s: PoliticsScenario, seats: RosatellumSeats) {
  if (seats.kind !== 'rosatellum-seats' || seats.schemaVersion !== 1 || seats.version !== s.data.lawAssets[ROSATELLUM].seats.version ||
    seats.geographyVersion !== s.data.lawAssets[ROSATELLUM].geography.version) throw new Error('ROSATELLUM_SEATS_MISMATCH');
  const ids = new Set<string>();
  for (const b of seats.ballots) {
    const grid = seats.chambers.find(c => c.chamber === b.chamber);
    const uni = grid?.uninominal.find(u => String(u.UNI_COD) === b.uninominal);
    if (ids.has(b.id) || !uni || String(uni.CIRC_COD) !== b.constituency ||
      (uni.PLURI_COD === null ? null : String(uni.PLURI_COD)) !== b.plurinominal ||
      b.plurinominal === null && b.regionId !== '02' && !(b.chamber === 'senato' && b.regionId === '04') ||
      b.plurinominal === null && !grid!.plurinominal.some(p => p.PLURI_COD === null && String(p.CIRC_COD) === b.constituency) ||
      b.plurinominal !== null && !grid!.plurinominal.some(p => String(p.PLURI_COD) === b.plurinominal && String(p.CIRC_COD) === b.constituency))
      throw new Error('ROSATELLUM_BALLOT_CROSSWALK');
    ids.add(b.id);
  }
  for (const ch of chambers) {
    const grid = seats.chambers.filter(c => c.chamber === ch);
    if (grid.length !== 1 || grid[0].totalSeats !== (ch === 'camera' ? 392 : 196) ||
      grid[0].plurinominal.reduce((n, p) => n + p.SEGGI_PLURI, grid[0].uninominal.length) !== grid[0].totalSeats ||
      new Set(grid[0].uninominal.map(u => String(u.UNI_COD))).size !== grid[0].uninominal.length ||
      new Set(grid[0].plurinominal.map(p => JSON.stringify([String(p.CIRC_COD), p.PLURI_COD === null ? null : String(p.PLURI_COD)]))).size !== grid[0].plurinominal.length ||
      grid[0].uninominal.some(u => seats.ballots.filter(b => b.chamber === ch && b.uninominal === String(u.UNI_COD)).length !== 1) ||
      grid[0].plurinominal.some(p => !Number.isSafeInteger(p.SEGGI_PLURI) || p.SEGGI_PLURI < 0 || !Number.isSafeInteger(p.MAX_CANDIDATI) || p.MAX_CANDIDATI < (p.PLURI_COD === null ? 0 : 1) || p.MAX_CANDIDATI > 4))
      throw new Error('ROSATELLUM_LEGAL_GRID');
  }
  for (const f of s.lawSettings[ROSATELLUM].filings) for (const l of f.locations)
    if (!seats.ballots.some(b => { const expected = ballotLocation(s, b); return expected.chamber === l.chamber && expected.level === l.level && expected.code === l.code; }))
      throw new Error('ROSATELLUM_FILING_LOCATION: use a proportional college, or a statutory uninominal-only ballot');
  const coverage = seats.ballots.filter(b => renewed(s, b.chamber)).map(b => ({ chamber: b.chamber, collegeId: b.id,
    listIds: s.subjects.lists.filter(l => effectiveFilings(s, ROSATELLUM, l.id).some(f => {
      const location = ballotLocation(s, b); return f.chamber === b.chamber && f.level === location.level && f.code === location.code;
    })).map(l => l.id) }));
  for (const list of s.subjects.lists) if (list.minority && coverage.some(c => c.listIds.includes(list.id) &&
    seats.ballots.find(b => b.id === c.collegeId)!.regionId !== list.minority!.regionId)) throw new Error('ROSATELLUM_MINORITY_FILING_REGION');
  return coverage;
}
export function prepareRosatellumAssembly(s: PoliticsScenario, seats: RosatellumSeats, domain: Pick<DomainContext, 'geographyRelations'>,
  references: GeneratedVotes['trace']['references']) {
  const coverage = rosatellumCoverage(s, seats), pins = effectivePlacements(s, ROSATELLUM, domain.geographyRelations);
  const person = (id: string) => s.candidates.find(c => c.id === id)!;
  for (const p of pins) {
    if (p.role === 'uninominal' && p.position !== 1 || p.role === 'plurinominal' && p.owner.kind !== 'list')
      throw new Error('ROSATELLUM_PIN_ROLE');
    if (p.affiliationListId !== null) {
      const affiliated = s.subjects.lists.find(l => l.id === p.affiliationListId)!;
      if (p.owner.kind === 'list' ? affiliated.id !== p.owner.id : affiliated.coalitionId !== p.owner.id)
        throw new Error('ROSATELLUM_PIN_AFFILIATION');
    }
    const uses = pins.filter(q => q.candidateId === p.candidateId);
    // DPR 361/1957, art. 19: «più di un collegio uninominale» / «più di cinque collegi plurinominali»;
    // «con lo stesso contrassegno»; D.Lgs. 533/1993, art. 9 refers to the same candidature rules.
    if (uses.filter(q => q.role === 'uninominal').length > 1 || uses.filter(q => q.role === 'plurinominal').length > 5 ||
      new Set(uses.filter(q => q.role === 'plurinominal').map(q => q.owner.id)).size > 1)
      throw new Error('ROSATELLUM_PIN_INCOMPATIBILITY');
  }
  const assemblers = chambers.filter(ch => renewed(s, ch)).map(ch => {
      const grid = seats.chambers.find(c => c.chamber === ch)!, physical = seats.ballots.filter(b => b.chamber === ch);
      const activeIds = new Set(coverage.filter(c => c.chamber === ch).flatMap(c => c.listIds));
      const lists = s.subjects.lists.filter(l => activeIds.has(l.id));
      const metadata: PoliticsGeneratedListRow[] = lists.map(l => ({ LISTA: l.id,
        COALIZIONE: l.kind === 'other' ? null : ticket(l), kind: l.kind,
        ...(l.minority ? { minorityRegion: l.minority.regionId } : {}),
        // Presence of an explicit filing enables the retained zero-support candidature policy.
        filing: { camera: [], senato: [] } }));
      const templates: PoliticsCandidateGenerationRamoSource = { candidati_uni: [], candidati_pluri: [] };
      const consumed = new Set<string>();
      for (const b of physical) {
        const filed = lists.filter(l => coverage.find(c => c.collegeId === b.id)!.listIds.includes(l.id) && l.kind !== 'other');
        for (const owner of [...new Set(filed.map(ticket))]) {
          const eligible = filed.filter(l => ticket(l) === owner);
          const selected = pins.filter(p => p.location.chamber === ch && p.role === 'uninominal' && p.location.code === b.uninominal &&
            (p.owner.kind === 'coalition' ? owner === `coalition:${p.owner.id}` : eligible.some(l => l.id === p.owner.id)));
          if (selected.length > 1) throw new Error('ROSATELLUM_COMPETING_UNINOMINAL_PINS');
          selected.forEach(p => consumed.add(p.id));
          const pin = selected[0];
          templates.candidati_uni.push({ COALIZIONE: owner, UNI_COD: b.uninominal, eligibleLists: eligible.map(l => l.id),
            LISTA_MINORANZA: pin && 'minority' in pin && pin.minority ? pin.affiliationListId ?? eligible[0].id :
              eligible.length === 1 && eligible[0].minority ? eligible[0].id : null,
            CANDIDATO_ID: pin?.candidateId ?? null, DATA_NASCITA: pin ? person(pin.candidateId).birthDate : null });
        }
      }
      for (const p of grid.plurinominal) for (const list of lists.filter(l => l.kind !== 'other' && physical.some(b =>
        b.plurinominal === String(p.PLURI_COD) && coverage.find(c => c.collegeId === b.id)!.listIds.includes(l.id)))) {
        for (let position = 1; position <= p.MAX_CANDIDATI; position++) {
          const pin = pins.find(q => q.location.chamber === ch && q.role === 'plurinominal' && q.location.code === String(p.PLURI_COD) && q.owner.id === list.id && q.position === position);
          if (pin) consumed.add(pin.id);
          templates.candidati_pluri.push({ CIRC_COD: String(p.CIRC_COD), PLURI_COD: String(p.PLURI_COD), LISTA: list.id,
            NUMERO_CANDIDATO: position, MINORANZA: list.minority !== null || !!(pin && 'minority' in pin && pin.minority),
            CANDIDATO_ID: pin?.candidateId ?? null, DATA_NASCITA: pin ? person(pin.candidateId).birthDate : null });
        }
      }
      if (pins.some(p => p.location.chamber === ch && !consumed.has(p.id))) throw new Error('ROSATELLUM_UNMATCHED_ACTIVE_PIN');
      const settings = s.lawSettings[ROSATELLUM].candidateGeneration;
      const input: PoliticsCandidateGenerationSource = { simulazioni: 1, frazione_uni_in_pluri: settings.uninominalToPlurinominalShare,
        frazioni_pluricandidature: plurinominalCandidacyCountSharesToFractions(settings.plurinominalCandidacyCountShares),
        default_data_nascita: `${Number(s.electionDate.slice(0, 4)) - 50}-01-01`,
        liste: metadata.map(l => ({ ...l, PERCENTUALE: references.find(r => r.scope.kind === 'national' && r.listIds[ch] === l.LISTA)?.[ch]?.share ?? 0 })),
        camera: ch === 'camera' ? templates : { candidati_uni: [], candidati_pluri: [] },
        senato: ch === 'senato' ? templates : { candidati_uni: [], candidati_pluri: [] } };
      // Pin tokens cannot collide with the retained generator's UNI_/PLURI_ identities.
      const declaredTokens = new Map<string, string>();
      for (const row of [...templates.candidati_uni, ...templates.candidati_pluri]) if (row.CANDIDATO_ID !== null) {
        const id = row.CANDIDATO_ID, token = `declared:${declaredTokens.size}`;
        const existing = [...declaredTokens].find(([, value]) => value === id)?.[0];
        if (!existing) declaredTokens.set(token, id);
        row.CANDIDATO_ID = existing ?? token;
      }
      let namespace = 'generated:';
      while (s.candidates.some(c => c.id.startsWith(namespace))) namespace += 'generated:';
      return (simulation: number, votes: GeneratedVotes) => {
      const candidates = generatePoliticsCandidates(input, { seed: `${s.execution.simulationSeed}:rosatellum-candidates:${ch}:${simulation}` })[ch];
      const candidateId = (id: string | null) => id === null ? null : declaredTokens.get(id) ?? `${namespace}${ch}:${simulation}:${id}`;
      candidates.candidati_uni_sim.forEach(c => { c.SIM = simulation; c.CANDIDATO_ID = candidateId(c.CANDIDATO_ID)!; });
      candidates.candidati_pluri_sim.forEach(c => { c.SIM = simulation; c.CANDIDATO_ID = candidateId(c.CANDIDATO_ID); });
      const uniVotes = candidates.candidati_uni_sim.map(c => {
        const b = physical.find(b => b.uninominal === String(c.UNI_COD))!, row = votes.counts.get(b.id);
        if (!row) throw new Error('ROSATELLUM_MISSING_BALLOT_COUNTS');
        return { ...c, VOTI_CANDIDATO: lists.filter(l => ticket(l) === c.COALIZIONE).reduce((n, l) => n + (row.votes.get(l.id) ?? 0), 0) };
      });
      const rows = physical.flatMap(b => {
        const row = votes.counts.get(b.id);
        if (!row || !Number.isSafeInteger(row.electors) || row.electors < 0 || [...row.votes.values()].some(n => !Number.isSafeInteger(n) || n < 0) ||
          [...row.votes.values()].reduce((a, b) => a + b, 0) > row.electors) throw new Error('ROSATELLUM_INVALID_BALLOT_COUNTS');
        return coverage.find(c => c.collegeId === b.id)!.listIds.map(id => {
          const list = lists.find(l => l.id === id)!, candidate = uniVotes.find(c => String(c.UNI_COD) === b.uninominal && c.COALIZIONE === ticket(list));
          return { SIM: simulation, UNI_COD: b.uninominal, LISTA: id, VOTI_LISTA_SIM: row.votes.get(id) ?? 0,
            CANDIDATO_ID: list.kind === 'other' ? null : candidate!.CANDIDATO_ID,
            CAND_MINORANZA: candidate?.LISTA_MINORANZA !== null && candidate?.LISTA_MINORANZA !== undefined && candidate?.LISTA_MINORANZA !== false,
            MINORANZA: list.minority !== null };
        });
      });
      const adapted = adaptGeneratedPoliticsRamo(ch, { liste: metadata, uni_liste_sim: rows, candidati_uni_sim: uniVotes,
        candidati_pluri_sim: candidates.candidati_pluri_sim,
        uni: grid.uninominal.map(u => ({ ...u, CIRC_COD: String(u.CIRC_COD), PLURI_COD: u.PLURI_COD === null ? null : String(u.PLURI_COD), UNI_COD: String(u.UNI_COD) })),
        pluri: grid.plurinominal.map(p => ({ ...p, CIRC_COD: String(p.CIRC_COD), PLURI_COD: p.PLURI_COD === null ? null : String(p.PLURI_COD) })),
        candidati_pluri_template: templates.candidati_pluri.map(p => ({ LISTA: p.LISTA, PLURI_COD: p.PLURI_COD, CIRC_COD: p.CIRC_COD!, MINORANZA: p.MINORANZA })) });
      return { input: adapted.simulations[0].input, context: { ...adapted.context, totale_seggi: grid.totalSeats } };
      };
  });
  return (simulation: number, votes: GeneratedVotes): Extract<PoliticsSimulationRequest, { algorithmId: typeof ROSATELLUM }> =>
    ({ algorithmId: ROSATELLUM, simulationId: simulation, chambers: assemblers.map(assemble => assemble(simulation, votes)) });
}
