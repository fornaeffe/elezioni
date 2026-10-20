import type { ResultTable, ScrutinyWarning } from '../../core/types';
import type { GenerationTrace, ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import type { ReformScrutinyInput } from './input';
import type { ComposedReformScrutinyResult } from './scrutiny';
import { seatTableMessages } from './endowments';

type Row = ResultTable['rows'][number];
const json = (value: unknown): string => JSON.stringify(value);
const subject = (value: { kind: string; id: string }): string => `${value.kind}:${value.id}`;

/** Retain transferable presentation rows, never all generated municipal inputs or
 * composed legal stage objects. Repeated provenance is stored once with a count. */
export function createReformPresentation(scenario: ReformGenerationScenario, snapshot: ReformGenerationSnapshot, requested: number) {
  const tables: ResultTable[] = [];
  function table(name: string, columns: string[]) {
    const t: ResultTable = { name, columns, rows: [] }; tables.push(t); return t;
  }
  const overview = table('Reform overview', ['Requested', 'Complete', 'Partial', 'Non-computable', 'Bonus awarded', 'Joint bonus probability / requested']);
  const sensitivity = table('Conditional profile sensitivity', ['Preference profile', 'Chamber profile', 'Requested', 'Joint bonus probability / requested', 'Complete candidate runs / requested', 'Interpretation']);
  const catalogue = table('Political subject names', ['Chamber', 'List', 'Name', 'Coalition']);
  for (const chamber of ['camera', 'senato'] as const) for (const list of scenario.lists[chamber]) catalogue.rows.push({ Chamber: chamber, List: list.id, Name: list.name, Coalition: list.coalitionId });
  const runs = table('Reform runs', ['Simulation', 'Status', 'Bonus', 'Reason']);
  const bonuses = table('Bonus eligibility', ['Simulation', 'Chamber', 'Subject', 'Votes', 'Valid votes', 'At least 42%', 'Status', 'Reason']);
  const votes = table('Reform vote shares', ['Simulation', 'Chamber', 'List', 'Votes', 'Valid votes', 'Vote share %']);
  const entitlement = table('Vote-based entitlements', ['Simulation', 'Chamber', 'Status', 'Ordinary allocation', 'Bonus allocation', 'Special allocation', 'Cap check', 'Cap effects']);
  const territorial = table('Ordinary territorial entitlements', ['Simulation', 'Chamber', 'Level', 'Territory', 'College', 'Subject', 'List', 'Seats']);
  const ledger = table('Seat entitlement and election locations', ['Simulation', 'Seat', 'Category', 'Chamber', 'Subject', 'List', 'Entitlement territory', 'Entitlement college', 'Status', 'Candidate', 'Election subject', 'Election list', 'Election territory', 'Election college', 'Reason']);
  const candidates = table('Candidate outcomes (conditional)', ['Simulation', 'Chamber', 'Candidate', 'Name', 'Seat', 'Category']);
  const stages = table('Legal stages and blockers', ['Simulation', 'Chamber', 'Norm', 'Status', 'Reason', 'Affected IDs']);
  const decisions = table('Admission decisions', ['Simulation', 'Chamber', 'Subject', 'Decision', 'Reason', 'Law']);
  const transfers = table('Cap and territorial transfers', ['Simulation', 'Trace']);
  const recovery = table('Candidate recovery traces', ['Simulation', 'Trace']);
  const exclusions = table('I11 zero-entitlement exclusions', ['Simulation', 'Chamber', 'Territory', 'List', 'Reason']);
  const draws = table('Legal draws (including I03)', ['Simulation', 'Rule', 'Norm', 'Context', 'Chosen', 'Alternatives', 'Stream']);
  const metadata = table('Models, references and conditions', ['Kind', 'Value', 'Occurrences']);
  const metadataRows = new Map<string, Row>();
  const warnings = new Map<string, ScrutinyWarning & { occurrences: number }>();
  let complete = 0, partial = 0, failed = 0, awarded = 0;
  const meta = (kind: string, value: unknown) => {
    const text = json(value), key = json([kind, text]), existing = metadataRows.get(key);
    if (existing) existing.Occurrences = Number(existing.Occurrences) + 1;
    else { const row = { Kind: kind, Value: text, Occurrences: 1 }; metadataRows.set(key, row); metadata.rows.push(row); }
  };
  const warn = (w: ScrutinyWarning) => {
    const key = json([w.code, w.severity, w.message]), existing = warnings.get(key);
    if (existing) existing.occurrences++;
    else warnings.set(key, { ...w, occurrences: 1 });
  };
  meta('Algorithm and model identity', { algorithm: scenario.algorithmId, models: scenario.modelIds, modelVersion: scenario.modelVersion,
    snapshotVersion: scenario.snapshotVersion, preferenceProfile: scenario.preferenceProfileId, chamberProfile: scenario.chamberProfileId,
    seed: scenario.seed, conversion: scenario.conversion, mode: scenario.mode });
  meta('Preference profile (sensitivity assumption)', snapshot.models.preferences.profiles.find(p => p.id === scenario.preferenceProfileId));
  meta('Chamber covariance profile (sensitivity assumption)', snapshot.models.chambers.profiles.find(p => p.id === scenario.chamberProfileId));
  meta('Source versions', [...snapshot.sourceVersions, ...snapshot.models.sourceVersions]);
  meta('Model and geography assumptions', [...snapshot.assumptions, ...snapshot.models.assumptions]);
  seatTableMessages(snapshot.seats).forEach((notice, i) => warn({ code: `R26_SEAT_TABLE_${i + 1}`, electionKind: 'politiche', ...notice }));
  warn({ code: 'R26_CONDITIONAL_CANDIDATES', electionKind: 'politiche', severity: 'warning', message:
    'Identità, seggi coperti e recuperi con preferenze sintetiche sono condizionati al modello e al profilo scelto; non sono previsioni individuali. I profili descrivono sensibilità, non intervalli di incertezza calibrata.' });
  warn({ code: 'R26_PAIRED_MEAN_MODE', electionKind: 'politiche', severity: 'info', message:
    'La quota comune è un ancoraggio in log-odds; i riferimenti espliciti di Camera/Senato hanno precedenza. La modalità media conserva incertezza e covarianza tra rami. La frequenza del premio congiunto dipende dal profilo scelto: rieseguire gli stessi seed con profili diversi per valutarne la sensibilità.' });
  return {
    add(input: ReformScrutinyInput, result: ComposedReformScrutinyResult, trace: GenerationTrace) {
      const sim = input.simulationId;
      if (result.status === 'complete') complete++; else if (result.status === 'partial') partial++; else failed++;
      if (result.bonus.status === 'awarded') awarded++;
      const reason = result.bonus.status === 'not-awarded' ? result.bonus.reasons : result.bonus.status === 'non-computable' ? result.bonus.blockers : [];
      runs.rows.push({ Simulation: sim, Status: result.status, Bonus: result.bonus.status, Reason: json(reason) });
      for (const c of result.bonus.firstSubjects) bonuses.rows.push({ Simulation: sim, Chamber: c.chamber, Subject: subject(c.subject), Votes: c.votes,
        'Valid votes': c.validVotes, 'At least 42%': c.reaches42Percent, Status: result.bonus.status, Reason: json(reason) });
      for (const c of result.chambers) {
        const camera = c.cameraTerritorial, senate = c.senateTerritorial;
        if (camera?.constituencies.status === 'complete') for (const t of camera.constituencies.targets) territorial.rows.push({ Simulation: sim, Chamber: c.chamber, Level: 'constituency-subject', Territory: t.territoryId, College: null, Subject: subject(t.subject), List: null, Seats: t.ordinarySeats });
        if (camera?.lists.status === 'complete') for (const t of camera.lists.targets) territorial.rows.push({ Simulation: sim, Chamber: c.chamber, Level: 'constituency-list', Territory: t.territoryId, College: null, Subject: subject(t.subject), List: t.listId, Seats: t.ordinarySeats });
        if (senate?.lists.status === 'complete') for (const t of senate.lists.targets) territorial.rows.push({ Simulation: sim, Chamber: c.chamber, Level: 'region-list', Territory: t.regionId, College: null, Subject: subject(t.subject), List: t.listId, Seats: t.ordinarySeats });
        for (const allocation of [camera?.colleges, senate?.colleges]) if (allocation?.status === 'complete') for (const t of allocation.targets) territorial.rows.push({ Simulation: sim, Chamber: c.chamber, Level: 'college-list', Territory: t.territoryId, College: t.collegeId, Subject: subject(t.subject), List: t.listId, Seats: t.ordinarySeats });
        for (const v of c.figures?.national.lists ?? []) votes.rows.push({ Simulation: sim, Chamber: c.chamber, List: v.listId, Votes: v.votes,
          'Valid votes': c.figures!.national.validVotes, 'Vote share %': v.percentage.status === 'defined' ? Number(v.percentage.value.numerator) / Number(v.percentage.value.denominator) : null });
        const n = c.national, r = c.regional;
        entitlement.rows.push({ Simulation: sim, Chamber: c.chamber, Status: n?.status ?? r?.status ?? 'non-computable',
          'Ordinary allocation': json(n?.status === 'complete' ? n.listTargets : r?.status === 'complete' ? r.targets : null),
          'Bonus allocation': json(result.bonus.awards.filter(a => a.chamber === c.chamber)), 'Special allocation': json({ special: c.special.seats, taa: c.taa && c.taa.status !== 'non-computable' ? c.taa.seats : null }),
          'Cap check': json(n?.capCheck ?? r?.cap?.check ?? null), 'Cap effects': json(n?.capCorrection ?? r?.cap ?? null) });
        for (const e of c.recovery?.excludedLists ?? []) exclusions.rows.push({ Simulation: sim, Chamber: c.chamber, Territory: e.territoryId, List: e.listId, Reason: e.reason });
      }
      const names = new Map(input.candidates.map(c => [c.id, c.name]));
      for (const seat of result.seats) {
        const e = seat.entitlement, at = seat.status === 'filled' ? seat.electedAt : null;
        ledger.rows.push({ Simulation: sim, Seat: seat.seatId, Category: seat.category, Chamber: e.chamber, Subject: subject(e.subject), List: e.listId,
          'Entitlement territory': e.territoryId, 'Entitlement college': e.collegeId, Status: seat.status, Candidate: seat.status === 'filled' ? seat.candidateId : null,
          'Election subject': at ? subject(at.subject) : null, 'Election list': at?.listId ?? null, 'Election territory': at?.territoryId ?? null,
          'Election college': at?.collegeId ?? null, Reason: seat.status === 'filled' ? null : seat.reason });
        if (seat.status === 'filled') candidates.rows.push({ Simulation: sim, Chamber: e.chamber, Candidate: seat.candidateId, Name: names.get(seat.candidateId) ?? seat.candidateId, Seat: seat.seatId, Category: seat.category });
      }
      for (const s of result.stages) {
        stages.rows.push({ Simulation: sim, Chamber: s.chamber, Norm: s.norm, Status: s.status, Reason: s.status === 'complete' ? null : s.reason, 'Affected IDs': s.status === 'complete' ? null : json(s.affectedIds) });
        if (s.status === 'non-computable') warn({ code: `R26_${s.norm}_NON_COMPUTABLE`, electionKind: 'politiche', simulationId: sim, severity: 'error', message: `${s.norm} ${s.chamber ?? ''}: ${s.reason}. La simulazione resta nel denominatore richiesto; nessuna riestrazione o modifica delle dotazioni.` });
      }
      for (const d of result.subjectDecisions) decisions.rows.push({ Simulation: sim, Chamber: d.chamber, Subject: subject(d.subject), Decision: d.decision, Reason: d.reason, Law: d.lawReference });
      for (const t of result.transfers) transfers.rows.push({ Simulation: sim, Trace: json(t) });
      for (const t of result.recoveries) recovery.rows.push({ Simulation: sim, Trace: json(t) });
      for (const d of result.draws) draws.rows.push({ Simulation: sim, Rule: d.rule, Norm: d.norm, Context: d.contextId, Chosen: d.chosenId, Alternatives: json(d.alternativeIds), Stream: d.streamId });
      result.warnings.forEach(warn);
      trace.warnings.forEach(w => warn({ ...w, severity: 'warning', simulationId: sim, electionKind: 'politiche' }));
      result.provenance.forEach(p => meta('Supplied/generated provenance', p));
      result.conditions.forEach(c => meta('Candidate/vote condition', c));
      result.seatTables.forEach(s => meta('Census / decree seat provenance', s));
      result.interpretations.forEach(i => meta('Applied interpretation', i));
      if (result.chamberModel.kind === 'generated') {
        const { references, ...parameters } = result.chamberModel;
        meta('Paired model and covariance assumptions', parameters);
        references.forEach(r => meta('Resolved chamber reference', r));
      } else meta('Supplied chamber provenance', result.chamberModel);
      meta('Generation versions', { streamVersion: trace.streamVersion, snapshotVersion: trace.snapshotVersion, modelVersion: trace.modelVersion });
      trace.normalization.forEach(n => meta('Reference normalization', n));
    },
    failure(sim: number, error: unknown) {
      failed++;
      const message = error instanceof Error ? error.message : String(error);
      runs.rows.push({ Simulation: sim, Status: 'non-computable', Bonus: 'unknown', Reason: message });
      warn({ code: 'R26_RUN_ERROR', simulationId: sim, electionKind: 'politiche', severity: 'error', message });
    },
    finish() {
      overview.rows = [{ Requested: requested, Complete: complete, Partial: partial, 'Non-computable': failed, 'Bonus awarded': awarded,
        'Joint bonus probability / requested': awarded / requested }];
      sensitivity.rows = [{ 'Preference profile': scenario.preferenceProfileId, 'Chamber profile': scenario.chamberProfileId, Requested: requested,
        'Joint bonus probability / requested': awarded / requested, 'Complete candidate runs / requested': complete / requested,
        Interpretation: 'Conditional on this profile pair; compare reruns with the same seed and alternative profiles. These are not calibrated uncertainty intervals.' }];
      const warningTable = table('Warnings and errors', ['Code', 'Severity', 'First simulation', 'Message', 'Occurrences']);
      warningTable.rows = [...warnings.values()].map(w => ({ Code: w.code, Severity: w.severity ?? 'warning', 'First simulation': w.simulationId ?? null, Message: w.message, Occurrences: w.occurrences }));
      return { tables, warnings: [...warnings.values()].map(({ occurrences, ...w }) => ({ ...w, message: occurrences > 1 ? `${w.message} (${occurrences} occorrenze)` : w.message })) };
    }
  };
}

/** Charts read the same exported table contract as the UI and CSV. Bonus
 * coalitions remain separate subjects, even when candidates have affiliations. */
export function buildReformSeatChart(table: ResultTable | undefined, chamber: string, simulation: number, scenario?: ReformGenerationScenario | null) {
  const totals = new Map<string, number>();
  for (const r of table?.rows ?? []) if (r.Chamber === chamber && r.Simulation === simulation) {
    const label = json([r.Category, r.Category === 'bonus' ? r.Subject : r.List ?? r.Subject, r.Status]);
    totals.set(label, (totals.get(label) ?? 0) + 1);
  }
  const lists = scenario?.lists[chamber === 'camera' ? 'camera' : 'senato'] ?? [];
  const name = (owner: string) => {
    if (owner.startsWith('coalition:')) {
      const members = lists.filter(l => l.coalitionId === owner.slice('coalition:'.length));
      return members.length ? `Coalizione: ${members.map(l => l.name).join(' + ')}` : owner;
    }
    const id = owner.startsWith('list:') ? owner.slice('list:'.length) : owner;
    return lists.find(l => l.id === id)?.name ?? id;
  };
  const categories: Record<string, string> = { ordinary: 'Ordinari', bonus: 'Premio', 'special-uninominal': 'Uninominali speciali', 'taa-proportional': 'Proporzionali TAA' };
  const statuses: Record<string, string> = { filled: 'coperti', unfilled: 'non coperti', unresolved: 'irrisolti' };
  return [...totals].map(([id, seats]) => {
    const [category, owner, status] = JSON.parse(id) as string[];
    return { id, label: `${categories[category] ?? category} / ${name(owner)} / ${statuses[status] ?? status}`, seats };
  });
}
