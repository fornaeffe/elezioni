import type { ScrutinyWarning } from './types';

export type DiagnosticSeverity = 'error' | 'warning' | 'info';
export interface DiagnosticFilter {
  severity?: string; code?: string; chamber?: string; text?: string; reason?: string; simulation?: number;
}
export const diagnosticTableName = 'Diagnostic events v1';
const fields = ['simulationId', 'scope', 'code', 'severity', 'message', 'occurrences', 'chamber', 'reason',
  'lawReference', 'todoReference', 'interpretationId', 'seatId', 'drawContextId'] as const;
export const diagnosticColumns = ['Simulation', 'Scope', 'Code', 'Severity', 'Message', 'Occurrences', 'Chamber', 'Reason',
  'Law', 'TODO', 'Interpretation', 'Seat', 'Draw'];
export const diagnosticCells = (w: ScrutinyWarning) => fields.map(field => w[field] ?? (field === 'occurrences' ? 1 : field === 'severity' ? 'warning' : null));
export function diagnosticEvent(cells: (string | number | boolean | null)[], columns: string[]): ScrutinyWarning {
  const value = Object.fromEntries(fields.flatMap((field, index) => {
    const cell = cells[columns.indexOf(diagnosticColumns[index])];
    return cell == null ? [] : [[field, cell]];
  })) as unknown as ScrutinyWarning;
  if (typeof value.code !== 'string' || !value.code || typeof value.message !== 'string' || !['info', 'warning', 'error'].includes(value.severity ?? 'warning') ||
    !Number.isSafeInteger(value.occurrences ?? 1) || (value.occurrences ?? 1) < 1 ||
    value.simulationId !== undefined && (!Number.isSafeInteger(value.simulationId) || value.simulationId < 1) ||
    value.scope !== undefined && !['global', 'simulation', 'execution'].includes(value.scope) ||
    value.scope === 'simulation' && value.simulationId === undefined || value.scope === 'global' && value.simulationId !== undefined ||
    value.chamber !== undefined && !['camera', 'senato'].includes(value.chamber) ||
    ['reason', 'lawReference', 'todoReference', 'interpretationId', 'seatId', 'drawContextId'].some(key => key in value && typeof value[key as keyof ScrutinyWarning] !== 'string')) {
    throw new Error('Invalid diagnostic event');
  }
  return { ...value, electionKind: 'politiche' };
}
export function diagnosticTitle(code: string): string {
  const titles: Record<string, string> = {
    R26_CONDITIONAL_CANDIDATES: 'Candidati e recuperi condizionati al modello',
    R26_PAIRED_MEAN_MODE: 'Ipotesi del modello congiunto Camera e Senato',
    R26_RUN_ERROR: 'Simulazione non eseguita',
    I15_DISJOINED_GROUP_CORRECTION: 'Correzione dei gruppi disgiunti',
    I16_REUSED_RECIPIENT_REMAINDER: 'Riutilizzo dei resti dei destinatari',
    I13_INTEGER_SEAT_REMOVAL: 'Sottrazione di seggi interi',
    I14_REPEATED_REMAINDER_PASS: 'Passaggi ripetuti sui resti',
    I03_DRAW: 'Parità risolta mediante sorteggio',
    I07_CROSS_REGION_RECOVERY: 'Recupero in un’altra regione',
    I08_PROVISIONAL_SEATS: 'Dotazioni territoriali provvisorie',
    I10_COALITION_RECOVERY_PRIORITY: 'Priorità di recupero nella coalizione',
    I12_TERRITORIAL_ELIGIBILITY: 'Ammissibilità territoriale dei candidati',
    NORMALIZED_SHARES: 'Quote dei voti normalizzate',
    RESULT_CANCELLED: 'Calcolo annullato', RESULT_INTERRUPTED: 'Calcolo interrotto',
    RESULT_EXECUTION_ERROR: 'Errore di esecuzione', RESULT_STORAGE_ERROR: 'Errore di salvataggio', WORKER_ERROR: 'Errore di esecuzione'
  };
  if (titles[code]) return titles[code];
  if (/^R26_SEAT_TABLE_\d+$/.test(code)) return 'Assunzioni sulle dotazioni territoriali';
  const stage = /^R26_(N\d+)_NON_COMPUTABLE$/.exec(code);
  return stage ? `Stadio ${stage[1]} non calcolabile` : code;
}
export const diagnosticSeverityLabel = (severity: string | undefined) => severity === 'error' ? 'Errore' : severity === 'info' ? 'Informazione' : 'Avviso';
/** Only explicit scope or a finite registry of established context codes proves
 * global applicability. A missing simulation ID alone proves nothing. */
export function diagnosticScope(w: ScrutinyWarning): 'global' | 'simulation' | 'execution' | 'unknown' {
  if (w.scope) return w.scope;
  if (w.simulationId !== undefined) return 'simulation';
  if (/^RESULT_/.test(w.code) || w.code === 'WORKER_ERROR') return 'execution';
  if (w.code === 'R26_CONDITIONAL_CANDIDATES' || w.code === 'R26_PAIRED_MEAN_MODE' || /^R26_SEAT_TABLE_\d+$/.test(w.code)) return 'global';
  return 'unknown';
}
export function diagnosticMatches(w: ScrutinyWarning, filter: DiagnosticFilter): boolean {
  return (!filter.code || filter.code === w.code) && (!filter.severity || filter.severity === (w.severity ?? 'warning')) &&
    (!filter.chamber || filter.chamber === w.chamber) && (filter.simulation === undefined || filter.simulation === w.simulationId) &&
    (!filter.reason || `${w.reason ?? ''}\n${w.message}`.toLocaleLowerCase('it').includes(filter.reason.toLocaleLowerCase('it'))) &&
    (!filter.text || `${diagnosticTitle(w.code)}\n${w.code}\n${w.message}`.toLocaleLowerCase('it').includes(filter.text.toLocaleLowerCase('it')));
}
export interface DiagnosticCount { occurrences: number; simulations: number; lastSimulation: number }
export interface DiagnosticGroup extends DiagnosticCount { code: string; severity: DiagnosticSeverity; title: string }
export function createMessageSummary(requested: number, processed: number, exact: boolean, filter: DiagnosticFilter = {}) {
  if (!Number.isSafeInteger(requested) || requested < 1 || !Number.isSafeInteger(processed) || processed < 0 || processed > requested) throw new Error('Invalid diagnostic denominator');
  const empty = (): DiagnosticCount => ({ occurrences: 0, simulations: 0, lastSimulation: 0 });
  const groups = new Map<string, DiagnosticGroup>();
  const totals = { error: empty(), warning: empty(), info: empty() };
  let lastSimulation = 0;
  function count(target: DiagnosticCount, w: ScrutinyWarning) {
    target.occurrences += w.occurrences ?? 1;
    if (exact && w.simulationId !== undefined) {
      if (!Number.isSafeInteger(w.simulationId) || w.simulationId < 1 || w.simulationId > processed) throw new Error('Invalid diagnostic simulation/coverage');
      if (target.lastSimulation !== w.simulationId) target.simulations++;
      target.lastSimulation = w.simulationId;
    }
  }
  return {
    add(w: ScrutinyWarning) {
      if (diagnosticScope(w) !== 'simulation') return;
      if (exact && w.simulationId !== undefined) {
        if (w.simulationId < lastSimulation) throw new Error('Diagnostic ledger is not in simulation order');
        lastSimulation = w.simulationId;
      }
      if (!diagnosticMatches(w, filter)) return;
      const severity = w.severity ?? 'warning', key = JSON.stringify([severity, w.code]);
      const group = groups.get(key) ?? { ...empty(), code: w.code, severity, title: diagnosticTitle(w.code) };
      count(group, w); count(totals[severity], w); groups.set(key, group);
    },
    finish(offset = 0) {
      const rank = { error: 0, warning: 1, info: 2 };
      const rows = [...groups.values()].sort((a, b) => rank[a.severity] - rank[b.severity] || b.simulations - a.simulations || b.occurrences - a.occurrences || a.code.localeCompare(b.code));
      return { requested, processed, exact, totals, groups: rows.slice(offset, offset + 50), groupCount: rows.length };
    }
  };
}
export type MessageSummary = ReturnType<ReturnType<typeof createMessageSummary>['finish']> & { contextAvailable?: boolean };
