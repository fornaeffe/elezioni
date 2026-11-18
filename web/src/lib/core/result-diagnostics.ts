import type { ResultTable } from './types';

/** Counts the presentation warning ledger, including its within-run occurrence
 * counts. Reasons are exact messages: no heuristic merging of distinct failures.
 * Rows must arrive in ascending simulation order, as committed by result-store.
 */
export function createDiagnosticFrequency(requested: number, processed: number) {
  if (!Number.isSafeInteger(requested) || requested < 1 || !Number.isSafeInteger(processed) || processed < 0 || processed > requested) throw new Error('Invalid diagnostic denominator');
  type Count = { occurrences: number; simulations: number; contextOccurrences: number; lastSimulation: number };
  const empty = (): Count => ({ occurrences: 0, simulations: 0, contextOccurrences: 0, lastSimulation: 0 });
  const buckets = { error: new Map<string, Count & { code: string; reason: string }>(),
    warning: new Map<string, Count & { code: string; reason: string }>(),
    info: new Map<string, Count & { code: string; reason: string }>() };
  const totals = { error: empty(), warning: empty(), info: empty() };
  const codes = { error: new Map<string, Count>(), warning: new Map<string, Count>(), info: new Map<string, Count>() };
  function count(target: Count, occurrences: number, simulation: number | null) {
    target.occurrences += occurrences;
    if (simulation === null) target.contextOccurrences += occurrences;
    else {
      if (simulation < target.lastSimulation) throw new Error('Diagnostic ledger is not in simulation order');
      if (simulation !== target.lastSimulation) target.simulations++;
      target.lastSimulation = simulation;
    }
  }
  return {
    add(row: ResultTable['rows'][number]) {
      const severity = row.Severity ?? 'warning';
      if (severity !== 'error' && severity !== 'warning' && severity !== 'info') throw new Error('Invalid diagnostic severity');
      const occurrences = Number(row.Occurrences), simulation = row['First simulation'] === null ? null : Number(row['First simulation']);
      if (!Number.isSafeInteger(occurrences) || occurrences < 1 || simulation !== null && (!Number.isSafeInteger(simulation) || simulation < 1 || simulation > processed)) throw new Error('Invalid diagnostic count/simulation');
      const code = String(row.Code), reason = String(row.Message), key = JSON.stringify([code, reason]);
      let entry = buckets[severity].get(key);
      if (!entry) { entry = { code, reason, ...empty() }; buckets[severity].set(key, entry); }
      count(entry, occurrences, simulation); count(totals[severity], occurrences, simulation);
      let codeCount = codes[severity].get(code);
      if (!codeCount) { codeCount = empty(); codes[severity].set(code, codeCount); }
      count(codeCount, occurrences, simulation);
    },
    finish() {
      const summarize = (value: Count) => ({ occurrences: value.occurrences, simulations: value.simulations, contextOccurrences: value.contextOccurrences,
        frequencyPerRequested: value.simulations / requested,
        frequencyPerProcessed: processed ? value.simulations / processed : null });
      const group = (severity: keyof typeof buckets) => ({ ...summarize(totals[severity]),
        codes: [...codes[severity]].map(([code, counts]) => ({ code, ...summarize(counts) })).sort((a, b) => a.code.localeCompare(b.code)),
        reasons: [...buckets[severity].values()].map(entry => ({ code: entry.code, reason: entry.reason, ...summarize(entry) }))
          .sort((a, b) => a.code.localeCompare(b.code) || a.reason.localeCompare(b.reason)) });
      return { requested, processed, errors: group('error'), warnings: group('warning'), information: group('info') };
    }
  };
}
