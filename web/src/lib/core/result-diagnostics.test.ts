import { describe, expect, it } from 'vitest';
import { createDiagnosticFrequency } from './result-diagnostics';

describe('per-reason diagnostic frequency', () => {
  it('separates severities, reasons, repeated occurrences and shared context with the requested denominator', () => {
    const ledger = createDiagnosticFrequency(10, 3);
    const add = (Severity: string, Message: string, simulation: number | null, Occurrences = 1) => ledger.add({ Code: 'same-code', Severity, Message, 'First simulation': simulation, Occurrences });
    add('warning', 'shared assumption', null);
    add('error', 'reason A', 1, 3); add('error', 'reason B', 1);
    add('warning', 'reason A', 1, 2); add('info', 'metadata', 1);
    add('error', 'reason A', 3);
    const result = ledger.finish();
    expect(result.errors.occurrences).toBe(5);
    expect(result.errors.simulations).toBe(2);
    expect(result.errors.frequencyPerRequested).toBe(0.2);
    expect(result.errors.frequencyPerProcessed).toBe(2 / 3);
    expect(result.errors.reasons.map(r => [r.reason, r.occurrences, r.simulations])).toEqual([['reason A', 4, 2], ['reason B', 1, 1]]);
    expect(result.warnings.contextOccurrences).toBe(1);
    expect(result.warnings.simulations).toBe(1);
    expect(result.information.occurrences).toBe(1);
  });
  it('rejects unordered/invalid rows and handles a session without processed runs', () => {
    const ledger = createDiagnosticFrequency(10, 0);
    expect(ledger.finish().errors.frequencyPerProcessed).toBeNull();
    expect(() => ledger.add({ Severity: 'error', Occurrences: 0 })).toThrow();
    const ordered = createDiagnosticFrequency(10, 10);
    const row = { Severity: 'warning', Code: 'W', Message: 'reason', Occurrences: 1 };
    ordered.add({ ...row, 'First simulation': 2 });
    expect(() => ordered.add({ ...row, 'First simulation': 1 })).toThrow('order');
  });
});
