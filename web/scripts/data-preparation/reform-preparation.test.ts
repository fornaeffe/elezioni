import { describe, expect, it } from 'vitest';
import { zipSync, strToU8 } from 'fflate';
import { ministryWorkbookRows } from './ministry-workbook';
import { reconstructHistoricalAttribution } from './reform-history';
import { fitPreferenceModel, type PreferenceObservation } from './reform-preferences';

describe('historical attribution and source formats', () => {
  const rows = [
    { COMUNE: 'X', DESCRLISTA: 'A', VOTILISTA: '30', VOTICANDIDATO: '50' },
    { COMUNE: 'X', DESCRLISTA: 'B', VOTILISTA: '10', VOTICANDIDATO: '50' },
    { COMUNE: 'Y', DESCRLISTA: 'A', VOTILISTA: '10', VOTICANDIDATO: '80' },
    { COMUNE: 'Y', DESCRLISTA: 'B', VOTILISTA: '50', VOTICANDIDATO: '80' }
  ].map(r => ({ ...r, 'CIRC-REG': 'R', COLLUNINOM: 'U1', COGNOME: 'Person', NOME: 'One', DATANASCITA: '01/01/1970' }));
  it('uses the whole old uninominal college and conserves both list and municipal margins', () => {
    const result = reconstructHistoricalAttribution(rows, 'camera');
    const totals = Object.fromEntries(['A', 'B'].map(l => [l, result.output.filter(r => r.row.DESCRLISTA === l).reduce((s, r) => s + r.votes, 0)]));
    expect(totals).toEqual({ A: 52, B: 78 });
    for (const [name, expected] of [['X', 50], ['Y', 80]] as const) expect(result.output.filter(r => r.row.COMUNE === name).reduce((s, r) => s + r.votes, 0)).toBe(expected);
    // Municipal attribution would add 8+3 to A, not the correct 12 nationally.
    expect(result.traces[0]).toMatchObject({ direct: 100, candidateOnly: 30, attributed: 130 });
    expect(reconstructHistoricalAttribution([...rows].reverse(), 'camera')).toEqual(result);
  });
  it('conserves small integer row margins when independent list rounding would disagree', () => {
    const small = rows.map(r => ({ ...r, VOTILISTA: '1', VOTICANDIDATO: r.COMUNE === 'X' ? '3' : '4' }));
    const result = reconstructHistoricalAttribution(small, 'camera');
    expect(result.output.filter(r => r.row.COMUNE === 'X').reduce((s, r) => s + r.votes, 0)).toBe(3);
    expect(result.output.filter(r => r.row.COMUNE === 'Y').reduce((s, r) => s + r.votes, 0)).toBe(4);
  });
  it('rejects duplicate/incomplete rows and impossible candidate totals', () => {
    expect(() => reconstructHistoricalAttribution([...rows, rows[0]], 'camera')).toThrow();
    expect(() => reconstructHistoricalAttribution(rows.slice(1), 'camera')).toThrow();
    expect(() => reconstructHistoricalAttribution(rows.map(r => ({ ...r, VOTICANDIDATO: '1' })), 'camera')).toThrow();
  });
  it('reads sparse typed cells and shared strings without accepting formulas', () => {
    const make = (row: string) => zipSync({ 'xl/sharedStrings.xml': strToU8('<sst><si><t>A &amp; B</t></si></sst>'),
      'xl/worksheets/sheet1.xml': strToU8(`<worksheet><sheetData><row r="1">${row}</row></sheetData></worksheet>`) });
    expect([...ministryWorkbookRows(make('<c r="A1" t="s"><v>0</v></c><c r="C1"><v>42</v></c>'))][0]).toEqual(['A & B', , '42']);
    expect(() => [...ministryWorkbookRows(make('<c r="A1"><f>1+1</f><v>2</v></c>'))]).toThrow('formula');
    expect(() => [...ministryWorkbookRows(make('<c r="A1" t="e"><v>#N/A</v></c>'))]).toThrow('Unsupported');
  });
});

describe('preference preparation and independent cluster validation', () => {
  it('pools sparse scopes, fits separate concentration/local scales and excludes invalid budgets explicitly', () => {
    const observations: PreferenceObservation[] = [];
    for (const election of ['er-2024', 'european-2024']) for (const region of ['R1', 'R2']) for (const municipality of ['M1', 'M2']) {
      observations.push({ election, region, province: region, municipality, list: 'L', ballots: 100,
        candidates: Array.from({ length: 4 }, (_, i) => ({ id: `${election}/${region}/${i}`, gender: i % 2 ? 'female' : 'male', marks: (4 - i) * 5 + Number(municipality === 'M2') })) });
    }
    observations.push({ ...observations[0], municipality: 'invalid', ballots: 1 });
    const fitted = fitPreferenceModel(observations);
    expect(fitted.audit.exclusions).toHaveLength(1);
    expect(fitted.audit.passed).toBe(true);
    expect(fitted.baseline.intensity).toBeCloseTo(0.52, 12);
    expect(fitted.baseline.strengthSigma).toBeGreaterThan(0);
    expect(fitted.baseline.localSigma).toBeGreaterThanOrEqual(0);
    expect(fitted.pools).toHaveLength(4);
    for (const f of fitted.audit.validation.folds) { expect(f.trainObservations).toBe(6); expect(f.heldObservations).toBe(2); expect(f.heldPeople).toBe(4); }
  });
});
