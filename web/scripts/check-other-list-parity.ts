/** Run after scripts/check_other_lists.R <output.json>; no source data writes. */
import { readFileSync } from 'node:fs';
import { strict as assert } from 'node:assert';
import { runPoliticsScrutiny } from '../src/lib/politics/scrutiny';
import { runRegionalErScrutiny } from '../src/lib/regional-er/scrutiny';
import type { PoliticsGoldenFixture, PoliticsScrutinyOutput } from '../src/lib/politics/types';
import type { RegionalErScrutinyOutput } from '../src/lib/regional-er/types';

const path = process.argv[2];
if (!path) throw new Error('Supply the JSON output from scripts/check_other_lists.R');
const expected = JSON.parse(readFileSync(path, 'utf8'));
const fixture: PoliticsGoldenFixture = JSON.parse(readFileSync(new URL('../../test/fixtures/politiche/debug_scrutinio.json', import.meta.url), 'utf8'));
function compare(actual: PoliticsScrutinyOutput | RegionalErScrutinyOutput, expected: Record<string, Record<string, unknown>[]>) {
  const canonical = (rows: Record<string, unknown>[]) => rows.map(r => Object.fromEntries(Object.entries(r).sort())).sort((a, b) => JSON.stringify(a).localeCompare(JSON.stringify(b)));
  for (const [table, rows] of Object.entries(actual)) {
    const a = canonical(rows), b = canonical(expected[table]);
    assert.equal(a.length, b.length, table);
    for (let i = 0; i < a.length; i++) for (const key of Object.keys(b[i])) {
      if (typeof b[i][key] === 'number') assert.ok(Math.abs(Number(a[i][key]) - Number(b[i][key])) < 1e-8, `${table}/${i}/${key}`);
      else assert.equal(a[i][key], b[i][key], `${table}/${i}/${key}`);
    }
  }
}
for (const ramo of ['camera', 'senato'] as const) {
  const r = structuredClone(fixture.rami[ramo]);
  const input = r.simulations[0].input;
  const colleges = [...new Map(input.liste_uni.map(l => [String(l.COLLEGIOUNINOMINALE), l])).values()];
  input.liste_uni.push(...colleges.map(l => ({ ...l, LISTA: 'Altre liste', CANDIDATO: null, MINORANZA: false, CAND_MINORANZA: false, VOTI_LISTA: 100 })));
  compare(runPoliticsScrutiny(input, { ramo, liste_naz: [...r.liste_naz, { LISTA: 'Altre liste', COALIZIONE: null, MINORANZA: false, kind: 'other' }], totali_pluri: r.totali_pluri, totale_seggi: r.totale_seggi }), expected[ramo]);
}
for (const share of [1, 30, 80]) {
  const lists = ['A', 'B', 'Altre liste'].map((LISTA, i) => ({ LISTA, COALIZIONE: i === 2 ? null : LISTA, DATA: '', LOGIT_P: 0, SIGMA_GLOBAL: 0, PERCENTUALE: 0, ...(i === 2 ? { kind: 'other' as const } : {}) }));
  const counts = [(100-share)*60, (100-share)*40, share*100];
  const comuni_liste = lists.map((l, i) => ({ CODICE_COMUNE: '1', COMUNE: 'Test', CODICE_PROVINCIA: '1', PROVINCIA: 'Test', LISTA: l.LISTA, VOTI_LISTA_SIM: counts[i] }));
  const pop_legale = [{ CODICE_COMUNE: '1', COMUNE: 'Test', CODICE_PROVINCIA: '1', PROVINCIA: 'Test', CODICE_REGIONE: '08', REGIONE: 'Emilia-Romagna', POPOLAZIONE: 10000 }];
  compare(runRegionalErScrutiny({ comuni_liste }, { liste: lists, pop_legale }, { seed: 'other-regional' }), expected[`regional-${share}`]);
}
console.log('Other R/TypeScript parity passed: Camera, Senato, regional 1/30/80%.');
