import { beforeAll, expect, it } from 'vitest';
import { fileURLToPath } from 'node:url';
import { readFile } from 'node:fs/promises';
import { gunzipSync } from 'node:zlib';
import { resolve } from 'node:path';
import { existsSync } from 'node:fs';
import { municipalResolver } from './workspace-history';
import { readManifest } from './normalize';
import { readDefinitiveGeography, type DefinitiveGeography } from './definitive-geography';
import { prepareWorkspaceSnapshots } from './workspace-snapshots';
import { sha256 } from './sources';
import { getBundledReformSeatTables } from '../../src/lib/politics/reform-2026/bundled-data';

const root = fileURLToPath(new URL('../../../', import.meta.url));
it('requires an official merger for reverse reconstruction and refuses ambiguous or missing names', () => {
  const municipalities = [{ code: '001001', name: 'Alpha', regionId: '01', provinceId: '001', province: 'Torino', region: 'Piemonte', population: 60 },
    { code: '001002', name: 'Beta', regionId: '01', provinceId: '001', province: 'Torino', region: 'Piemonte', population: 40 }];
  const geo: DefinitiveGeography = { rows: [], municipalities };
  const after = municipalities.map(m => ({ PRO_COM_T: m.code, COMUNE: m.name, PRO_COM_T_DT_FI: '001999', COMUNE_DT_FI: 'Alpha' }));
  const historical = municipalResolver(geo, [], []), merged = municipalResolver(geo, [], after);
  expect(historical('Piemonte', 'Alpha', 2022).map(m => m.code)).toEqual(['001001']);
  expect(merged('Piemonte', 'Alpha', 2024).map(m => m.code)).toEqual(['001001', '001002']);
  expect(() => merged('Piemonte', 'Unknown', 2024)).toThrow('Unresolved');
  const ambiguous = municipalResolver(geo, after.map(r => ({ ...r, COMUNE: 'Duplicate', PRO_COM_T_DT_FI: r.PRO_COM_T })), []);
  expect(() => ambiguous('Piemonte', 'Duplicate', 2018)).toThrow('Ambiguous');
});

it('pins the frozen regression outputs independently of R and legacy caches', async () => {
  const manifest = JSON.parse(await readFile(resolve(root, 'test/fixtures/r-reference-manifest.json'), 'utf8'));
  for (const fixture of manifest.fixtures) {
    const bytes = await readFile(resolve(root, 'test/fixtures', fixture.path));
    expect(bytes.length).toBe(fixture.bytes);
    expect(sha256(bytes)).toBe(fixture.sha256);
    expect(sha256(gunzipSync(bytes))).toBe(fixture.uncompressedSha256);
  }
});

const present = existsSync(resolve(root, 'data/sources/regional-2020.zip'));
// The complete release command acquires all sources before running this suite.
import { describe } from 'vitest';
describe.skipIf(!present)('full production source preparation', () => {
  let prepared: Awaited<ReturnType<typeof prepareWorkspaceSnapshots>>, geo: DefinitiveGeography;
  beforeAll(async () => {
    geo = await readDefinitiveGeography(await readManifest(root), resolve(root, 'data/sources'));
    prepared = await prepareWorkspaceSnapshots(root, resolve(root, 'data/sources'));
  }, 30_000);
  it('conserves every source electorate and the complete legal municipal census', () => {
    expect(geo.municipalities).toHaveLength(7904);
    expect(geo.municipalities.reduce((n, m) => n + m.population, 0)).toBe(59030133);
    const historical = prepared.politics.data.comuni_liste_elezioni!;
    for (const audit of prepared.audit.history as { election: string; electors: number }[]) {
      const votes = audit.election.startsWith('regionali') ? prepared.regional.data.comuni_liste_elezioni! : historical;
      expect(votes.filter(r => r.ELEZIONE === audit.election).reduce((n, r) => n + r.VOTI, 0)).toBe(audit.electors);
    }
    const latest = historical.filter(r => r.ELEZIONE === 'europee 2024');
    const electorate = new Map<string, number>();
    for (const r of latest) electorate.set(String(r.CODICE_COMUNE), (electorate.get(String(r.CODICE_COMUNE)) ?? 0) + r.VOTI);
    const cells = new Map<string, number>();
    for (const r of prepared.politics.data.base_dati) cells.set(String(r.CODICE_COMUNE), (cells.get(String(r.CODICE_COMUNE)) ?? 0) + r.ELETTORI);
    expect(cells).toEqual(electorate);
    expect(prepared.regional.data.pop_legale.reduce((n, r) => n + r.POPOLAZIONE, 0)).toBe(geo.municipalities.filter(m => m.regionId === '08').reduce((n, m) => n + m.population, 0));
  });
  it('retains all previous-law uninominal colleges and isolates the reform Bolzano overlay', () => {
    expect(prepared.politics.data.camera.uni).toHaveLength(147);
    expect(prepared.politics.data.senato.uni).toHaveLength(74);
    for (const [ch, seats] of [['camera', 392], ['senato', 196]] as const)
      expect(prepared.politics.data[ch].uni.length + prepared.politics.data[ch].pluri.reduce((n, r) => n + r.SEGGI_PLURI, 0)).toBe(seats);
    const original = geo.rows.find(r => r.PRO_COM === '21076')!; // Salorno
    expect(original.SU20_COD).toBe('40004');
    expect(prepared.politics.data.base_dati.filter(r => r.CODICE_COMUNE === '021076').map(r => r.SU20_COD)).toEqual([40004]);
    // Reform moves Salorno and the other statutory annex municipalities to U05.
    const reform = getBundledReformSeatTables().geography;
    const u04 = reform.colleges.find(c => c.id === 'senato:U:040004')!;
    const originalU04 = new Set(geo.rows.filter(r => r.SU20_COD === '40004').map(r => r.PRO_COM.padStart(6, '0')));
    expect(u04.population).toBeLessThan(geo.municipalities.filter(m => originalU04.has(m.code)).reduce((n, m) => n + m.population, 0));
  });
  it('preserves absent regional history and records estimates and source denominator anomalies', () => {
    expect(prepared.regional.data.comuni_liste_elezioni!.some(r => r.ELEZIONE === 'regionali 2020' && ['099030', '099031'].includes(String(r.CODICE_COMUNE)))).toBe(false);
    const history = prepared.audit.history as { election: string; denominatorAnomalies: unknown[]; estimatedMappings: unknown[] }[];
    expect(history.find(a => a.election === 'camera 2018')!.denominatorAnomalies.length).toBeGreaterThan(0);
    expect(history.find(a => a.election === 'europee 2024')!.estimatedMappings.length).toBeGreaterThan(0);
    expect(prepared.politics.metadata.preparation?.sourceHashes.some(s => s.id === 'municipality-history-2021-20220925')).toBe(true);
    expect(prepared.politics.metadata.preparation?.sourceHashes.some(s => s.id === 'municipality-history-2021-20240609')).toBe(true);
  });
});
