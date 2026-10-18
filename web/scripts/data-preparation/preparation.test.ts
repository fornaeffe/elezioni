import { afterEach, describe, expect, it } from 'vitest';
import { existsSync } from 'node:fs';
import { link, mkdir, mkdtemp, readFile, rm, writeFile } from 'node:fs/promises';
import { createServer } from 'node:http';
import { tmpdir } from 'node:os';
import { basename, join, relative, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import { zipSync, strToU8 } from 'fflate';
import { acquireSource, sha256, sourcePath, verifySource, writeVersion, type Source } from './sources';
import { archiveMember, bolzanoAnnex4, dbf, decode, delimited, electoralSpreadsheet } from './formats';
import { prepareTerritoryCatalog, readManifest } from './normalize';
import { validateTerritoryCatalog } from '../../src/lib/politics/reform-2026/geography';
import { buildProvisionalSeatTables, validateSeatTableBundle } from '../../src/lib/politics/reform-2026/endowments';
import { createSeededDrawContext } from '../../src/lib/politics/reform-2026/ties';

const root = fileURLToPath(new URL('../../../', import.meta.url));
const testDirectories: string[] = [];
const temporary = async () => {
  const directory = await mkdtemp(join(tmpdir(), 'elezioni-data-test-'));
  testDirectories.push(directory); return directory;
};
afterEach(async () => {
  for (const directory of testDirectories.splice(0)) {
    if (!basename(directory).startsWith('elezioni-data-test-') || relative(tmpdir(), directory).startsWith('..')) throw new Error('Unexpected cleanup path');
    await rm(directory, { recursive: true, force: true });
  }
});
const entry = (bytes: Buffer, path = 'fixture.bin'): Source => ({ id: 'fixture', path, url: 'https://example.invalid/fixture',
  sha256: sha256(bytes), bytes: bytes.length, headers: {} });

describe('reproducible source acquisition', () => {
  it('verifies bytes and refuses cache changes', async () => {
    const directory = await temporary(), bytes = Buffer.from('pinned source'), source = entry(bytes);
    const imported = join(directory, 'manual.bin'); await writeFile(imported, bytes);
    await acquireSource(join(directory, 'cache'), source, imported);
    expect(await readFile(join(directory, 'cache/fixture.bin'))).toEqual(bytes);
    await writeFile(join(directory, 'cache/fixture.bin'), 'corrupt');
    await expect(acquireSource(join(directory, 'cache'), source)).rejects.toThrow('differs from its pinned version');
    expect(() => verifySource(Buffer.from('different'), source)).toThrow();
  });
  it('downloads a missing source and reuses it offline', async () => {
    const bytes = Buffer.from('reproducible HTTP response'), directory = await temporary();
    let requests = 0;
    const server = createServer((_request, response) => { requests++; response.end(bytes); });
    await new Promise<void>(resolve => server.listen(0, '127.0.0.1', resolve));
    const address = server.address();
    if (!address || typeof address === 'string') throw new Error('Missing HTTP address');
    const source = { ...entry(bytes), url: `http://127.0.0.1:${address.port}/source` };
    try { await acquireSource(directory, source); }
    finally { await new Promise<void>((resolve, reject) => server.close(error => error ? reject(error) : resolve())); }
    await acquireSource(directory, source);
    expect(requests).toBe(1);
  });
  it('rejects paths outside the cache and preserves immutable outputs', async () => {
    const directory = await temporary();
    expect(() => sourcePath(directory, entry(Buffer.from('x'), '../outside'))).toThrow('Unsafe');
    expect(() => sourcePath(directory, entry(Buffer.from('x'), '.'))).toThrow('Unsafe');
    const path = join(directory, 'v1.json');
    await writeVersion(path, { seats: 1 });
    await writeVersion(path, { seats: 1 });
    await expect(writeVersion(path, { seats: 2 })).rejects.toThrow('immutable');
    expect(JSON.parse(await readFile(path, 'utf8'))).toEqual({ seats: 1 });
  });
});

function workbook(rows: readonly (readonly (string | number | null)[])[]): Buffer {
  const escape = (s: string) => s.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
  const xml = rows.map((row, ri) => `<row r="${ri + 1}">${row.map((value, ci) => value === null ? '' :
    `<c r="${String.fromCharCode(65 + ci)}${ri + 1}" t="inlineStr"><is><t>${escape(String(value))}</t></is></c>`).join('')}</row>`).join('');
  return Buffer.from(zipSync({
    '[Content_Types].xml': strToU8('<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types"><Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/><Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/></Types>'),
    'xl/workbook.xml': strToU8('<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"><sheets><sheet name="Geography" sheetId="1" r:id="rId1"/></sheets></workbook>'),
    'xl/_rels/workbook.xml.rels': strToU8('<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships"><Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/></Relationships>'),
    'xl/worksheets/sheet1.xml': strToU8(`<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><sheetData>${xml}</sheetData></worksheet>`)
  }));
}

describe('official source formats', () => {
  it('extracts merged-cell continuations and skips capoluogo summaries', async () => {
    const bytes = workbook([
      ['Regione: Test'], ['Circoscrizione: Test'], ['Codice della circoscrizione elettorale'],
      [1, 'Test', 'Test - U01', 'Summary', null, null],
      [null, null, null, null, 'First municipality', 1001],
      [null, null, null, null, 'Second municipality', 1002],
      [null, null, 'Test - U02', 'Another summary'],
      [null, null, null, null, 'Third municipality', 1003]
    ]);
    expect(await electoralSpreadsheet(bytes, 'uni')).toEqual([
      { territoryCode: '1', territoryName: 'Test', collegeName: 'Test - U01', member: 'First municipality', municipalityCode: '001001' },
      { territoryCode: '1', territoryName: 'Test', collegeName: 'Test - U01', member: 'Second municipality', municipalityCode: '001002' },
      { territoryCode: '1', territoryName: 'Test', collegeName: 'Test - U02', member: 'Third municipality', municipalityCode: '001003' }
    ]);
  });
  it('does not carry a previous constituency over a new section header', async () => {
    const bytes = workbook([['Regione: A'], ['Codice della circoscrizione elettorale'], [1, 'A', 'A - P01'],
      [null, null, null, 'A - U01'], ['Regione: B'], ['Codice della circoscrizione elettorale'],
      [null, null, null, 'B - U01']]);
    await expect(electoralSpreadsheet(bytes, 'pluri')).rejects.toThrow('continuation');
  });
  it('handles legacy encodings, quoted fields, archive selection and truncated DBFs', () => {
    expect(decode(Uint8Array.of(0xe8)).text).toBe('è');
    expect(delimited(Buffer.from('id,name\r\n001,"A, B"\r\n'))).toEqual([{ id: '001', name: 'A, B' }]);
    expect(archiveMember(zipSync({ 'a.txt': strToU8('a'), 'b.txt': strToU8('b') }), 'a.txt')).toEqual(strToU8('a'));
    expect(() => archiveMember(zipSync({}), 'missing')).toThrow('Missing archive');
    expect(() => dbf(Uint8Array.of(3))).toThrow();
    const bytes = new Uint8Array(70), view = new DataView(bytes.buffer);
    bytes[0] = 3; view.setUint32(4, 1, true); view.setUint16(8, 65, true); view.setUint16(10, 5, true);
    bytes.set(strToU8('NAME'), 32); bytes[43] = 67; bytes[48] = 4; bytes[64] = 13; bytes[65] = 32;
    bytes.set([0x43, 0x61, 0x66, 0xe8], 66);
    expect(dbf(bytes)).toEqual([{ NAME: 'Cafè' }]);
  });
  it('parses semicolon statutory lists and rejects missing annex sections', () => {
    const html = '<pre>4. - COLLEGIO DI BOLZANO\nComprende i seguenti comuni della provincia di Bolzano:\nBolzano; Laives.\n5. - COLLEGIO DI MERANO\nComprende i seguenti comuni della provincia di Bolzano:\nMerano.\n6. - COLLEGIO DI BRESSANONE\nComprende i seguenti comuni della provincia di Bolzano:\nBressanone».</pre>';
    expect(bolzanoAnnex4(html)).toEqual([{ college: 'U04', name: 'Bolzano' }, { college: 'U04', name: 'Laives' }, { college: 'U05', name: 'Merano' }, { college: 'U06', name: 'Bressanone' }]);
    expect(() => bolzanoAnnex4('<pre>missing</pre>')).toThrow('Missing Bolzano');
  });
});

const sourceDirectory = resolve(root, 'data/sources');
const hasSources = ['electoral-geography-2022.zip', 'legal-population-2021.csv', 'municipalities-2021.json',
  'submunicipal-geography-2021.zip', 'camera-a1.xlsx', 'camera-a2.xlsx', 'senato-b1.xlsx', 'senato-b2.xlsx',
  'bolzano-annex-4.html', 'bolzano-annex-5.html'].every(file => existsSync(join(sourceDirectory, file)));
describe.skipIf(!hasSources)('real-source preparation (requires npm run data:download; no network during tests)', () => {
  it('covers every municipality, verifies both Bolzano annexes and rejects duplicated memberships', async () => {
    const manifest = await readManifest(root);
    const { catalog } = await prepareTerritoryCatalog(root, manifest);
    expect(validateTerritoryCatalog(catalog)).toEqual([]);
    expect(catalog.municipalities).toHaveLength(7904);
    for (const chamber of ['camera', 'senato']) {
      expect(catalog.memberships.filter(m => m.chamber === chamber && m.municipalityCode === '081025')).toHaveLength(1);
      const moved = catalog.memberships.find(m => m.chamber === chamber && m.municipalityCode === '099030')!;
      expect(catalog.territories.find(t => t.id === catalog.colleges.find(c => c.id === moved.collegeId)!.territoryId)?.regionId).toBe('08');
    }
    const u04 = catalog.memberships.filter(m => m.collegeId === 'senato:U:040004').map(m => m.municipalityCode).sort();
    expect(u04).toEqual(['021008', '021040']);
    expect(catalog.populationReconciliations[0].estimated).toBe(false);
    const duplicate = { ...catalog, memberships: [...catalog.memberships, catalog.memberships[0]] };
    expect(validateTerritoryCatalog(duplicate)).toContain('Duplicate/missing membership');
    const unknown = { ...catalog, colleges: catalog.colleges.map((c, i) => i ? c : { ...c, territoryId: 'unknown' }) };
    expect(validateTerritoryCatalog(unknown)).toContain('Unknown membership territory unknown');
  }, 20000);
  it('estimates populations if the optional fine-geography file is absent, using a different immutable version', async () => {
    const directory = await temporary(), manifest = await readManifest(root);
    await mkdir(directory, { recursive: true });
    for (const s of manifest.sources.filter(s => !['submunicipal-geography-2021', 'submunicipal-population-2021', 'municipality-changes-2020-2021'].includes(s.id))) await link(join(sourceDirectory, s.path), join(directory, s.path));
    const { catalog } = await prepareTerritoryCatalog(root, manifest, directory);
    expect(catalog.population).toBe(59030133);
    expect(catalog.populationReconciliations[0].estimated).toBe(true);
    expect(catalog.notices.some(n => n.method === 'population-estimate' && n.message.includes('stimata'))).toBe(true);
    expect(validateTerritoryCatalog(catalog)).toEqual([]);
    const { municipalities: _municipalities, memberships: _memberships, ...summary } = catalog;
    const tables = buildProvisionalSeatTables(summary, 'estimated-population-test', 'fallback-test', createSeededDrawContext('fallback-test', 'N03-test'));
    expect(validateSeatTableBundle(tables)).toEqual([]);
    expect(tables.chambers[0].seatTable.populationNotes?.some(n => n.includes('stimata'))).toBe(true);
    const { catalog: complete } = await prepareTerritoryCatalog(root, manifest);
    expect(catalog.version).not.toBe(complete.version);
  }, 20000);
});
