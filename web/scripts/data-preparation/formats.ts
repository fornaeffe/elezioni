import { parse } from 'csv-parse/sync';
import { unzipSync } from 'fflate';
import { readSheet } from 'read-excel-file/node';

export function decode(bytes: Uint8Array): { text: string; encoding: string } {
  try { return { text: new TextDecoder('utf-8', { fatal: true }).decode(bytes), encoding: 'utf-8' }; }
  catch { return { text: new TextDecoder('windows-1252', { fatal: true }).decode(bytes), encoding: 'windows-1252' }; }
}

export function delimited(bytes: Uint8Array, delimiter = ','): Record<string, string>[] {
  return parse(decode(bytes).text, { delimiter, columns: true, bom: true, skip_empty_lines: true, trim: true });
}

/** Extract only the required member, never archive paths onto the filesystem. */
export function archiveMember(bytes: Uint8Array, name: string): Uint8Array {
  const members = unzipSync(bytes, { filter: entry => entry.name === name });
  if (!members[name]) throw new Error(`Missing archive member ${name}`);
  return members[name];
}

/** dBASE III attributes of the official geographic units; polygon processing is unnecessary. */
export function dbf(bytes: Uint8Array): Record<string, string>[] {
  const view = new DataView(bytes.buffer, bytes.byteOffset, bytes.byteLength);
  if (bytes.length < 33 || bytes[0] !== 3) throw new Error('Expected dBASE III');
  const count = view.getUint32(4, true), header = view.getUint16(8, true), length = view.getUint16(10, true);
  if (header < 33 || header > bytes.length || bytes[header - 1] !== 13 || header + count * length > bytes.length) throw new Error('Truncated DBF');
  const fields: { name: string; length: number }[] = [];
  for (let offset = 32; offset < header - 1; offset += 32) {
    const name = new TextDecoder().decode(bytes.slice(offset, offset + 11)).split('\0')[0];
    if (!name || fields.some(f => f.name === name)) throw new Error('Invalid DBF field');
    fields.push({ name, length: bytes[offset + 16] });
  }
  if (fields.reduce((sum, f) => sum + f.length, 1) !== length) throw new Error('Invalid DBF record size');
  // The 2022 archive declares UTF-8 but contains Windows-1252 attributes.
  const encoding = decode(bytes.slice(header, header + count * length)).encoding;
  const decoder = new TextDecoder(encoding);
  const rows: Record<string, string>[] = [];
  for (let row = 0; row < count; row++) {
    let offset = header + row * length;
    if (bytes[offset] === 42) continue;
    if (bytes[offset++] !== 32) throw new Error('Invalid DBF deletion marker');
    const record: Record<string, string> = {};
    for (const field of fields) {
      record[field.name] = decoder.decode(bytes.slice(offset, offset + field.length)).trim();
      offset += field.length;
    }
    rows.push(record);
  }
  return rows;
}

export interface SpreadsheetMember { territoryCode: string; territoryName: string; collegeName: string; member: string; municipalityCode: string | null; }

/** Definitive 2020 tables: explicit continuation state, never blanket filling all empty cells. */
export async function electoralSpreadsheet(bytes: Buffer, kind: 'uni' | 'pluri'): Promise<SpreadsheetMember[]> {
  const rows = await readSheet(bytes);
  const result: SpreadsheetMember[] = [];
  let territoryCode = '', territoryName = '', collegeName = '', sawHeader = false;
  for (const row of rows) {
    const values = row.map(value => value === null ? '' : String(value).trim());
    if (/^Regione:|^Circoscrizione:/.test(values[0] ?? '')) {
      territoryCode = ''; territoryName = ''; collegeName = ''; sawHeader = false;
      continue;
    }
    if (values[0]?.startsWith('Codice della circoscrizione')) { sawHeader = true; continue; }
    if (!sawHeader) continue;
    if (/^\d+$/.test(values[0] ?? '')) { territoryCode = values[0]; territoryName = values[1]; }
    if (values[2]) collegeName = values[2];
    const municipality = values[5];
    const member = kind === 'uni' ? values[4] : values[3];
    // Summary/capoluogo rows have no municipality code, and are not memberships.
    if (!member || (kind === 'uni' && !/^\d+$/.test(municipality ?? ''))) continue;
    if (!territoryCode || !territoryName || !collegeName || !collegeName.includes(kind === 'uni' ? ' - U' : ' - P')) throw new Error('Invalid spreadsheet continuation');
    result.push({ territoryCode, territoryName, collegeName, member,
      municipalityCode: kind === 'uni' ? municipality.padStart(6, '0') : null });
  }
  if (!result.length) throw new Error('No geography extracted from spreadsheet');
  return result;
}

export const normalizeName = (value: string): string => value.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase().replace(/[^a-z0-9]/g, '');

export function annexText(html: string): string {
  const pre = html.match(/<pre\b[^>]*>([\s\S]*?)<\/pre>/i)?.[1];
  if (!pre) throw new Error('Missing statutory annex text');
  return pre.replace(/<[^>]*>/g, '').replace(/&#(\d+);/g, (_, code) => String.fromCodePoint(Number(code)))
    .replace(/&apos;/g, "'").replace(/&amp;/g, '&').replace(/&nbsp;/g, ' ');
}

/** Annex 4 has semicolon-separated municipality lists, simpler than the paginated bilingual table. */
export function bolzanoAnnex4(html: string): { college: string; name: string }[] {
  const text = annexText(html);
  const entries: { college: string; name: string }[] = [];
  for (const number of [4, 5, 6]) {
    const section = text.split(new RegExp(`\\b${number}\\.\\s*-\\s*COLLEGIO`))[1]?.split(/\b[456]\.\s*-\s*COLLEGIO/)[0];
    const list = section?.split(/Comprende i seguenti comuni della provincia di Bolzano:/i)[1];
    if (!list) throw new Error(`Missing Bolzano section ${number}`);
    for (const name of list.replace(/[».\s]+$/, '').split(';')) entries.push({ college: `U0${number}`, name: name.replace(/\s+/g, ' ').trim() });
  }
  return entries;
}
