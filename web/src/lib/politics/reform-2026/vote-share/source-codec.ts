import { gzipSync, gunzipSync } from 'fflate';
import { FAST_SHARE_MODEL, day } from './types';
import type { ColumnName, CompactSource, OriginalRow, OriginalSource, SourceChunk } from './source-types';

export const SOURCE_METHOD = 'fast-share-original-columns-v1';
export const ROW_FLAGS = { invalidDenominator: 1, unmatched: 2, missingVoters: 4, unlocated: 8 } as const;
export const MAX_SOURCE_BYTES = 256 * 1024 * 1024;
export interface ByteAsset { path: string; compressedBytes: number; decodedBytes: number; sha256: string; decodedSha256: string }
/** Fetch may transparently decode Content-Encoding; disk/cache readers must use raw bytes. */
export type AssetBytes = Uint8Array | { transport: 'http'; body: Uint8Array };
export function canonicalJson(value: unknown): string {
  if (value === null || typeof value === 'string' || typeof value === 'boolean') return JSON.stringify(value);
  if (typeof value === 'number' && Number.isFinite(value)) return JSON.stringify(value);
  if (Array.isArray(value)) return '[' + value.map(canonicalJson).join(',') + ']';
  if (value && typeof value === 'object') return '{' + Object.keys(value).sort().map(k => JSON.stringify(k) + ':' + canonicalJson((value as Record<string, unknown>)[k])).join(',') + '}';
  throw new Error('Nonfinite/unsupported canonical source value');
}
export async function byteHash(bytes: Uint8Array): Promise<string> {
  const owned = new Uint8Array(bytes.length); owned.set(bytes);
  return [...new Uint8Array(await crypto.subtle.digest('SHA-256', owned))].map(b => b.toString(16).padStart(2, '0')).join('');
}
export const sourceKey = async (kind: string, value: unknown) => kind + '-' + await byteHash(new TextEncoder().encode(kind + '\n' + canonicalJson(value)));
export async function compressAsset(stem: string, decoded: Uint8Array): Promise<{ descriptor: ByteAsset; compressed: Uint8Array }> {
  const compressed = gzipSync(decoded, { level: 9, mtime: 0 });
  const sha256 = await byteHash(compressed), decodedSha256 = await byteHash(decoded);
  return { compressed, descriptor: { path: stem + '-' + sha256 + '.gz', compressedBytes: compressed.length, decodedBytes: decoded.length, sha256, decodedSha256 } };
}
export async function decodeAsset(asset: ByteAsset, input: AssetBytes): Promise<Uint8Array> {
  if (!/^[\w.-]+$/.test(asset.path) || !Number.isSafeInteger(asset.decodedBytes) || asset.decodedBytes < 1 || asset.decodedBytes > MAX_SOURCE_BYTES ||
    !Number.isSafeInteger(asset.compressedBytes) || asset.compressedBytes < 18 || !/^[a-f0-9]{64}$/.test(asset.sha256) ||
    !/^[a-f0-9]{64}$/.test(asset.decodedSha256)) throw new Error('SOURCE_ASSET_INTEGRITY');
  const compressed = input instanceof Uint8Array ? input : input.body, hash = await byteHash(compressed);
  if (compressed.length !== asset.compressedBytes || hash !== asset.sha256) {
    if (!(input instanceof Uint8Array) && input.transport === 'http' && compressed.length === asset.decodedBytes && hash === asset.decodedSha256) return compressed;
    throw new Error('SOURCE_ASSET_INTEGRITY');
  }
  const trailer = new DataView(compressed.buffer, compressed.byteOffset, compressed.length).getUint32(compressed.length - 4, true);
  if (trailer !== asset.decodedBytes) throw new Error('SOURCE_DECODE_SIZE');
  const decoded = gunzipSync(compressed, { out: new Uint8Array(asset.decodedBytes) });
  if (decoded.length !== asset.decodedBytes || await byteHash(decoded) !== asset.decodedSha256) throw new Error('SOURCE_DECODE_INTEGRITY');
  return decoded;
}
function u32(n: number) { if (!Number.isSafeInteger(n) || n < 0 || n > 0xffffffff) throw new Error('SOURCE_UINT32_RANGE'); return n; }
export function rowFlags(row: OriginalRow, match: string) {
  const valid = row.choices.reduce((n, c) => n + c.count, 0);
  return (row.electors <= 0 || valid > row.electors || row.voters !== null && (row.voters > row.electors || valid > row.voters) ? 1 : 0) |
    (match === 'unmatched' ? 2 : 0) | (row.voters === null ? 4 : 0) | (match === 'unlocated' ? 8 : 0);
}
export async function encodeSource(input: OriginalSource) {
  const units = [...input.units].sort((a, b) => a.id.localeCompare(b.id)), choices = [...input.choices].sort((a, b) => a.id.localeCompare(b.id));
  const unitIndex = new Map(units.map((u, i) => [u.id, i])), choiceIndex = new Map(choices.map((c, i) => [c.id, i]));
  if (units.length !== unitIndex.size || choices.length !== choiceIndex.size) throw new Error('Duplicate source dictionaries');
  const assets = new Map<string, Uint8Array>(), events: CompactSource['payload']['events'] = [];
  for (const event of [...input.events].sort((a, b) => a.id.localeCompare(b.id))) {
    day(event.date); day(event.availableDate);
    const rows = [...event.rows].sort((a, b) => a.unit.localeCompare(b.unit));
    if (new Set(rows.map(r => r.unit)).size !== rows.length) throw new Error('Duplicate municipal observation');
    const values = rows.reduce((n, r) => n + r.choices.length, 0);
    const arrays: Record<ColumnName, number[]> = { unit: [], electors: [], voters: [], flags: [], offsets: [0], choice: [], count: [], direct: [] };
    for (const row of rows) {
      const unit = unitIndex.get(row.unit); if (unit === undefined) throw new Error('Unknown source unit');
      arrays.unit.push(unit); arrays.electors.push(u32(row.electors)); arrays.voters.push(row.voters === null ? 0 : u32(row.voters)); arrays.flags.push(rowFlags(row, units[unit].match));
      const counts = [...row.choices].sort((a, b) => a.id.localeCompare(b.id));
      if (!counts.length || new Set(counts.map(c => c.id)).size !== counts.length) throw new Error('Empty/duplicate original choice');
      for (const c of counts) {
        const index = choiceIndex.get(c.id);
        if (index === undefined || choices[index].eventId !== event.id || c.direct > c.count) throw new Error('Invalid original choice binding');
        arrays.choice.push(index); arrays.count.push(u32(c.count)); arrays.direct.push(u32(c.direct));
      }
      arrays.offsets.push(arrays.count.length);
    }
    const size = Object.values(arrays).reduce((n, a) => n + a.length * 4, 0);
    if (size > MAX_SOURCE_BYTES) throw new Error('SOURCE_WORKSPACE_LIMIT');
    const bytes = new Uint8Array(size), view = new DataView(bytes.buffer), columns = {} as SourceChunk['columns']; let offset = 0;
    for (const key of Object.keys(arrays) as ColumnName[]) {
      columns[key] = { dtype: 'uint32', offset, length: arrays[key].length };
      for (const n of arrays[key]) { view.setUint32(offset, u32(n), true); offset += 4; }
    }
    const encoded = await compressAsset('history-' + event.id + '.u32', bytes); assets.set(encoded.descriptor.path, encoded.compressed);
    const { rows: _rows, ...metadata } = event;
    events.push({ ...metadata, national: { electors: rows.reduce((n, r) => n + r.electors, 0), voters: rows.some(r => r.voters === null) ? null : rows.reduce((n, r) => n + r.voters!, 0),
      valid: arrays.count.reduce((n, x) => n + x, 0), direct: arrays.direct.reduce((n, x) => n + x, 0) }, chunk: { ...encoded.descriptor, byteOrder: 'little-endian', rows: rows.length, values, columns } });
  }
  const body = { schemaVersion: 1 as const, kind: 'fast-share-source' as const, modelId: FAST_SHARE_MODEL,
    sourceVersions: [...new Set(input.sourceVersions)].sort(), methodVersions: [...new Set([...input.methodVersions, SOURCE_METHOD])].sort(), assumptions: input.assumptions,
    payload: { range: { first: events.map(e => e.date).sort()[0], latestObservation: events.map(e => e.date).sort().at(-1)!, latestPollPublication: input.polls.map(p => p.availableDate).sort().at(-1) ?? events.map(e => e.date).sort().at(-1)! },
      missing: 'Absent rows mean missing history. Offered choices include explicit observed zeros. No absent count becomes zero.', flags: ROW_FLAGS,
      correspondencePolicy: input.correspondencePolicy, citations: [...input.citations].sort((a, b) => a.id.localeCompare(b.id)),
      units, choices, polls: [...input.polls].sort((a, b) => a.id.localeCompare(b.id)), geography: input.geography, events } };
  const envelope: CompactSource = { ...body, id: await sourceKey('fast-share-source-v1', body) };
  await validateSourceEnvelope(envelope);
  return { envelope, assets };
}
export async function validateSourceEnvelope(source: CompactSource) {
  if (source.schemaVersion !== 1 || source.kind !== 'fast-share-source' || source.modelId !== FAST_SHARE_MODEL || !source.methodVersions.includes(SOURCE_METHOD)) throw new Error('Unsupported compact source version');
  const { id, ...body } = source;
  if (id !== await sourceKey('fast-share-source-v1', body)) throw new Error('SOURCE_CONTENT_ID');
  const p = source.payload;
  for (const values of [p.units, p.choices, p.events, p.polls]) if (new Set(values.map(x => x.id)).size !== values.length) throw new Error('Duplicate source metadata ID');
  if (!p.events.length || !p.units.length || !p.choices.length) throw new Error('Empty compact history');
  const targetIds = new Set(p.geography.targets.map(t => t.code));
  if (targetIds.size !== p.geography.targets.length || p.units.some(u => u.targetCode !== null && !targetIds.has(u.targetCode))) throw new Error('Invalid source/target crosswalk');
  const colleges = new Map(p.geography.colleges.map(c => [c.id, c])), cells = new Set<string>(), mass = new Map<string, number>();
  if (colleges.size !== p.geography.colleges.length) throw new Error('Duplicate physical college');
  for (const c of p.geography.cells) {
    const college = colleges.get(c.collegeId), key = JSON.stringify([c.chamber, c.collegeId, c.unitId]);
    if (!college || college.chamber !== c.chamber || college.kind === 'taa-proportional' || !targetIds.has(c.municipality) || !c.unitId || !Number.isSafeInteger(c.population) || c.population < 0 || cells.has(key)) throw new Error('Invalid physical ballot cell');
    cells.add(key); const parent = c.chamber + ':' + c.municipality; mass.set(parent, (mass.get(parent) ?? 0) + c.population);
  }
  for (const t of p.geography.targets) if (!Number.isSafeInteger(t.population) || t.population <= 0 || ['camera', 'senato'].some(ch => mass.get(ch + ':' + t.code) !== t.population)) throw new Error('Incomplete physical geography/population conservation');
  for (const c of p.choices) if (!p.events.some(e => e.id === c.eventId) || !c.members.length || new Set(c.members).size !== c.members.length || !c.citations.length) throw new Error('Incomplete source membership');
  if (!p.correspondencePolicy.id || !p.correspondencePolicy.status || new Set(p.citations.map(c => c.id)).size !== p.citations.length || p.choices.some(c => c.citations.some(id => !p.citations.some(s => s.id === id)))) throw new Error('Incomplete membership provenance');
  for (const e of p.events) {
    if (!['camera', 'european', 'senato'].includes(e.context) || ![18, 25].includes(e.minimumAge) || day(e.availableDate) < day(e.date) ||
      (e.minimumAge === 25 && e.role !== 'age-audit')) throw new Error('Invalid source date/context/electorate');
    if (!/^[\w.-]+$/.test(e.chunk.path)) throw new Error('Unsafe source chunk path');
  }
}
/** Decodes only one selected event; callers can release each chunk after use. */
export async function decodeSourceRows(source: CompactSource, eventId: string, compressed: AssetBytes, observe?: (phase: string, liveNumericBytes: number) => void): Promise<OriginalRow[]> {
  const event = source.payload.events.find(e => e.id === eventId); if (!event) throw new Error('Unknown historical source');
  const chunk = event.chunk, bytes = await decodeAsset(chunk, compressed), view = new DataView(bytes.buffer, bytes.byteOffset, bytes.length);
  const body = compressed instanceof Uint8Array ? compressed : compressed.body, extraWireBytes = body === bytes ? 0 : body.byteLength;
  observe?.('inflated', bytes.byteLength + extraWireBytes);
  if (chunk.byteOrder !== 'little-endian' || !Number.isSafeInteger(chunk.rows) || chunk.rows <= 0 || !Number.isSafeInteger(chunk.values) || chunk.values < chunk.rows) throw new Error('Invalid source shape');
  const columns = new Map<ColumnName, Uint32Array>(); let end = 0;
  for (const name of ['unit','electors','voters','flags','offsets','choice','count','direct'] as ColumnName[]) {
    const c = chunk.columns[name], expected = name === 'offsets' ? chunk.rows + 1 : ['choice', 'count', 'direct'].includes(name) ? chunk.values : chunk.rows;
    if (c.dtype !== 'uint32' || c.offset !== end || c.length !== expected || c.offset + c.length * 4 > bytes.length) throw new Error('Invalid source column layout');
    columns.set(name, Uint32Array.from({ length: c.length }, (_, i) => view.getUint32(c.offset + i * 4, true))); end += c.length * 4;
  }
  if (end !== bytes.length) throw new Error('Trailing source bytes');
  observe?.('columns', 2 * bytes.byteLength + extraWireBytes);
  const get = (name: ColumnName) => columns.get(name)!;
  if (get('offsets')[0] !== 0 || get('offsets')[chunk.rows] !== chunk.values) throw new Error('Invalid source offsets');
  const rows: OriginalRow[] = []; let totalElectors = 0, valid = 0, direct = 0, voters = 0, missingVoters = false;
  for (let i = 0; i < chunk.rows; i++) {
    const u = source.payload.units[get('unit')[i]], flags = get('flags')[i], a = get('offsets')[i], b = get('offsets')[i + 1];
    if (!u || a >= b || b > chunk.values || flags > 15) throw new Error('Invalid source row indices');
    const row: OriginalRow = { unit: u.id, electors: get('electors')[i], voters: flags & 4 ? null : get('voters')[i], choices: [] };
    for (let j = a; j < b; j++) {
      const c = source.payload.choices[get('choice')[j]], count = get('count')[j], original = get('direct')[j];
      if (!c || c.eventId !== eventId || original > count) throw new Error('Invalid source choice index/count');
      row.choices.push({ id: c.id, count, direct: original }); valid += count; direct += original;
    }
    if (new Set(row.choices.map(c => c.id)).size !== row.choices.length || rowFlags(row, u.match) !== flags) throw new Error('Source row flags/choices differ');
    rows.push(row); totalElectors += row.electors; voters += row.voters ?? 0; missingVoters ||= row.voters === null;
  }
  if (new Set(rows.map(r => r.unit)).size !== rows.length || totalElectors !== event.national.electors || valid !== event.national.valid || direct !== event.national.direct ||
    (missingVoters ? null : voters) !== event.national.voters) throw new Error('SOURCE_COUNT_CONSERVATION');
  observe?.('rows', 2 * bytes.byteLength + extraWireBytes);
  return rows;
}
