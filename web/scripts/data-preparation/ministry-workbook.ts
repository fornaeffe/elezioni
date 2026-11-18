import { unzipSync } from 'fflate';

const xmlText = (s: string) => s.replace(/&#x([0-9a-f]+);/gi, (_, n) => String.fromCodePoint(parseInt(n, 16)))
  .replace(/&#(\d+);/g, (_, n) => String.fromCodePoint(Number(n))).replace(/&lt;/g, '<').replace(/&gt;/g, '>')
  .replace(/&quot;/g, '"').replace(/&apos;/g, "'").replace(/&amp;/g, '&');

/** Bounded, read-only parser for the pinned Ministry single-sheet flat XLSX
 * tables. Iterates rows without constructing a million-cell object graph.
 * Deliberately rejects formulas and unsupported cell encodings, never uses
 * cached formula results. Dates remain raw serials for stable person keys. */
export function* ministryWorkbookRows(bytes: Uint8Array): Generator<string[]> {
  const zip = unzipSync(bytes, { filter: e => ['xl/sharedStrings.xml', 'xl/worksheets/sheet1.xml'].includes(e.name) });
  const strings = [...new TextDecoder().decode(zip['xl/sharedStrings.xml']).matchAll(/<si>([\s\S]*?)<\/si>/g)]
    .map(m => [...m[1].matchAll(/<t(?:\s[^>]*)?>([\s\S]*?)<\/t>/g)].map(t => xmlText(t[1])).join(''));
  if (!zip['xl/worksheets/sheet1.xml'] || !strings.length) throw new Error('Unsupported Ministry workbook');
  const text = new TextDecoder('utf-8', { fatal: true }).decode(zip['xl/worksheets/sheet1.xml']);
  for (const row of text.matchAll(/<row\b[^>]*>([\s\S]*?)<\/row>/g)) {
    const cells: string[] = [];
    for (const cell of row[1].matchAll(/<c\b([^>]*?)(?:\/>|>([\s\S]*?)<\/c>)/g)) {
      const address = cell[1].match(/\br="([A-Z]+)\d+"/)?.[1];
      if (!address || /<f\b/.test(cell[2] ?? '')) throw new Error('Unsupported/formula source cell');
      const index = [...address].reduce((v, c) => v * 26 + c.charCodeAt(0) - 64, 0) - 1;
      const type = cell[1].match(/\bt="([^"]+)"/)?.[1] ?? 'n';
      if (!['n', 's'].includes(type)) throw new Error(`Unsupported source cell type ${type}`);
      const raw = cell[2]?.match(/<v>([^<]*)<\/v>/)?.[1] ?? '';
      const value = type === 's' ? strings[Number(raw)] : raw;
      if (value === undefined) throw new Error('Unknown source shared string');
      cells[index] = value;
    }
    yield cells;
  }
}
