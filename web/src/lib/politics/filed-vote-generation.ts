import { createNormalSampler, createSeededRng, type NormalSampler } from '../core/rng';
import { isFiled } from '../workspaces/filing';
import type { PoliticsMunicipalListParameterRow, PoliticsVoteGenerationSource, RawUniListVotesRow } from './types';

function distance(election: string, reference: string): number {
  const days = (Date.parse(election) - Date.parse(reference)) / 86_400_000;
  if (!Number.isFinite(days) || days < 0) throw new Error(`Invalid vote generation reference date ${reference}`);
  return Math.sqrt(days);
}
function round(value: number): number {
  const lower = Math.floor(value), fraction = value - lower;
  return Math.abs(fraction - .5) <= Number.EPSILON * Math.max(1, value) ? lower + lower % 2 : Math.round(value);
}

/** Shared latent field for explicit filed sets. Normal draws retain the legacy
 * global-list/simulation, then local-list/simulation/unit traversal. Typed fields
 * replace expanded per-simulation row objects. Only this new scenario contract
 * enters here; historical fixtures keep the original generator unchanged. */
export function generateFiledListVotes(source: PoliticsVoteGenerationSource,
  options: { normal?: NormalSampler; seed?: string | number } = {}): Record<'camera' | 'senato', RawUniListVotesRow[]> {
  const units = source.base_dati, lists = source.liste, n = source.simulazioni, cells = units.length * n;
  if (!Number.isSafeInteger(n) || n < 1) throw new RangeError('Invalid simulation count');
  const requiredBytes = cells * (lists.length + 4) * Float64Array.BYTES_PER_ELEMENT;
  if (!Number.isSafeInteger(requiredBytes) || requiredBytes > 256 * 1024 ** 2) throw new RangeError('Explicit-filing numerical workspace exceeds 256 MiB; reduce the generation chunk');
  const normal = options.normal ?? createNormalSampler(createSeededRng(options.seed ?? 'vote-generation-default'));
  const municipal = new Map<string, PoliticsMunicipalListParameterRow[]>();
  for (const row of source.comuni_liste) {
    const key = String(row.CODICE_COMUNE), rows = municipal.get(key) ?? [];
    rows.push(row); municipal.set(key, rows);
  }
  const local = new Map<string, { unit: number; row: PoliticsMunicipalListParameterRow; dt: number }[]>();
  units.forEach((base, unit) => {
    for (const row of municipal.get(String(base.CODICE_COMUNE)) ?? []) {
      const rows = local.get(row.LISTA) ?? [];
      rows.push({ unit, row, dt: distance(source.data_elezione, row.DATA) }); local.set(row.LISTA, rows);
    }
  });
  const globals = lists.map(list => {
    const draws = new Float64Array(n), sd = list.SIGMA_GLOBAL * distance(source.data_elezione, list.DATA);
    for (let sim = 0; sim < n; sim++) draws[sim] = normal(list.LOGIT_P, sd);
    return draws;
  });
  const fields = lists.map((list, i) => {
    const field = new Float64Array(cells);
    for (let sim = 0; sim < n; sim++) for (const { unit, row, dt } of local.get(list.LISTA) ?? [])
      field[sim * units.length + unit] = 1 / (1 + Math.exp(-(globals[i][sim] + normal(row.DELTA, row.SIGMA_DELTA * dt))));
    return field;
  });
  const totals = new Float64Array(cells), valid = new Float64Array(cells);
  const filed = { camera: new Float64Array(cells), senato: new Float64Array(cells) };
  const masks = {} as Record<'camera' | 'senato', Uint8Array[]>;
  for (const ch of ['camera', 'senato'] as const) {
    const column = ch === 'camera' ? 'CU20_COD' : 'SU20_COD';
    const colleges = new Map(source[ch].uni.map(r => [String(r.UNI_COD), r.CIRC_COD]));
    masks[ch] = lists.map(list => Uint8Array.from(units, unit => {
      const circ = colleges.get(String(unit[column]));
      if (circ === undefined) throw new Error(`Unknown ${ch} filing geography ${unit[column]}`);
      return Number(isFiled(list, ch, circ));
    }));
  }
  for (let i = 0; i < lists.length; i++) for (let cell = 0; cell < cells; cell++) {
    const p = fields[i][cell]; totals[cell] += p;
    if (lists[i].LISTA === 'astensione') continue;
    valid[cell] += p;
    for (const ch of ['camera', 'senato'] as const) if (masks[ch][i][cell % units.length]) filed[ch][cell] += p;
  }
  for (let cell = 0; cell < cells; cell++) {
    if (!(totals[cell] > 0) || !Number.isFinite(totals[cell])) throw new Error('Cannot normalize generated votes');
    if (valid[cell] > 0 && (!(filed.camera[cell] > 0) || !(filed.senato[cell] > 0))) throw new Error('No filed political support in a physical unit');
  }
  const output = {} as Record<'camera' | 'senato', RawUniListVotesRow[]>;
  for (const ch of ['camera', 'senato'] as const) {
    const column = ch === 'camera' ? 'CU20_COD' : 'SU20_COD', aggregates = new Map<string, RawUniListVotesRow>();
    // Keep list/simulation/unit insertion order, including legitimate zero counts.
    for (let i = 0; i < lists.length; i++) {
      if (lists[i].LISTA === 'astensione') continue;
      for (let sim = 0; sim < n; sim++) for (let unit = 0; unit < units.length; unit++) {
        if (!masks[ch][i][unit]) continue;
        const cell = sim * units.length + unit;
        const count = valid[cell] === 0 ? 0 : round(fields[i][cell] / filed[ch][cell] * (valid[cell] / totals[cell]) * units[unit].ELETTORI);
        const code = units[unit][column], key = `${sim + 1}\u001f${code}\u001f${lists[i].LISTA}`;
        const previous = aggregates.get(key);
        if (previous) previous.VOTI_LISTA_SIM += count;
        else aggregates.set(key, { SIM: sim + 1, UNI_COD: code, LISTA: lists[i].LISTA, VOTI_LISTA_SIM: count });
      }
    }
    output[ch] = [...aggregates.values()];
  }
  return output;
}
