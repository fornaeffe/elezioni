/** Original compact-source boundary for the declared common-18+ Senate adapter. */
import { decodeSourceRows, rowFlags, validateSourceEnvelope, type AssetBytes } from './source-codec';
import { observableGroups, type SourceSelection } from './source-compiler';
import type { CompactSource } from './source-types';
import type { PairedHistory } from './senate-adapter';
import { day, sum } from './types';
import type { MunicipalObservation } from './profiles';

export async function compilePairedHistory(
  source: CompactSource,
  readChunk: (path: string) => Promise<AssetBytes>,
  selection: Pick<SourceSelection, 'referenceDate' | 'components' | 'sourceMemberships' | 'componentMap'>,
  pair: { camera: string; senato: string; electoratePolicy: 'equal-only' }
) {
  await validateSourceEnvelope(source);
  if (pair.electoratePolicy !== 'equal-only') throw new Error('PAIRED_ELECTORATE_POLICY_REQUIRED');
  const choices = new Map(source.payload.choices.map((c) => [c.id, c])),
    units = new Map(source.payload.units.map((u) => [u.id, u])),
    known = new Set(selection.components);
  if (!known.size || known.size !== selection.components.length)
    throw new Error('Invalid paired component basis');
  const excluded: { chamber: string; unit: string; reason: string }[] = [],
    dates: string[] = [],
    available: string[] = [];
  const tables = new Map<string, Map<string, MunicipalObservation>>();
  for (const chamber of ['camera', 'senato'] as const) {
    const event = source.payload.events.find((e) => e.id === pair[chamber]);
    if (
      !event ||
      event.context !== chamber ||
      event.minimumAge !== 18 ||
      event.role !== (chamber === 'camera' ? 'reference' : 'paired-adaptation') ||
      day(event.date) > day(selection.referenceDate) ||
      day(event.availableDate) > day(selection.referenceDate)
    )
      throw new Error('PAIRED_SOURCE_UNAVAILABLE: common 18+ cutoff-eligible observations required');
    dates.push(event.date);
    available.push(event.availableDate);
    const rows = await decodeSourceRows(source, event.id, await readChunk(event.chunk.path)),
      table = new Map<string, MunicipalObservation>();
    for (const row of rows) {
      const unit = units.get(row.unit)!;
      if (!unit.targetCode || unit.match !== 'same-place' || rowFlags(row, unit.match) & 1) {
        excluded.push({
          chamber,
          unit: row.unit,
          reason: 'Unmatched geography or invalid original denominator'
        });
        continue;
      }
      const raw = row.choices.map((c) => {
        const choice = choices.get(c.id)!,
          edit = selection.sourceMemberships?.[c.id];
        if (
          edit &&
          (!edit.provenance.trim() || !edit.members.length || (choice.opaque && edit.members.length > 1))
        )
          throw new Error('Invalid paired membership edit');
        return edit?.members ?? choice.members;
      });
      if (new Set(raw.flat()).size !== raw.flat().length)
        throw new Error('Overlapping original paired source memberships');
      const groups = raw.map((g) => [...new Set(g.map((id) => selection.componentMap?.[id] ?? id))].sort());
      if (groups.flat().some((id) => !known.has(id))) throw new Error('Incomplete paired membership/basis');
      const coarse = observableGroups(
        groups,
        row.choices.map((c) => c.count)
      );
      if (table.has(unit.targetCode))
        throw new Error('Duplicate paired target geography; review crosswalk before adapting');
      table.set(unit.targetCode, {
        id: unit.targetCode,
        region: unit.region,
        electors: row.electors,
        nonValid: row.electors - sum(coarse.map((g) => g.count)),
        groups: coarse.map((g) => g.members),
        counts: coarse.map((g) => g.count)
      });
    }
    tables.set(chamber, table);
  }
  if (dates[0] !== dates[1]) throw new Error('PAIRED_ELECTION_DATE_MISMATCH');
  const rows: PairedHistory['rows'] = [],
    camera = tables.get('camera')!,
    senato = tables.get('senato')!;
  for (const municipality of [...new Set([...camera.keys(), ...senato.keys()])].sort()) {
    const c = camera.get(municipality),
      s = senato.get(municipality);
    if (!c || !s || c.electors !== s.electors) {
      excluded.push({
        chamber: 'pair',
        unit: municipality,
        reason:
          !c || !s
            ? 'Missing matched chamber observation'
            : 'Different original 18+ electorates; excluded by equal-only policy, no denominator repair'
      });
      continue;
    }
    rows.push({ municipality, camera: c, senato: s });
  }
  if (!rows.length) throw new Error('No eligible paired common-18+ rows');
  return {
    history: {
      date: dates[0],
      availableDate: available.sort().at(-1)!,
      minimumAge: 18,
      sourceVersions: [source.id, ...source.sourceVersions],
      rows
    } satisfies PairedHistory,
    audit: {
      policy: 'paired-original-equal-18plus-v1',
      included: rows.length,
      excluded,
      conditionalElectors: sum(rows.map((r) => r.camera.electors)),
      assumption:
        'Only matched equal original 18+ denominators enter contrasts; unmatched or different-roll municipalities use labelled reference-profile fallbacks. No source counts or denominators are changed.'
    }
  };
}
