import { apportion } from '../generation-math';
import { forecastRandom } from './forecast-random';
import { day } from './types';
import type { SourceGeography } from './source-types';
import type { PhysicalCell, TargetElectorate } from './forecast-types';
import type { Chamber } from '../input';
/** Conditional dated electorates are reconstructed BEFORE filing or sampling.
 * Every physical census part receives an integer electorate; no vote is moved
 * to another college after a multinomial draw. No Camera TAA double ballot. */
export function physicalCells(
  geography: SourceGeography,
  electorates: TargetElectorate[],
  chambers: Chamber[],
  seed: string
) {
  const targets = new Map(electorates.map((e) => [e.code, e])),
    colleges = new Map(geography.colleges.map((c) => [c.id, c]));
  if (
    targets.size !== electorates.length ||
    new Set(geography.targets.map((t) => t.code)).size !== geography.targets.length ||
    electorates.length !== geography.targets.length ||
    electorates.some((e) => !e.provenance.trim())
  )
    throw new Error('ELECTORATE_CROSSWALK: incomplete/duplicate dated target');
  const output: PhysicalCell[] = [],
    ids = new Set<string>();
  const byMunicipality = new Map<string, SourceGeography['cells']>();
  for (const cell of geography.cells) {
    if (!targets.has(cell.municipality)) throw new Error('PHYSICAL_GEOGRAPHY_INVALID: unknown municipality');
    const key = cell.chamber + ':' + cell.municipality,
      rows = byMunicipality.get(key) ?? [];
    rows.push(cell);
    byMunicipality.set(key, rows);
  }
  for (const municipality of [...geography.targets].sort((a, b) => a.code.localeCompare(b.code))) {
    const target = targets.get(municipality.code);
    if (!target) throw new Error('ELECTORATE_CROSSWALK: missing target ' + municipality.code);
    day(target.date);
    for (const chamber of chambers) {
      const n = target[chamber];
      if (!Number.isSafeInteger(n) || n < 0) throw new Error('Invalid conditional electorate');
      const parts = (byMunicipality.get(chamber + ':' + municipality.code) ?? []).sort(
        (a, b) => a.unitId.localeCompare(b.unitId) || a.collegeId.localeCompare(b.collegeId)
      );
      if (
        !parts.length ||
        parts.reduce((s, p) => s + p.population, 0) !== municipality.population ||
        parts.some(
          (p) =>
            !Number.isSafeInteger(p.population) ||
            p.population < 0 ||
            colleges.get(p.collegeId)?.kind === 'taa-proportional' ||
            colleges.get(p.collegeId)?.chamber !== chamber
        )
      )
        throw new Error('PHYSICAL_GEOGRAPHY_INVALID');
      const rng = forecastRandom(seed, 0, 'electorate-reconstruction:' + chamber + ':' + municipality.code),
        counts = apportion(
          n,
          parts.map((p) => p.population),
          rng.uniform
        );
      parts.forEach((p, i) => {
        const id = JSON.stringify([chamber, municipality.code, p.unitId, p.collegeId]);
        if (ids.has(id)) throw new Error('Duplicate physical cell');
        ids.add(id);
        output.push({
          id,
          chamber,
          collegeId: p.collegeId,
          municipality: municipality.code,
          region: municipality.region,
          province: municipality.province,
          electors: counts[i]
        });
      });
    }
  }
  return output.sort((a, b) => a.id.localeCompare(b.id));
}
