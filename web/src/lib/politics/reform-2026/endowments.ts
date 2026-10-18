import { allocateByQuotaAndRemainders, type QuotaAllocationResult } from './allocation';
import { exactFraction } from './exact';
import type { Chamber } from './input';
import type { DrawTrace, ReformWarning, ScrutinyExecutionContext } from './result';
import { validateGeographySummary, type ChamberGeography, type GeographySummary } from './geography';

export interface ApportionmentTrace {
  readonly id: string;
  readonly chamber: Chamber;
  readonly seats: number;
  readonly figures: readonly { readonly id: string; readonly population: number }[];
  readonly allocation: QuotaAllocationResult;
}
export interface SeatTableBundle {
  readonly schemaVersion: 1;
  readonly version: string;
  readonly geography: GeographySummary;
  readonly seed: string | null;
  readonly method: 'I08-v1' | 'official-decree';
  readonly chambers: readonly ChamberGeography[];
  readonly traces: readonly ApportionmentTrace[];
  readonly draws: readonly DrawTrace[];
  readonly warnings: readonly ReformWarning[];
}

function requireValid(errors: readonly string[]): void {
  if (errors.length) throw new Error(errors.join('\n'));
}

/** K56/K57 population quotas; I08's interim bonus/college formula is not a decree formula. */
export function buildProvisionalSeatTables(
  geography: GeographySummary,
  version: string,
  seed: string,
  execution: ScrutinyExecutionContext
): SeatTableBundle {
  requireValid(validateGeographySummary(geography));
  if (!version || !seed) throw new Error('Table version and generation seed are required');
  const traces: ApportionmentTrace[] = [];
  function allocate(chamber: Chamber, id: string, figures: readonly { id: string; population: number }[], seats: number): Map<string, number> {
    const allocation = allocateByQuotaAndRemainders({ seats, quotaMode: 'exact',
      subjects: [...figures].sort((a, b) => a.id < b.id ? -1 : a.id > b.id ? 1 : 0).map(f => ({ id: f.id, figure: exactFraction(f.population), tieValues: [] })),
      tieCriteria: [], context: { simulationId: 0, request: { chamber, norm: 'N03', rule: 'I03',
        contextId: `${version}/${id}`, lawReference: 'K56/K57; C3; S1; provisional I08-v1' } }
    }, execution);
    traces.push({ id, chamber, seats, figures: [...figures], allocation });
    if (allocation.status !== 'complete') throw new Error(`Non-computable population allocation ${id}: ${allocation.reason}`);
    return new Map(allocation.assignments.map(a => [a.id, a.seats]));
  }
  const chambers: ChamberGeography[] = [];
  for (const chamber of ['camera', 'senato'] as const) {
    const ts = geography.territories.filter(t => t.chamber === chamber);
    const figures = ts.map(t => ({ id: t.id, population: t.population }));
    let endowments: Map<string, number>;
    // K56: «dividendo il numero degli abitanti della Repubblica ... per trecentonovantadue»;
    // «sulla base dei quozienti interi e dei più alti resti».
    if (chamber === 'camera') endowments = allocate(chamber, 'camera-constituencies', figures, 392);
    else {
      // K57: «il Molise ne ha due, la Valle d'Aosta uno»; S1(4): «sei collegi uninominali».
      // I08: freeze fixed territories and minima below the unconstrained result, then recompute.
      endowments = new Map(ts.filter(t => ['02', '04', '14'].includes(t.regionId)).map(t => [t.id, t.regionId === '02' ? 1 : t.regionId === '04' ? 6 : 2]));
      let active = figures.filter(f => !endowments.has(f.id));
      let round = 0;
      while (active.length) {
        const remaining = 196 - [...endowments.values()].reduce((a, b) => a + b, 0);
        const allocation = allocate(chamber, `senato-regions-${round++}`, active, remaining);
        const below = active.filter(f => allocation.get(f.id)! < 3);
        if (!below.length) { for (const [id, seats] of allocation) endowments.set(id, seats); break; }
        for (const f of below) endowments.set(f.id, 3);
        active = active.filter(f => !endowments.has(f.id));
      }
    }
    // C3(1)(b): «a esclusione delle circoscrizioni di cui all'articolo 2» / «settanta seggi».
    // S1(2-ter)(a): «con esclusione delle regioni Valle d'Aosta ... e Trentino-Alto Adige» / «trentacinque seggi».
    // Population quotas and remainders for this bonus distribution are provisional I08, not a quoted decree formula.
    const bonuses = allocate(chamber, `${chamber}-bonus`, ts.filter(t => t.kind === 'ordinary').map(t => ({ id: t.id, population: t.population })), chamber === 'camera' ? 70 : 35);
    const territories = ts.map(t => ({ id: t.id, regionId: t.regionId, kind: t.kind, bonusSeats: bonuses.get(t.id) ?? 0 }));
    const colleges: ChamberGeography['colleges'][number][] = [];
    for (const t of ts) {
      const cs = geography.colleges.filter(c => c.territoryId === t.id);
      const total = endowments.get(t.id)!;
      if (t.kind !== 'ordinary') {
        const uni = cs.filter(c => c.kind === 'special-uninominal');
        if ((t.kind === 'vda' && total !== 1) || (t.kind === 'taa' && total !== (chamber === 'camera' ? 7 : 6))) throw new Error(`Statutory special-seat conflict: ${t.id} has ${total}`);
        for (const c of cs) {
          const seats = c.kind === 'special-uninominal' ? 1 : total - uni.length;
          colleges.push({ id: c.id, territoryId: t.id, kind: c.kind, seatsWithoutBonus: seats, seatsWithBonus: seats });
        }
      } else {
        const bonus = bonuses.get(t.id)!;
        if (bonus > total) throw new Error(`Bonus exceeds territory capacity ${t.id}`);
        const fs = cs.map(c => ({ id: c.id, population: c.population }));
        // C3(2)(a)-(b), S1(2-ter)(b)-(c): with bonus, «detratti quelli ...»;
        // without bonus, «tutti i seggi a questa spettanti». I08 supplies the interim within-territory formula.
        const without = allocate(chamber, `${t.id}/without-bonus`, fs, total);
        const withBonus = allocate(chamber, `${t.id}/with-bonus`, fs, total - bonus);
        for (const c of cs) {
          const a = without.get(c.id)!, b = withBonus.get(c.id)!;
          // Independent largest-remainder allocations can exhibit a population/house paradox.
          // Do not silently replace the approved method with a coupled allocation.
          if (b > a || a === 0) throw new Error(`Incompatible alternative college capacities: ${c.id} (${a}/${b})`);
          colleges.push({ id: c.id, territoryId: t.id, kind: c.kind, seatsWithoutBonus: a, seatsWithBonus: b });
        }
      }
    }
    const populationNotes = geography.notices.filter(n => n.severity === 'warning' && n.collegeIds.some(id => colleges.some(c => c.id === id))).map(n => n.message);
    chambers.push({ chamber, regions: geography.regions.map(r => ({ id: r.id, autonomous: r.autonomous })), territories, colleges,
      seatTable: { kind: 'provisional-census', reference: geography.censusReference, version, censusDate: geography.censusDate,
        geographyVersion: geography.version, method: 'I08-v1', populationNotes } });
  }
  const bundle: SeatTableBundle = { schemaVersion: 1, version, geography, seed, method: 'I08-v1', chambers, traces,
    draws: traces.flatMap(t => t.allocation.draws), warnings: traces.flatMap(t => t.allocation.warnings) };
  requireValid(validateSeatTableBundle(bundle));
  return bundle;
}

/** Also accepts verified official tables over the same catalog; does not assume I08 reproduces a decree. */
export function validateSeatTableBundle(b: SeatTableBundle): string[] {
  const errors = validateGeographySummary(b.geography);
  if (b.schemaVersion !== 1 || !b.version || !['I08-v1', 'official-decree'].includes(b.method) ||
      (b.method === 'I08-v1' ? !b.seed : b.seed !== null)) errors.push('Invalid seat-table metadata');
  if (b.method === 'official-decree' && (b.traces.length || b.draws.length || b.warnings.length)) errors.push('Official tables cannot claim I08 generation traces');
  if (b.chambers.length !== 2 || new Set(b.chambers.map(c => c.chamber)).size !== 2) errors.push('Both chambers are required');
  for (const chamber of ['camera', 'senato'] as const) {
    const c = b.chambers.find(c => c.chamber === chamber);
    if (!c) continue;
    const ts = b.geography.territories.filter(t => t.chamber === chamber);
    const cs = b.geography.colleges.filter(t => t.chamber === chamber);
    const source = c.seatTable;
    if (source.kind !== (b.method === 'I08-v1' ? 'provisional-census' : 'official-decree') || source.version !== b.version || !source.reference) errors.push(`Invalid ${chamber} table provenance`);
    if (source.kind === 'provisional-census' && (source.censusDate !== b.geography.censusDate || source.geographyVersion !== b.geography.version || source.method !== b.method)) errors.push('Mismatched census/geography/method');
    if (c.regions.length !== b.geography.regions.length || c.regions.some(r => !b.geography.regions.some(g => g.id === r.id && g.autonomous === r.autonomous)) || new Set(c.regions.map(r => r.id)).size !== c.regions.length) errors.push('Mismatched regions');
    if (c.territories.length !== ts.length || new Set(c.territories.map(t => t.id)).size !== ts.length || c.colleges.length !== cs.length || new Set(c.colleges.map(t => t.id)).size !== cs.length) errors.push(`Incomplete ${chamber} seat geography`);
    for (const college of c.colleges) {
      const mapped = cs.find(x => x.id === college.id);
      if (!mapped || college.kind !== mapped.kind || college.territoryId !== mapped.territoryId ||
          !Number.isSafeInteger(college.seatsWithoutBonus) || college.seatsWithoutBonus <= 0 || !Number.isSafeInteger(college.seatsWithBonus) || college.seatsWithBonus < 0 || college.seatsWithBonus > college.seatsWithoutBonus ||
          (college.kind === 'special-uninominal' && (college.seatsWithoutBonus !== 1 || college.seatsWithBonus !== 1))) errors.push(`Invalid capacity ${college.id}`);
    }
    for (const t of c.territories) {
      const mapped = ts.find(x => x.id === t.id);
      const colleges = c.colleges.filter(x => x.territoryId === t.id);
      const without = colleges.reduce((s, x) => s + x.seatsWithoutBonus, 0);
      const withBonus = colleges.reduce((s, x) => s + x.seatsWithBonus, 0);
      if (!mapped || mapped.kind !== t.kind || mapped.regionId !== t.regionId || !Number.isSafeInteger(t.bonusSeats) || t.bonusSeats < 0 || without - withBonus !== t.bonusSeats) errors.push(`Invalid territorial capacity ${t.id}`);
      if (t.kind !== 'ordinary' && (t.bonusSeats !== 0 || without !== (t.kind === 'vda' ? 1 : chamber === 'camera' ? 7 : 6))) errors.push(`Invalid special capacity ${t.id}`);
      if (chamber === 'senato' && without < (t.regionId === '02' ? 1 : t.regionId === '14' ? 2 : t.regionId === '04' ? 6 : 3)) errors.push(`Constitutional minimum ${t.id}`);
    }
    const sum = (f: (x: ChamberGeography['colleges'][number]) => number) => c.colleges.reduce((s, x) => s + f(x), 0);
    const total = chamber === 'camera' ? 392 : 196, bonus = chamber === 'camera' ? 70 : 35;
    if (sum(x => x.seatsWithoutBonus) !== total || sum(x => x.seatsWithBonus) !== total - bonus || c.territories.reduce((s, t) => s + t.bonusSeats, 0) !== bonus ||
        sum(x => x.kind === 'ordinary' ? x.seatsWithBonus : 0) !== (chamber === 'camera' ? 314 : 154)) errors.push(`Seat conservation ${chamber}`);
    const requiredNotes = b.geography.notices.filter(n => n.severity === 'warning' && n.collegeIds.some(id => c.colleges.some(x => x.id === id))).map(n => n.message);
    if (requiredNotes.some(n => !source.populationNotes?.includes(n))) errors.push('Population estimation disclosure is missing');
  }
  return errors;
}

/** Explicit adapter; callers provide votes/candidatures separately in R26-19. */
export function chamberGeography(bundle: SeatTableBundle, chamber: Chamber): ChamberGeography {
  requireValid(validateSeatTableBundle(bundle));
  return structuredClone(bundle.chambers.find(c => c.chamber === chamber)!);
}

export function seatTableMessages(bundle: SeatTableBundle): readonly { severity: 'info' | 'warning'; message: string }[] {
  return [{ severity: 'info', message: bundle.method === 'I08-v1'
    ? `Dotazioni provvisorie I08 basate sul censimento del ${bundle.geography.censusDate}; non sono tabelle di un decreto di assegnazione.`
    : 'Dotazioni tratte dal decreto di assegnazione indicato nella fonte.' }, ...bundle.geography.notices.map(({ severity, message }) => ({ severity, message }))];
}
