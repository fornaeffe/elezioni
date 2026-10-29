import type { FitContext, NationalSource } from '../../src/lib/politics/reform-2026/vote-share/types';
import type { ProfileSource } from '../../src/lib/politics/reform-2026/vote-share/profiles';
function counts(total: number, weights: number[]): number[] {
  const denominator = weights.reduce((s, x) => s + x, 0), result = weights.map(w => Math.floor(total * w / denominator));
  result[result.length - 1] += total - result.reduce((s, x) => s + x, 0); return result;
}
/** Synthetic dimension probe, not additional electoral evidence or a default. */
export function syntheticNational(components: number, pollCount: number): NationalSource {
  const ids = Array.from({ length: components }, (_, i) => 'p' + String(i).padStart(3, '0'));
  const dates = ['2010-01-01', '2011-01-01', '2014-01-01', '2016-01-01'];
  const elections = dates.map((date, e) => {
    const nonValid = 5000000 + e * 400000, electors = 20000000, context: FitContext = e % 2 ? 'european' : 'camera';
    return { id: 'event-' + e, date, context, electors, groups: ids.map(id => [id]),
      counts: [nonValid, ...counts(electors - nonValid, ids.map((_, j) => 1 + 0.25 * Math.sin(j * 1.7 + e * 0.4)))] };
  });
  const polls = Array.from({ length: pollCount }, (_, p) => {
    const groups = Array.from({ length: Math.min(8, components) }, () => [] as string[]);
    ids.forEach((id, j) => groups[j % groups.length].push(id));
    const weights = groups.map((g, j) => g.length * (1 + 0.15 * Math.sin(j * 0.5 + p * 0.01))), total = weights.reduce((s, x) => s + x, 0);
    const context: FitContext = p % 2 ? 'camera' : 'european', day = 2 + p % 25;
    const date = (d: number) => '2016-01-' + String(d).padStart(2, '0');
    return { id: 'poll-' + String(p).padStart(4, '0'), date: date(day), availableDate: date(day + 2), fieldworkEnd: date(day + 1),
      context, sample: 800, groups, shares: weights.map(w => w / total), errorGroup: '2016-H1-' + context };
  });
  return { components: ids, referenceDate: '2016-02-01', elections, polls };
}
export function syntheticProfiles(componentCount: number, municipalities: number): ProfileSource {
  const source = syntheticNational(componentCount, 0);
  const targets = Array.from({ length: municipalities }, (_, i) => ({ id: 'unit-' + String(i).padStart(5, '0'), region: 'r' + i % 4, electors: 5000 + i }));
  return { components: source.components, referenceDate: source.referenceDate, targetContext: 'camera', nationalElections: source.elections,
    targets, elections: source.elections.map((e, t) => ({ id: e.id, date: e.date, context: e.context,
      rows: targets.map((unit, c) => {
        const nonValid = Math.floor(unit.electors * (0.22 + (c % 5) * 0.02)), valid = unit.electors - nonValid;
        return { ...unit, nonValid, groups: e.groups,
          counts: counts(valid, source.components.map((_, j) => 1 + 0.3 * Math.sin(c * 0.1 + j * 0.3 + t * 0.2))) };
      }) })) };
}
