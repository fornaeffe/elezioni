/** Camera-only research preparation approved for this comparison, never browser routing. */
import type { ReplaySource } from './fast-share-replay';
import type { loadPartyResearchData } from '../data-preparation/party-drift-panel-data';
import { membershipFor } from '../data-preparation/party-drift-memberships';
import { observableGroups } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { fitReference } from './archive/models-v1/information';
import { fitProfiles } from './archive/models-v1/profiles';
import { identificationGroups, originalPartitions, unidentifiedDirections } from './archive/models-v1/identification';
import { poolConditional, completeLocal, COMPONENT_POLICY, type ComponentObservation } from './archive/models-v1/component-policy';
import { legacySurfaces, legacyMovement } from './archive/models-v1/legacy-components';
import type { CompiledLegacy } from './archive/models-v1/legacy-types';
import { pollChoiceId, type SourceComposition } from './archive/models-v1/correspondences';
import { feasibleField } from './archive/models-v1/feasible-field';
import { forecastRandom } from '../../src/lib/politics/reform-2026/vote-share/forecast-random';
import { referenceNoise } from '../../src/lib/politics/reform-2026/vote-share/reference-noise';
import { projectMargins } from './archive/models-v1/projection';
import { politicalIntervention, removeAbsent } from './archive/models-v1/interventions';
import { movementScales } from '../../src/lib/politics/reform-2026/vote-share/transition';
import { multinomial } from '../../src/lib/politics/reform-2026/generation-math';
import { sum, years } from './archive/models-v1/types';
import type { PhysicalCell } from '../../src/lib/politics/reform-2026/vote-share/forecast-types';
import type { ReleasedScenario } from '../../src/lib/politics/reform-2026/vote-share/release';
import type { ReformAssemblyContext } from '../../src/lib/politics/reform-2026/generation-types';
import policy from '../../../data/legacy-comparison-policy.json' with { type: 'json' };

export type ResearchInput = Awaited<ReturnType<typeof loadPartyResearchData>>;
export type CameraTarget = { id: string; region: string; electors: number; offered: string[] };
export type CameraDesign = { components: string[]; cutoff: string; targetDate: string; targets: CameraTarget[] };
export function design(source: ReplaySource): CameraDesign {
  // Only conditional electorates and filed identities cross this boundary. No held-out counts.
  return { components: source.components, cutoff: source.cutoff, targetDate: source.targetDate,
    targets: source.truth.map(t => ({ id: t.id, region: t.region, electors: t.electors, offered: t.groups.flat() })) };
}

/** Restore original offered-set strata absent from the old aggregate export.
 * Retain its population/domain and aggregate scale-estimation view exactly. */
export function restoreOriginalStrata<T extends Pick<ReplaySource, 'components' | 'nationalHistory' | 'cutoff'>>(source: T, input: ResearchInput) {
  const known = new Set(source.components), canonical = (id: string) => known.has(id) ? id : 'research-other';
  const { nationalHistory, ...rest } = source;
  return { ...rest, nationalHistory: nationalHistory.map(n => {
    const event = input.history.events.find(e => e.id === n.id);
    if (!event || event.date >= source.cutoff) throw new Error('INELIGIBLE_STRATA_SOURCE:' + n.id);
    const strata = new Map<string, { groups: string[][]; counts: number[]; electors: number }>();
    for (const m of event.municipalities) {
      if (m.regionId === '02' || !m.matchable || m.electors <= 0 || m.validVotes > m.electors) continue;
      const observed = observableGroups(m.lists.map(l => [...new Set(membershipFor(input.policy, event.id, l.name).members.map(canonical))].sort()), m.lists.map(l => l.votes)),
        groups = observed.map(g => g.members), key = JSON.stringify(groups),
        row = strata.get(key) ?? { groups, counts: Array(groups.length + 1).fill(0), electors: 0 };
      row.electors += m.electors; row.counts[0] += m.electors - m.validVotes;
      observed.forEach((g, j) => row.counts[j + 1] += g.count); strata.set(key, row);
    }
    const rows = [...strata.values()];
    if (sum(rows.map(r => r.electors)) !== n.electors || sum(rows.map(r => r.counts[0])) !== n.counts[0])
      throw new Error('STRATA_DOMAIN_MISMATCH:' + n.id);
    return { ...n, strata: rows };
  }) };
}

/** These existing reviewed joint weights and the declared equal-weight fallback
 * are scenario assumptions. They are never M02 component observations. */
function composition(members: string[], event = '', label = ''): SourceComposition {
  let weights = (policy.jointWeights as Record<string, Record<string, number>>)[event + ':' + label];
  const signature = [...members].sort().join('|');
  const known: Record<string, Record<string, number>> = {
    'art1|possible|sel-si': { art1: .4, possible: .1, 'sel-si': .5 },
    'noi-residual|scelta-civica|udc': { 'noi-residual': .6, 'scelta-civica': .15, udc: .25 },
    'centro-democratico|impegno-residual': { 'centro-democratico': 1, 'impegno-residual': 0 },
  };
  weights ??= known[signature];
  return { fractions: Object.fromEntries(members.map(id => [id, weights ? weights[id] ?? 0 : 1])),
    provenance: weights ? 'Existing reviewed legacy-comparison joint composition; not observed internal counts.' :
      'Explicit equal-weight research composition, following legacy-comparison-policy.json defaultJointWeight; includes dated pooled groups.' };
}

export function prepareCameraM01(d: CameraDesign, input: ResearchInput, template: ReleasedScenario, context: ReformAssemblyContext,
  cells: PhysicalCell[], ordinaryColleges: string[], seed: string) {
  const ids = [...d.components].sort(), n = ids.length + 1,
    position = new Map(ids.map((id, j) => [id, j + 1]));
  if (!position.has('research-other')) throw new Error('FROZEN_RESEARCH_POOL_REQUIRED');
  const canonical = (id: string) => position.has(id) ? id : 'research-other';
  const event = input.history.events.filter(e => e.kind === 'camera' && e.date < d.cutoff).at(-1);
  if (!event) throw new Error('NO_CUTOFF_CAMERA_BASELINE');
  const all: ComponentObservation[] = [], exact = new Map<string, ComponentObservation>();
  const compositions: Record<string, SourceComposition> = {};
  for (const m of event.municipalities) {
    if (!m.matchable || m.electors <= 0 || m.validVotes > m.electors) continue;
    const counts = Array(n).fill(0) as number[], offered = Array(n).fill(false) as boolean[];
    counts[0] = m.electors - m.validVotes; offered[0] = true;
    for (const list of m.lists) {
      const members = membershipFor(input.policy, event.id, list.name).members,
        key = event.id + ':' + list.name,
        c = compositions[key] ??= composition(members, event.id, list.name),
        total = sum(Object.values(c.fractions));
      if (!(total > 0)) throw new Error('EMPTY_RESEARCH_COMPOSITION:' + key);
      for (const member of members) {
        const j = position.get(canonical(member))!;
        offered[j] = true;
        counts[j] += list.votes * c.fractions[member] / total;
      }
    }
    const row = { code: m.id, region: m.regionId, electors: m.electors, counts, offered };
    all.push(row); exact.set(m.id, row);
  }
  const national = poolConditional(all, n), regions = new Map<string, Float64Array>();
  for (const region of new Set(d.targets.map(t => t.region)))
    regions.set(region, poolConditional(all.filter(r => r.region === region), n, national.probabilities).probabilities);
  const rows = d.targets.map(t => {
    const q = completeLocal(exact.get(t.id), regions.get(t.region)!);
    const total = sum(q);
    return { code: t.id, probabilities: [...q.map(v => v / total)], original: exact.get(t.id) };
  });
  const s = { ...template, referenceDate: d.cutoff, electionDate: d.targetDate, renewal: 'camera' as const, seed };
  const compiled: CompiledLegacy = {
    inputKey: 'research-camera-only', policy: COMPONENT_POLICY, componentIds: ids, componentMap: {},
    pollCollegeIds: { camera: ordinaryColleges, senato: [] }, historicalDate: event.date,
    rows: { camera: rows, senato: [] }, polls: [], compositions, requiredReferences: [],
    audit: { baseline: { camera: event.id, senato: 'not-used-camera-only-test' },
      missingHistory: rows.filter(r => !r.original).map(r => 'camera/' + r.code), policy: COMPONENT_POLICY, fallbackCounts: {} },
  };
  // The caller supplies only the frozen, cutoff-eligible question groups.
  const setPolls = (polls: ReplaySource['polls']) => {
    compiled.polls = polls.filter(p => p.kind === 'camera').map(p => ({
      id: p.id, date: p.date.slice(0, 10), available: p.available.slice(0, 10), end: p.end.slice(0, 10),
      context: 'camera', sample: p.sample, source: 'Frozen original-domain research poll ' + p.id,
      enabled: true, complete: true,
      groups: p.groups.map((members, g) => {
        const label = members.join('|');
        compositions[pollChoiceId(p.id, label)] = composition(members);
        return { label, members, share: p.shares[g] };
      }),
    }));
    const required = ids.filter((id, j) => d.targets.some(t => t.offered.includes(id)) &&
      (!national.observed[j + 1] || national.probabilities[j + 1] === 0 || national.unidentified[j + 1]));
    const anchors = required.map(id => {
      // An explicit named group may provide an assumed split; an unnamed Other
      // complement may not manufacture a new identity. Prefer separate evidence.
      const candidates = polls.filter(p => p.kind === 'camera').flatMap(p => p.groups.flatMap((g, at) =>
        g.includes(id) && !g.includes('research-other') ? [{ p, g, at }] : []))
        .sort((a, b) => b.p.available.localeCompare(a.p.available) || b.p.end.localeCompare(a.p.end) || a.p.id.localeCompare(b.p.id));
      const point = candidates[0];
      if (!point) throw new Error('M01_HISTORICAL_REFERENCE_REQUIRED:' + id);
      const weights = composition(point.g).fractions;
      return { id, mean: point.p.shares[point.at] * weights[id] / sum(Object.values(weights)),
        poll: point.p.id, available: point.p.available, composition: weights };
    });
    compiled.requiredReferences = required;
    return anchors;
  };
  return { ids, compiled, s, context, cells, setPolls, national };
}

export function prepareCameraM02(source: Omit<ReplaySource, 'truth' | 'nationalTruth'>, targets: CameraTarget[], seed: string) {
  const national = { components: source.components, referenceDate: source.cutoff,
    elections: source.nationalHistory.map(({ kind, ...e }) => ({ ...e, context: kind })),
    polls: source.polls.map(({ kind, available, end, group, ...p }) => ({ ...p, date: p.date.slice(0, 10),
      context: kind, availableDate: available.slice(0, 10), fieldworkEnd: end.slice(0, 10), errorGroup: group })) };
  const partitions = originalPartitions(national);
  if (identificationGroups(source.components, partitions).some(g => g.length > 1) ||
      unidentifiedDirections(source.components, partitions).length)
    throw new Error('UNRESOLVED_RESEARCH_BASIS');
  const fit = fitReference(national, seed + ':fit');
  if (!fit.political.diagnostics.passed || !fit.participation.diagnostics.passed) throw new Error('FIT_NOT_ACCEPTED');
  const profile = fitProfiles({ originalOffered: true, components: source.components, referenceDate: source.cutoff,
    targetContext: 'camera', nationalElections: national.elections,
    elections: source.elections.map(({ kind, ...e }) => ({ ...e, context: kind })),
    targets: targets.map(t => ({ id: t.id, region: t.region, electors: t.electors })) });
  return { fit, profile };
}

export function cameraM02Reference(prepared: ReturnType<typeof prepareCameraM02>, cells: PhysicalCell[], seed: string, draw: number, rho: number) {
  const { fit, profile } = prepared, k = fit.componentIds.length, n = k + 1;
  const row = forecastRandom(seed, draw, 'posterior-political-row', 'camera', rho).integer(fit.political.chains * fit.political.drawsPerChain);
  const row0 = forecastRandom(seed, draw, 'posterior-non-valid-row', 'camera', rho).integer(fit.participation.chains * fit.participation.drawsPerChain);
  const a = fit.participation.values[row0 * 4], q = fit.political.values.slice(row * 2 * k, row * 2 * k + k);
  const b = Float64Array.from([a, ...q.map(v => (1 - a) * v)]),
    profiles = new Map(profile.municipalityIds.map((id, i) => [id, i])),
    noise = referenceNoise(seed, draw, n, rho, .25),
    scores = cells.map(c => {
      const at = profiles.get(c.municipality)! * n, z = noise('camera', c.municipality, c.region);
      return Float64Array.from(b, (v, j) => v > 0 ? Math.log(v) + profile.means[at + j] + profile.profileSd[at + j] * z[j] : -Infinity);
    });
  const projected = projectMargins(b, cells.length, i => ({ weight: cells[i].electors, scores: scores[i] }));
  return cells.map((_, i) => projected.probabilities(i));
}

export function generateCameraDraw(model: 'M01' | 'M02', d: CameraDesign, cells: PhysicalCell[],
  seed: string, draw: number, context: ReformAssemblyContext,
  prepared: ReturnType<typeof prepareCameraM02>, legacy: ReturnType<typeof prepareCameraM01>,
  anchors: ReturnType<ReturnType<typeof prepareCameraM01>['setPolls']>) {
  const ids = prepared.fit.componentIds, n = ids.length + 1,
    profile = context.models.chambers.profiles.find(p => p.id === legacy.s.chamberProfileId)!,
    rho = profile.sharedVariance + (1 - profile.sharedVariance) * profile.residualCorrelation;
  let reference: Float64Array[], future: readonly Float64Array[];
  if (model === 'M01') {
    const scenario = { ...legacy.s, seed };
    reference = legacySurfaces(scenario, legacy.compiled, cells, 'camera', draw, false);
    if (anchors.length) politicalIntervention(reference, cells.map(c => c.electors),
      anchors.map(a => [ids.indexOf(a.id) + 1]), anchors.map(a => a.mean), () => 1);
    future = legacyMovement(scenario, legacy.compiled, context, cells, reference, ids, 'camera', draw);
  } else {
    reference = cameraM02Reference(prepared, cells, seed, draw, rho);
    const scale = movementScales(prepared.fit.audit.scales.political, prepared.fit.audit.scales.participation,
      forecastRandom(seed, draw, 'movement-scales', 'camera', rho));
    future = feasibleField({ model: 'bounded-mass-pair-field-v2', reference, cells,
      politicalScales: new Float64Array(n - 1).fill(scale.political), localScales: prepared.profile.futureScales,
      participationScale: scale.nonValid, discrepancy: 1.5 * prepared.fit.audit.electionDiscrepancy,
      horizon: years(d.targetDate, d.cutoff), seed, simulationId: draw, chamber: 'camera', correlation: rho, scaleMultiplier: 1.5 }).probabilities;
  }
  const targets = new Map(d.targets.map((t, i) => [t.id, { t, i }])), out = new Uint32Array(d.targets.length * n);
  for (let i = 0; i < cells.length; i++) {
    const c = cells[i], target = targets.get(c.municipality)!,
      p = removeAbsent(future[i], [true, ...ids.map(id => target.t.offered.includes(id))]),
      counts = multinomial(c.electors, [...p], forecastRandom(seed, draw, 'multinomial:' + c.id, 'camera', rho).uniform);
    if (sum(counts) !== c.electors) throw new Error('PHYSICAL_ELECTORATE_CONSERVATION');
    counts.forEach((v, j) => out[target.i * n + j] += v);
    counts.forEach((v, j) => { if (j && v && !target.t.offered.includes(ids[j - 1])) throw new Error('FILING_ZERO_VIOLATION'); });
  }
  d.targets.forEach((t, i) => { if (sum(out.subarray(i * n, (i + 1) * n)) !== t.electors) throw new Error('MUNICIPAL_ELECTORATE_CONSERVATION'); });
  return out;
}
