/** Test-only construction from retained observations and frozen legal geography; no production converter. */
import { readFileSync } from 'node:fs';
import { gunzipSync } from 'node:zlib';
import { originalFitFixture } from './fast-fit-input';
import { fixture as domainFixture } from '../../src/lib/politics/scenario/scenario.fixture';
import { ROSATELLUM, REFORM, chambers, type Asset, type Location, type LawId, type PoliticsScenario } from '../../src/lib/politics/scenario/schema';
import { byteHash } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import type { SourceGeography } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import type { PoliticsGeneratedAdapterFixture } from '../../src/lib/politics/types';
import type { ExecutionCatalogue, GenerationParameters, RosatellumSeats } from '../../src/lib/politics/execution';
import { implementationKey } from '../../scripts/data-preparation/fast-share-release-io';
import { fileURLToPath } from 'node:url';
type Mutable<T> = T extends readonly (infer V)[] ? Mutable<V>[] : T extends object ? { -readonly [K in keyof T]: Mutable<T[K]> } : T;
export async function commonPoliticsFixture(law: LawId = REFORM, model: 'M01' | 'M02' = 'M01') {
  const original = await originalFitFixture(), prior = original.accepted.scenario;
  const scenario = structuredClone(domainFixture().scenario) as Mutable<PoliticsScenario>;
  const bytes = new Map<string, Uint8Array<ArrayBuffer>>();
  const put = async (id: string, version: string, value: unknown): Promise<Asset> => {
    const b = new TextEncoder().encode(JSON.stringify(value)); bytes.set(id, b);
    return { id, version, schemaVersion: 1, bytes: b.length, sha256: await byteHash(b), uri: `/constructed/${id}.json` };
  };
  scenario.id = 'common-execution'; scenario.name = 'Constructed execution fixture';
  scenario.selection.scrutiny.id = law; scenario.selection.generatorsByLaw[REFORM].id = model;
  scenario.electionDate = prior.electionDate; scenario.referenceDate = prior.referenceDate;
  scenario.execution.fittingSeed = prior.fitSeed; scenario.execution.simulationSeed = prior.seed;
  scenario.history = { electionIds: [...prior.historySelection.electionIds], pairedCamera: prior.historySelection.pairedCamera,
    pairedSenato: prior.historySelection.pairedSenato, polls: { selected: [...prior.polls.selected], added: [], excluded: {} },
    sourceMemberships: original.source.payload.choices.map(c => ({ sourceId: c.eventId, questionId: 'q', choiceId: c.id,
      members: [...c.members], provenance: 'Constructed exact observation' })), pooling: [] };
  const lists = prior.lists.camera;
  scenario.subjects.coalitions = [...new Set(lists.flatMap(l => l.coalitionId === null ? [] : [l.coalitionId]))]
    .map(id => ({ id, name: id, color: '#123456', chambers: ['camera', 'senato'] }));
  scenario.subjects.lists = lists.map(l => ({ id: l.id, name: l.name, color: '#123456', kind: l.kind ?? 'list', coalitionId: l.coalitionId, minority: null }));
  scenario.subjects.components = [];
  const fallback = domainFixture().scenario.newComponents[0].fallback;
  scenario.memberships = lists.map(l => ({ listId: l.id, camera: [...l.componentIds],
    senato: [...prior.lists.senato.find(s => s.id === l.id)!.componentIds], locked: true, newComponentId: `new-${l.id}` }));
  scenario.newComponents = lists.map(l => ({ id: `new-${l.id}`, name: `New ${l.id}`, support: 'Explicit test assumption',
    provenance: 'Test only', fallback: structuredClone(fallback) }));
  scenario.references = prior.references.map(r => ({ ...r, scope: r.scope.kind === 'national' ? r.scope : { ...r.scope, namespace: 'reference-test' }, denominator: 'valid-votes', appliesTo: ['M01', 'M02'],
    uncertainty: { sdPercentagePoints: r.sdPercentagePoints, origin: 'explicit', policyVersion: null } }));
  scenario.nonValidReferences = prior.nonValidReferences.map(r => ({ ...r, scope: r.scope.kind === 'national' ? r.scope : { ...r.scope, namespace: 'reference-test' }, denominator: 'electors', appliesTo: ['M01', 'M02'],
    uncertainty: { sdPercentagePoints: r.sdPercentagePoints, origin: 'explicit', policyVersion: null } }));
  // The common grammar owns the SD encoding; old statistical input fields stay outside it.
  for (const r of [...scenario.references, ...scenario.nonValidReferences]) delete (r as unknown as { sdPercentagePoints?: number }).sdPercentagePoints;
  scenario.modelSettings.M01.compositions = original.source.payload.choices.map(c => ({ sourceId: c.eventId, questionId: 'q', choiceId: c.id,
    fractions: Object.fromEntries(c.members.map(id => [id, 1 / c.members.length])), provenance: 'Observed singleton', proxy: false }));
  scenario.modelSettings.M01.supplementalPolls = [];
  scenario.modelSettings.M01.parameterDate = '2026-10-16'; // Retained compiler's distinct latest-poll parameter date.
  scenario.modelSettings.M02.forecast = prior.models.forecast ?? 'bounded-mass-pair-field-v2';
  scenario.modelSettings.M02.forecastScale = prior.models.forecastScale ?? 1;
  scenario.modelSettings.M02.profileRegionCorrelation = prior.models.profileRegionCorrelation ?? 0;
  scenario.modelSettings.M02.forecastReference = prior.models.forecastReference ?? 'conditional';
  scenario.modelSettings.M02.uncertaintyMultipliers = [];
  const locations: Location[] = [], relations: ExecutionCatalogue['geographyRelations'][number][] = [];
  const location = (l: LawId, ch: 'camera' | 'senato', level: Location['level'], code: string, version: string): Location => {
    const value = { law: l, geographyVersion: version, chamber: ch, level, code };
    if (!locations.some(x => JSON.stringify(x) === JSON.stringify(value))) locations.push(value);
    return value;
  };
  const old = JSON.parse(gunzipSync(readFileSync(new URL('../../../test/fixtures/politiche/generated_adapter.json.gz', import.meta.url))).toString('utf8')) as PoliticsGeneratedAdapterFixture;
  const ros: RosatellumSeats = { schemaVersion: 1, kind: 'rosatellum-seats', version: 'constructed-ros-seats', geographyVersion: 'constructed-ros-geography',
    chambers: chambers.map(ch => ({ chamber: ch, totalSeats: ch === 'camera' ? 392 : 196,
      uninominal: old.rami[ch].uni, plurinominal: old.rami[ch].pluri.map(p =>
        ({ ...p, MAX_CANDIDATI: p.PLURI_COD === null ? 0 : Math.min(4, Math.max(2, p.SEGGI_PLURI)) })) })),
    ballots: chambers.flatMap(ch => old.rami[ch].uni.map(u => ({ id: `ros:${ch}:${u.UNI_COD}`, chamber: ch,
      uninominal: String(u.UNI_COD), plurinominal: u.PLURI_COD === null ? null : String(u.PLURI_COD), constituency: String(u.CIRC_COD),
      regionId: String(ch === 'camera' ? Math.floor(Number(u.CIRC_COD) / 100) : u.CIRC_COD).padStart(2, '0') }))) };
  const geography: SourceGeography = { ...structuredClone(original.accepted.assets.geography), version: ros.geographyVersion,
    targets: original.accepted.assets.geography.targets.map(t => ({ ...t, population: 1000000 })),
    colleges: ros.ballots.map(b => ({ id: b.id, chamber: b.chamber, kind: b.regionId === '02' ? 'special-uninominal' : 'ordinary', territoryId: b.constituency })), cells: [] };
  for (const ch of chambers) for (const [i, t] of geography.targets.entries()) {
    const ballots = ros.ballots.filter(b => b.chamber === ch && b.regionId === t.region);
    ballots.forEach((b, j) => geography.cells.push({ chamber: ch, collegeId: b.id, municipality: t.code, unitId: `${ch}:${i}:${j}`,
      population: Math.floor(t.population / ballots.length) + (j < t.population % ballots.length ? 1 : 0) }));
  }
  for (const b of ros.ballots) {
    const uni = location(ROSATELLUM, b.chamber, 'uninominal', b.uninominal, geography.version);
    if (b.plurinominal !== null) relations.push({ parent: location(ROSATELLUM, b.chamber, 'plurinominal', b.plurinominal, geography.version), child: uni });
  }
  const rgeo = original.accepted.context.seats.geography;
  for (const t of rgeo.territories) for (const c of rgeo.colleges.filter(c => c.territoryId === t.id))
    relations.push({ parent: location(REFORM, t.chamber, 'territory', t.id, rgeo.version), child: location(REFORM, c.chamber, 'college', c.id, rgeo.version) });
  scenario.lawSettings[ROSATELLUM].filings = lists.map(l => ({ listId: l.id, locations: locations.filter(loc => loc.law === ROSATELLUM &&
    (loc.level === 'plurinominal' || ros.ballots.some(b => b.chamber === loc.chamber && b.uninominal === loc.code && b.plurinominal === null))) }));
  scenario.lawSettings[REFORM].filings = lists.map(l => ({ listId: l.id, locations: prior.coverage.filter(c => c.listIds.includes(l.id)).map(c =>
    locations.find(loc => loc.law === REFORM && loc.level === 'college' && loc.chamber === c.chamber && loc.code === c.collegeId)!) }));
  scenario.lawSettings[REFORM].bonusLinks = [];
  scenario.lawSettings[REFORM].candidateGeneration = { ...prior.candidateGeneration, bonusPolicy: 'reference-proportional-v1' };
  scenario.lawSettings[REFORM].preferenceProfileId = prior.preferenceProfileId;
  scenario.lawSettings[REFORM].chamberProfileId = prior.chamberProfileId;
  scenario.data.historical = await put('source', original.source.id, original.source);
  scenario.data.electorate = await put('electorate', 'conditional-electors-test', { schemaVersion: 1, kind: 'politics-electorate', minimumAge: 18, targets: original.accepted.assets.electorates });
  scenario.data.referenceGeography = await put('reference-geography', 'reference-test', [
    ...new Set(geography.targets.map(t => t.region))].map(code => ({ kind: 'region', code })).concat(
      [...new Set(geography.targets.map(t => t.province))].map(code => ({ kind: 'province', code })),
      geography.targets.map(t => ({ kind: 'municipality', code: t.code }))));
  const catalogue: ExecutionCatalogue = { componentIds: ['a', 'b', 'c', 'd'], pollIds: original.source.payload.polls.map(p => p.id),
    sources: original.source.payload.events.map(e => ({ id: e.id, questionId: 'q', choiceIds: original.source.payload.choices.filter(c => c.eventId === e.id).map(c => c.id), availableDate: e.availableDate })),
    locations, geographyRelations: relations };
  scenario.data.catalogue = await put('catalogue', 'test-catalogue', catalogue);
  for (const l of [ROSATELLUM, REFORM] as const) {
    const g = l === ROSATELLUM ? geography : original.accepted.assets.geography;
    scenario.data.lawAssets[l].geography = await put(`${l}-geography`, g.version, g);
    scenario.data.lawAssets[l].seats = await put(`${l}-seats`, l === ROSATELLUM ? ros.version : original.accepted.context.seats.version,
      l === ROSATELLUM ? ros : original.accepted.context.seats);
    for (const m of ['M01', 'M02'] as const) {
      const parameters: GenerationParameters = { schemaVersion: 1, kind: 'politics-generation-parameters', modelId: m, modelVersion: '2.0.0',
        models: original.accepted.context.models, rosatellumChamberProfileId: prior.chamberProfileId };
      scenario.data.lawAssets[l].models[m] = [await put(`${l}-${m}`, 'parameters-test', parameters)];
    }
  }
  const implementationId = await implementationKey(fileURLToPath(new URL('../../../', import.meta.url)), 'web/src/lib/workers/politics-compute.ts');
  const readers = { implementationId, asset: async (pin: Asset) => { const b = bytes.get(pin.id); if (!b) throw new Error(`Missing ${pin.id}`); return b; }, sourceChunk: original.read };
  return { scenario, readers, bytes, put, original, ros, geography, catalogue };
}
