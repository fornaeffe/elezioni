/** Scenario-independent production bundles. No default choices or latest-bundle resolution. */
import { readFile } from 'node:fs/promises';
import { resolve } from 'node:path';
import { readManifest } from '../data-preparation/normalize';
import { readDefinitiveGeography } from '../data-preparation/definitive-geography';
import { prepareWorkspaceGeography } from '../data-preparation/workspace-geography';
import { canonicalJson, declaredAssets, verifyAssetBytes } from '../../src/lib/politics/scenario/dependencies';
import { chambers, REFORM, ROSATELLUM, type Asset, type Location, type PoliticsScenario } from '../../src/lib/politics/scenario/schema';
import type { ExecutionCatalogue, GenerationParameters, RosatellumSeats } from '../../src/lib/politics/execution';
import type { releaseInputs } from '../data-preparation/fast-share-release-inputs';
import { decodeSourceRows, byteHash } from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import type { SourceGeography } from '../../src/lib/politics/reform-2026/vote-share/source-types';
import { replaceGenerated } from '../data-preparation/workspace-snapshots';

export async function buildPoliticsAssets(root: string, sources: string, inputs: Awaited<ReturnType<typeof releaseInputs>>) {
  const source = inputs.source, manifest = await readManifest(root), geo = await readDefinitiveGeography(manifest, sources);
  const snapshot = JSON.parse(await readFile(resolve(root, 'web/static/data/v1/politics-static.json'), 'utf8'));
  const electors = new Map<string, number>();
  for (const r of snapshot.data.comuni_liste_elezioni) if (r.ELEZIONE === 'europee 2024')
    electors.set(String(r.CODICE_COMUNE), (electors.get(String(r.CODICE_COMUNE)) ?? 0) + r.VOTI);
  const oldGeo = await prepareWorkspaceGeography(geo, manifest, sources, electors);
  const ros: RosatellumSeats = { schemaVersion: 1, kind: 'rosatellum-seats', version: `${oldGeo.version}-seats`, geographyVersion: oldGeo.version,
    chambers: chambers.map(chamber => ({ chamber, totalSeats: chamber === 'camera' ? 392 : 196,
      uninominal: oldGeo[chamber].uni, plurinominal: oldGeo[chamber].pluri.map(p => {
        if (typeof p.MAX_CANDIDATI !== 'number') throw new Error('Missing prepared candidate capacity');
        return { ...p, MAX_CANDIDATI: p.MAX_CANDIDATI };
      }) })),
    ballots: chambers.flatMap(chamber => oldGeo[chamber].uni.map(u => ({ id: `ros:${chamber}:${u.UNI_COD}`, chamber,
      uninominal: String(u.UNI_COD), plurinominal: u.PLURI_COD === null ? null : String(u.PLURI_COD), constituency: String(u.CIRC_COD),
      regionId: String(chamber === 'camera' ? Math.floor(Number(u.CIRC_COD) / 100) : u.CIRC_COD).padStart(2, '0') }))) };
  const rosGeo: SourceGeography = { ...source.payload.geography, version: oldGeo.version, cells: oldGeo.cells,
    colleges: ros.ballots.map(b => ({ id: b.id, chamber: b.chamber, kind: 'ordinary', territoryId: b.constituency })) };
  const locations: Location[] = [], geographyRelations: ExecutionCatalogue['geographyRelations'][number][] = [];
  const loc = (law: typeof REFORM | typeof ROSATELLUM, chamber: 'camera' | 'senato', level: Location['level'], code: string, version: string) => {
    const row = { law, chamber, level, code, geographyVersion: version };
    if (!locations.some(l => canonicalJson(l) === canonicalJson(row))) locations.push(row);
    return row;
  };
  for (const b of ros.ballots) {
    const uni = loc(ROSATELLUM, b.chamber, 'uninominal', b.uninominal, rosGeo.version);
    if (b.plurinominal !== null) geographyRelations.push({ parent: loc(ROSATELLUM, b.chamber, 'plurinominal', b.plurinominal, rosGeo.version), child: uni });
  }
  const context = inputs.legacy.context;
  for (const c of context.seats.geography.colleges) geographyRelations.push({
    parent: loc(REFORM, c.chamber, 'territory', c.territoryId, source.payload.geography.version),
    child: loc(REFORM, c.chamber, 'college', c.id, source.payload.geography.version) });
  const catalogue: ExecutionCatalogue = {
    componentIds: [...new Set([...source.payload.choices.flatMap(c => c.members), ...source.payload.polls.flatMap(p => p.normalized.flatMap(g => g.members))])].sort(),
    sources: [...source.payload.events.map(e => ({ id: e.id, questionId: 'ballot', availableDate: e.availableDate,
      choiceIds: source.payload.choices.filter(c => c.eventId === e.id).map(c => c.id) })),
      ...source.payload.polls.map(p => ({ id: p.id, questionId: 'ballot', availableDate: p.availableDate,
        choiceIds: p.normalized.map(g => `poll:${p.id}:${g.label}`) }))],
    pollIds: source.payload.polls.map(p => p.id), locations, geographyRelations };
  const camera = source.payload.events.find(e => e.id === 'camera-2022')!;
  const rows = await decodeSourceRows(source, camera.id, await inputs.read(camera.chunk.path));
  const units = new Map(source.payload.units.map(u => [u.id, u]));
  const byCode = new Map(rows.map(r => [units.get(r.unit)!.targetCode, r.electors]));
  const electorate = { schemaVersion: 1, kind: 'politics-electorate', minimumAge: 18,
    targets: source.payload.geography.targets.map(t => {
      const n = byCode.get(t.code); if (!n) throw new Error(`Missing conditional 18+ electorate ${t.code}`);
      return { code: t.code, date: camera.date, camera: n, senato: n, provenance: 'Conditional common 18+ Camera 2022 electorate, held equal across chambers; not a demographic forecast.' };
    }) };
  const values: Record<string, { version: string; value: unknown }> = {
    historical: { version: source.id, value: source },
    catalogue: { version: 'politics-catalogue-v1', value: catalogue },
    'reference-geography': { version: 'istat-census-2021-v1', value: [
      ...[...new Set(geo.municipalities.map(m => m.regionId))].map(code => ({ kind: 'region', code })),
      ...[...new Set(geo.municipalities.map(m => m.provinceId))].map(code => ({ kind: 'province', code })),
      ...geo.municipalities.map(m => ({ kind: 'municipality', code: m.code }))] },
    electorate: { version: 'conditional-camera-2022-common-18-v1', value: electorate },
    'ros-geography': { version: rosGeo.version, value: rosGeo }, 'ros-seats': { version: ros.version, value: ros },
    'reform-geography': { version: source.payload.geography.version, value: source.payload.geography },
    'reform-seats': { version: context.seats.version, value: context.seats },
  };
  for (const modelId of ['M01', 'M02'] as const) {
    const value: GenerationParameters = { schemaVersion: 1, kind: 'politics-generation-parameters', modelId, modelVersion: '2.0.0',
      models: context.models, rosatellumChamberProfileId: 'central-v1' };
    values[modelId] = { version: `${modelId}-2.0.0/${context.models.version}`, value };
  }
  const assets = new Map<string, { pin: Asset; bytes: Uint8Array<ArrayBuffer>; value: unknown }>();
  for (const [id, { version, value }] of Object.entries(values)) {
    const bytes = new TextEncoder().encode(canonicalJson(value)), sha256 = await byteHash(bytes);
    assets.set(id, { pin: { id, version, schemaVersion: 1, bytes: bytes.length, sha256,
      uri: `data/politics/${id}-${sha256.slice(0, 12)}.json` }, bytes, value });
  }
  return { assets, catalogue, ros, rosGeo, electorate };
}
export async function validateBuiltPoliticsAssets(scenario: PoliticsScenario, built: Awaited<ReturnType<typeof buildPoliticsAssets>>) {
  for (const pin of new Map(declaredAssets(scenario).map(a => [a.id, a])).values()) {
    const asset = built.assets.get(pin.id);
    if (!asset || canonicalJson(asset.pin) !== canonicalJson(pin)) throw new Error(`INCOMPATIBLE_AUTHORED_PIN: ${pin.id}; review data/politics-default-scenario.json`);
    await verifyAssetBytes(pin, asset.bytes);
  }
}
export async function packagePoliticsAssets(root: string, scenario: PoliticsScenario, built: Awaited<ReturnType<typeof buildPoliticsAssets>>, check: boolean) {
  await validateBuiltPoliticsAssets(scenario, built);
  for (const pin of new Map(declaredAssets(scenario).map(a => [a.id, a])).values()) {
    const asset = built.assets.get(pin.id)!;
    const path = resolve(root, 'web/static', pin.uri);
    if (check) await verifyAssetBytes(pin, new Uint8Array(await readFile(path)));
    else await replaceGenerated(path, asset.bytes);
  }
}
