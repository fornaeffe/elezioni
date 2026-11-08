/** Decoded, checksum-pinned preparation assets. Packaging/default construction is CP-04. */
import type { Asset, PoliticsScenario } from '../scenario/schema';
import { requiredAssets, verifyAssetBytes } from '../scenario/dependencies';
import type { DomainContext } from '../scenario/validation';
import type { CompactSource, SourceGeography } from '../reform-2026/vote-share/source-types';
import type { AssetBytes } from '../reform-2026/vote-share/source-codec';
import { validateSourceEnvelope } from '../reform-2026/vote-share/source-codec';
import type { TargetElectorate } from '../reform-2026/vote-share/forecast-types';
import type { ReformModelBundle } from '../reform-2026/generation-types';
import type { SeatTableBundle } from '../reform-2026/endowments';
import type { RosatellumSeats } from './rosatellum';
import { activeGenerator } from '../scenario/registry';
import { REFORM_MODEL_IDS } from '../reform-2026/generation-types';

export interface GenerationParameters {
  schemaVersion: 1;
  kind: 'politics-generation-parameters';
  modelId: 'M01' | 'M02';
  modelVersion: '2.0.0';
  models: ReformModelBundle;
  /** Rosatellum's pinned statistical profile; no dependency on inactive reform settings. */
  rosatellumChamberProfileId: string;
}
export type ExecutionCatalogue = Pick<DomainContext, 'componentIds' | 'sources' | 'pollIds' | 'locations' | 'geographyRelations'>;
export interface ExecutionElectorate {
  schemaVersion: 1;
  kind: 'politics-electorate';
  minimumAge: 18;
  targets: TargetElectorate[];
}
export interface ExecutionReaders {
  /** Node callers provide the actual transitive code hash; browser builds verify their compiled identity. */
  implementationId: string;
  /** Exact decoded bytes, after any transport decompression. */
  asset(pin: Asset): Promise<Uint8Array<ArrayBuffer>>;
  /** Existing original-column chunks retain their compressed/HTTP integrity contract. */
  sourceChunk(path: string): Promise<AssetBytes>;
}
export async function loadExecutionData(s: PoliticsScenario, readers: ExecutionReaders) {
  const loaded = new Map<string, unknown>();
  for (const pin of requiredAssets(s)) {
    if (pin.schemaVersion !== 1) throw new Error(`UNSUPPORTED_EXECUTION_ASSET_SCHEMA: ${pin.id}@${pin.schemaVersion}`);
    const bytes = await readers.asset(pin);
    await verifyAssetBytes(pin, bytes);
    loaded.set(pin.id, JSON.parse(new TextDecoder('utf-8', { fatal: true }).decode(bytes)));
  }
  const get = <T>(a: Asset) => loaded.get(a.id) as T;
  const law = s.data.lawAssets[s.selection.scrutiny.id], model = activeGenerator(s);
  if (law.models[model.id].length !== 1) throw new Error('MODEL_ASSET_CONTRACT: supply one exact generation-parameters bundle');
  const parameters = get<GenerationParameters>(law.models[model.id][0]);
  if (parameters.kind !== 'politics-generation-parameters' || parameters.schemaVersion !== 1 ||
    parameters.modelId !== model.id || parameters.modelVersion !== model.version || parameters.models?.schemaVersion !== 1 ||
    Object.entries(REFORM_MODEL_IDS).some(([key, id]) => parameters.models.modelIds?.[key as keyof typeof REFORM_MODEL_IDS] !== id))
    throw new Error('MODEL_ASSET_CONTRACT: model/version mismatch');
  const source = get<CompactSource>(s.data.historical);
  await validateSourceEnvelope(source);
  const geography = get<SourceGeography>(law.geography);
  if (source.id !== s.data.historical.version || geography.version !== law.geography.version)
    throw new Error('DATA_VERSION_MISMATCH: source/geography pins');
  const catalogue = get<ExecutionCatalogue>(s.data.catalogue);
  const electorate = get<ExecutionElectorate>(s.data.electorate);
  if (electorate.kind !== 'politics-electorate' || electorate.schemaVersion !== 1 || electorate.minimumAge !== 18 || !Array.isArray(electorate.targets))
    throw new Error('ELECTORATE_CONTRACT: explicit common-18+ conditional targets required');
  const domain: DomainContext = { ...catalogue,
    referenceGeography: get<DomainContext['referenceGeography']>(s.data.referenceGeography),
    availableAssets: requiredAssets(s) };
  return { source, geography, domain, parameters,
    electorates: electorate.targets,
    seats: get<SeatTableBundle | RosatellumSeats>(law.seats) };
}
export type ExecutionData = Awaited<ReturnType<typeof loadExecutionData>>;
