/** Small initial payload only. Fitted arrays and original counts are not fetched here. */
import { runtimeDataUrl } from '../../../core/runtime-data';
import { decodeState } from './state-codec';
import type {
  CandidatePointer,
  ReleaseCatalog,
  ScenarioState,
} from './release';
import { requireFastScenario } from './scenario';
export async function loadEditorDefault(base: string, signal?: AbortSignal) {
  const root = 'data/reform-2026/fast-share/';
  const response = await fetch(runtimeDataUrl(base, root + 'candidate.json'), {
    signal,
    cache: 'no-cache',
  });
  if (!response.ok)
    throw new Error(
      'Catalogo candidato non disponibile: HTTP ' + response.status,
    );
  const pointer = (await response.json()) as CandidatePointer;
  if (
    pointer.kind !== 'fast-share-candidate' ||
    pointer.schemaVersion !== 1 ||
    pointer.assetRoot !== root
  )
    throw new Error('Catalogo candidato non supportato');
  const read = async (path: string) => {
    if (!/^[\w.-]+$/.test(path)) throw new Error('Invalid asset path');
    const r = await fetch(runtimeDataUrl(base, root + path), { signal });
    if (!r.ok) throw new Error('Dati non disponibili: HTTP ' + r.status);
    return {
      transport: 'http' as const,
      body: new Uint8Array(await r.arrayBuffer()),
    };
  };
  const catalog = await decodeState<ReleaseCatalog>(
    pointer.catalog,
    read,
    'fast-share-catalog',
  );
  if (catalog.envelope.id !== pointer.catalogId)
    throw new Error('Catalog identity mismatch');
  const state = await decodeState<ScenarioState>(
    catalog.state.assets.scenario,
    read,
    'fast-share-scenario',
  );
  if (state.envelope.id !== catalog.state.scenarioId)
    throw new Error('Scenario identity mismatch');
  return { pointer, scenario: requireFastScenario(state.state.scenario) };
}
