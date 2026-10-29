import {
  canonicalJson,
  decodeSourceRows,
  sourceKey,
} from '../../src/lib/politics/reform-2026/vote-share/source-codec';
import { compilePairedHistory } from '../../src/lib/politics/reform-2026/vote-share/paired-source';
import {
  loadCandidate,
  loadCandidateSource,
  type CandidatePointer,
} from '../../src/lib/politics/reform-2026/vote-share/release';
import { compileSource } from '../../src/lib/politics/reform-2026/vote-share/source-compiler';
import { runPoliticsReformScrutiny } from '../../src/lib/politics/reform-2026/scrutiny';
import { readBytes, implementationKey } from './fast-share-release-io';
import { fileURLToPath } from 'node:url';
/** Every production asset is required, including lazily loaded refit chunks. */
export async function verifyShareRelease(
  pointer: CandidatePointer,
  directory: string,
) {
  const read = readBytes(directory),
    loaded = await loadCandidate(pointer, read, {
      expectedGenerationImplementationId: await implementationKey(
        fileURLToPath(new URL('../../../', import.meta.url)),
        'web/src/lib/politics/reform-2026/vote-share/generation.ts',
      ),
    }),
    source = await loadCandidateSource(loaded.catalog, read);
  const required = [
    ...loaded.scenario.historySelection.electionIds,
    'senato-2018',
    'senato-2022',
  ].sort();
  if (
    canonicalJson(source.payload.events.map((e) => e.id).sort()) !==
      canonicalJson(required) ||
    source.payload.geography.targets.length !==
      loaded.context.seats.geography.municipalityCount
  )
    throw new Error('RELEASE_INCOMPLETE_SOURCE_DOMAIN');
  for (const e of source.payload.events)
    await decodeSourceRows(source, e.id, await read(e.chunk.path));
  const compiled = await compileSource(source, read, loaded.selection);
  const paired = await compilePairedHistory(source, read, loaded.selection, {
    camera: loaded.scenario.historySelection.pairedCamera,
    senato: loaded.scenario.historySelection.pairedSenato,
    electoratePolicy: 'equal-only',
  });
  if (
    (await sourceKey('paired-input-v1', {
      date: paired.history.date,
      availableDate: paired.history.availableDate,
      minimumAge: paired.history.minimumAge,
      rows: paired.history.rows,
    })) !== loaded.catalog.pairedInputKey
  )
    throw new Error('RELEASE_PAIRED_INPUT_MISMATCH');
  if (
    compiled.nationalInputKey !== loaded.catalog.nationalInputKey ||
    compiled.profileInputKey !== loaded.catalog.profileInputKey ||
    canonicalJson(loaded.selection.components) !==
      canonicalJson(loaded.assets.fit.componentIds)
  )
    throw new Error('RELEASE_COMPILED_INPUT_MISMATCH');
  const result = loaded.prepared!.run(1),
    scrutiny = runPoliticsReformScrutiny(result.input);
  if (
    scrutiny.status === 'non-computable' ||
    scrutiny.stages.some((s) => s.status === 'non-computable')
  )
    throw new Error(
      'DEFAULT_LEGAL_NON_COMPUTABLE:' +
        JSON.stringify(
          scrutiny.stages.filter((s) => s.status === 'non-computable'),
        ),
    );
  return {
    catalogId: pointer.catalogId,
    sourceId: source.id,
    referenceDate: loaded.scenario.referenceDate,
    electionDate: loaded.scenario.electionDate,
    municipalities: source.payload.geography.targets.length,
    physicalCells:
      loaded.prepared!.cells.camera.length +
      loaded.prepared!.cells.senato.length,
    fittedComponents: loaded.assets.fit.componentIds.length,
    futureComponents: loaded.scenario.components.length,
    politicalDrawsPerChain: loaded.assets.fit.political.drawsPerChain,
    politicalDiagnostics: loaded.assets.fit.political.diagnostics,
    participationDiagnostics: loaded.assets.fit.participation.diagnostics,
    projectionResidual: result.audit.projectionResidual,
    legalStatus: scrutiny.status,
    seats: scrutiny.seats.length,
    filled: scrutiny.seats.filter((s) => s.status === 'filled').length,
    sourceIntegrity: true,
    matchingSemanticKeys: true,
    defaultStartFits: 0,
  };
}
