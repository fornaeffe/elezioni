/** Current implementation-bound reduced frozen state for synthetic UI evidence. */
import { resolve } from "node:path";
import { implementationKey } from "../../scripts/data-preparation/fast-share-release-io";
import { loadedFitFixture } from "./fast-fit-input";
import { createCheckpoint } from "../../src/lib/politics/reform-2026/vote-share/checkpoint";
import type { PortableFitFile } from "../../src/lib/politics/reform-2026/vote-share/checkpoint";
let cached: Promise<PortableFitFile> | undefined;
export function currentBrowserSession() {
  return (cached ??= (async () => {
    const implementation = await implementationKey(
      resolve(".."),
      "web/src/lib/politics/reform-2026/vote-share/unified-generation.ts",
    );
    const f = await loadedFitFixture(implementation);
    return createCheckpoint(f.accepted);
  })());
}
export function currentArchive<T extends { result: { benchmark: object } }>(
  payload: T,
  fitFile: PortableFitFile,
) {
  return {
    ...payload,
    schema_version: 3,
    scenario: fitFile.scenario,
    fitFile,
    result: {
      ...payload.result,
      benchmark: {
        ...payload.result.benchmark,
        dataVersion: fitFile.scenario.data.sourceBundleId,
        seed: fitFile.scenario.seed,
        scrutinyAlgorithmId: fitFile.scenario.algorithmId,
      },
    },
  };
}
