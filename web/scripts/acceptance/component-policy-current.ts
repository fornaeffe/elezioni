/** Current production check; never rewrites the frozen pre-correction audit. */
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve } from "node:path";
import {
  loadCandidate,
  loadCandidateSource,
} from "../../src/lib/politics/reform-2026/vote-share/release";
import { requireReferenceCoverage } from "../../src/lib/politics/reform-2026/vote-share/component-policy";
import { physicalCells } from "../../src/lib/politics/reform-2026/vote-share/physical-cells";
import { compileLegacy } from "../../src/lib/politics/reform-2026/vote-share/legacy-adapter";
import { prepareScenarioGeneration } from "../../src/lib/politics/reform-2026/vote-share/unified-generation";
import { runPoliticsReformScrutiny } from "../../src/lib/politics/reform-2026/scrutiny";
import { fileURLToPath } from "node:url";
import {
  readBytes,
  implementationKey,
} from "../data-preparation/fast-share-release-io";
const directory = resolve(
    process.argv[2] ?? "static/data/reform-2026/fast-share",
  ),
  pointer = JSON.parse(
    await readFile(resolve(directory, "candidate.json"), "utf8"),
  ),
  read = readBytes(directory),
  loaded = await loadCandidate(pointer, read, {
    expectedGenerationImplementationId: await implementationKey(
      fileURLToPath(new URL("../../../", import.meta.url)),
      "web/src/lib/politics/reform-2026/vote-share/unified-generation.ts",
    ),
  }),
  source = await loadCandidateSource(loaded.catalog, read),
  output = resolve("../data/derived/correspondence-corrections");
await mkdir(output, { recursive: true });
const receipt = {
  catalog: pointer.catalogId,
  created: new Date().toISOString(),
  models: [] as object[],
  failures: [] as object[],
};
for (const model of ["selected", "legacy-poll-informed-v1"] as const) {
  const s = structuredClone(loaded.scenario);
  s.generation!.model = model;
  const started = performance.now(),
    legacy =
      model === "selected"
        ? undefined
        : await compileLegacy(s, source, read, loaded.assets, loaded.context),
    runs = [] as object[];
  if (legacy) {
    const cells = {
      camera: physicalCells(
        loaded.assets.geography,
        loaded.assets.electorates,
        ["camera"],
        s.seed,
      ),
      senato: physicalCells(
        loaded.assets.geography,
        loaded.assets.electorates,
        ["senato"],
        s.seed,
      ),
    };
    const missing = [] as string[];
    for (const id of legacy.requiredReferences)
      try {
        requireReferenceCoverage(s, cells, [id]);
      } catch {
        missing.push(id);
      }
    if (missing.length) {
      console.log(JSON.stringify({ missingReferences: missing }));
      throw new Error("DEFAULT_REFERENCE_INPUTS_REQUIRED");
    }
  }
  const prepared = prepareScenarioGeneration(
    s,
    loaded.assets,
    loaded.context,
    legacy,
  );
  for (let id = 1; id <= 3; id++) {
    try {
      const generated = prepared.run(id),
        result = runPoliticsReformScrutiny(generated.input),
        failed = result.stages.filter((s) => s.status === "non-computable");
      runs.push({
        id,
        status: result.status,
        failed,
        support: generated.trace.componentSupport,
      });
      if (result.status === "non-computable" || failed.length)
        receipt.failures.push({ model, id, failed });
    } catch (error) {
      receipt.failures.push({ model, id, error: String(error) });
    }
  }
  receipt.models.push({
    model,
    elapsedMs: performance.now() - started,
    runs,
    legacyAudit: legacy?.audit,
  });
}
await writeFile(
  resolve(output, "node-summary.json"),
  JSON.stringify(receipt, null, 2) + "\n",
);
console.log(
  JSON.stringify({
    catalog: receipt.catalog,
    models: receipt.models.map((m) => ({
      model: (m as { model: string }).model,
    })),
    failures: receipt.failures,
  }),
);
if (receipt.failures.length) throw new Error("CURRENT_COMPONENT_POLICY_FAILED");
