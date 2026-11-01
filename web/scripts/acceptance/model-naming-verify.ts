/** Naming must not change sampled ballots or candidates. Frozen history stays untouched. */
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import {
  loadCandidate,
  loadCandidateSource,
} from "../../src/lib/politics/reform-2026/vote-share/release";
import { prepareScenarioGeneration } from "../../src/lib/politics/reform-2026/vote-share/unified-generation";
import { compileLegacy } from "../../src/lib/politics/reform-2026/vote-share/legacy-adapter";
import { generationModels } from "../../src/lib/politics/reform-2026/vote-share/correspondences";
import {
  readBytes,
  implementationKey,
} from "../data-preparation/fast-share-release-io";
const dir = resolve("static/data/reform-2026/fast-share"),
  read = readBytes(dir);
const pointer = JSON.parse(
  await readFile(resolve(dir, "candidate.json"), "utf8"),
);
const loaded = await loadCandidate(pointer, read, {
  expectedGenerationImplementationId: await implementationKey(
    resolve(".."),
    "web/src/lib/politics/reform-2026/vote-share/unified-generation.ts",
  ),
});
const source = await loadCandidateSource(loaded.catalog, read),
  results: Record<string, string> = {};
function strip(v: any): any {
  if (Array.isArray(v)) return v.map(strip);
  if (v && typeof v === "object")
    return Object.fromEntries(
      Object.entries(v)
        .filter(([k]) => k !== "provenance")
        .map(([k, x]) => [k, strip(x)]),
    );
  return v;
}
for (const item of generationModels) {
  const model = String(item.id),
    family =
      model === "M01" || model === "legacy-poll-informed-v1" ? "M01" : "M02";
  const s = structuredClone(loaded.scenario);
  s.generation!.model = item.id;
  if ("version" in item) (s.generation as any).modelVersion = item.version;
  const legacy =
    family === "M01"
      ? await compileLegacy(s, source, read, loaded.assets, loaded.context)
      : undefined;
  const prepared = prepareScenarioGeneration(
    s,
    loaded.assets,
    loaded.context,
    legacy,
  );
  for (const id of [1, 7]) {
    const { input } = prepared.run(id);
    results[family + ":" + id] = createHash("sha256")
      .update(
        JSON.stringify(
          strip({
            renewal: input.renewal,
            candidates: input.candidates,
            bonusLinks: input.bonusLinks,
          }),
        ),
      )
      .digest("hex");
  }
}
const target = resolve(
  "../data/derived/correspondence-corrections/model-naming-baseline.json",
);
if (process.argv.includes("--baseline"))
  await writeFile(target, JSON.stringify(results, null, 2) + "\n");
else if (
  JSON.stringify(JSON.parse(await readFile(target, "utf8"))) !==
  JSON.stringify(results)
)
  throw new Error("MODEL_NAMING_CHANGED_GENERATION");
console.log(
  JSON.stringify({
    stage: process.argv.includes("--baseline") ? "baseline" : "equal",
    results,
  }),
);
