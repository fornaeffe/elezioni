import { requireGenerationModel } from '../../src/lib/politics/reform-2026/vote-share/model-registry';
/** Curate only completed, current-bound acceptance; never accept missing receipts. */
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { decodeState } from "../../src/lib/politics/reform-2026/vote-share/state-codec";
import type {
  ReleaseCatalog,
  ScenarioState,
} from "../../src/lib/politics/reform-2026/vote-share/release";
import { readBytes } from "../data-preparation/fast-share-release-io";
import { requireBrowserAcceptance, requireUnitAcceptance } from "./reports";
requireGenerationModel({ model: 'M02', modelVersion: '1.0.0' }); // Historical: checkout 8dcb830/965f7ba.
const json = async (path: string) =>
  JSON.parse(await readFile(resolve(path), "utf8"));
const dir = resolve("static/data/reform-2026/fast-share");
const pointer = await json(resolve(dir, "candidate.json"));
const catalog = (
  await decodeState<ReleaseCatalog>(
    pointer.catalog,
    readBytes(dir),
    "fast-share-catalog",
  )
).state;
const defaults = (
  await decodeState<ScenarioState>(
    catalog.assets.scenario,
    readBytes(dir),
    "fast-share-scenario",
  )
).state;
const app = await json("../data/derived/r26-21-acceptance/report.json");
if (app.status !== "passed")
  throw new Error("Application acceptance is incomplete");
const units = requireUnitAcceptance(
  await json("../data/derived/r26-21-acceptance/unit.json"),
);
const browsers = [];
for (const name of [
  "browser-root",
  "browser-project",
  "unified-root",
  "unified-project",
]) {
  browsers.push({
    name,
    ...requireBrowserAcceptance(
      await json("../data/derived/r26-21-acceptance/" + name + ".json"),
    ),
  });
}
for (const base of ["root", "nested"])
  browsers.push({
    name: "editor-" + base,
    ...requireBrowserAcceptance(
      await json(
        "../data/derived/fv08-acceptance/browser/ui-" + base + ".json",
      ),
    ),
  });
const campaigns = [];
for (const model of ["M02", "M01"]) {
  const path =
    "../data/derived/fv08-unified/browser/chromium-" +
    model +
    "-root-1000.json.gz.receipt.json";
  const r = await json(path);
  if (
    r.generationImplementationId !== catalog.generationImplementationId ||
    r.draws !== 1000 ||
    r.runs.length !== 1000 ||
    r.pageErrors.length ||
    !r.fitFilePresent ||
    r.elapsedMs >= 3600000 ||
    r.runs.some(
      (v: any, i: number) =>
        v.simulation !== i + 1 ||
        v.status !== "complete" ||
        v.seats !== 588 ||
        v.distinctCandidates !== 588 ||
        v.camera !== 392 ||
        v.senato !== 196 ||
        v.unfilled !== 0,
    )
  )
    throw new Error("Incomplete or stale full campaign: " + model);
  campaigns.push({
    model,
    browserVersion: r.browserVersion,
    draws: r.draws,
    workers: r.workers,
    elapsedMs: r.elapsedMs,
    archiveBytes: r.archiveBytes,
    complete: 1000,
    filledSeatsPerRun: 588,
    distinctCandidatesPerRun: 588,
    pageErrors: r.pageErrors,
    exportImportAndRestore: "passed",
    peakRendererPrivateBytes:
      r.memory?.rendererProcesses?.peakPrivateBytes ??
      r.memory?.rendererProcess?.peakPrivateBytes,
    receipt: path,
  });
}
const refits = await json(
  "../data/derived/fv08-unified/resources/default-six-refits.json",
);
const maximum = await json(
  "../data/derived/fv08-unified/resources/maximum-worker.json",
);
if (
  refits.configuration.implementations.generation !==
    catalog.generationImplementationId ||
  refits.runs.length !== 6 ||
  refits.runs.some(
    (r: any) => r.phase !== "ready" || r.milliseconds >= 30000,
  ) ||
  maximum.runs.length !== 7 ||
  maximum.runs.some(
    (r: any) =>
      !r.ok ||
      r.workspaceBytes > 256 * 1024 ** 2 ||
      r.totalMs >= (r.rate === 4 ? 120000 : 30000),
  )
)
  throw new Error("Incomplete, stale or failed resource receipts");
const median = (values: number[]) => {
  values.sort((a, b) => a - b);
  const i = Math.floor(values.length / 2);
  return values.length % 2 ? values[i] : (values[i - 1] + values[i]) / 2;
};
const summary = {
  stage: "post-FV-08-correspondence-corrections",
  status: "passed",
  createdAt: new Date().toISOString(),
  scope:
    "Current component/prefiling v5 policy; local production acceptance, not predictive calibration or deployment.",
  catalogId: pointer.catalogId,
  sourceId: catalog.sourceId,
  generationImplementationId: catalog.generationImplementationId,
  referenceDate: defaults.scenario.referenceDate,
  electionDate: defaults.scenario.electionDate,
  defaultReferenceEstimates: (defaults.defaultPolicy as Record<string, unknown>)
    .unsupportedReferenceEstimates,
  units,
  browsers,
  campaigns,
  resources: {
    defaultRefits: 6,
    medianRefitMs: median(refits.runs.map((r: any) => r.milliseconds)),
    maximumDimensionRuns: 7,
    medianMaximumMs: median(
      maximum.runs.filter((r: any) => r.rate === 1).map((r: any) => r.totalMs),
    ),
    quarterCpuMs: maximum.runs.find((r: any) => r.rate === 4).totalMs,
    maximumObservedWorkspaceBytes: Math.max(
      ...maximum.runs.map((r: any) => r.workspaceBytes),
    ),
    workspaceLimitBytes: 256 * 1024 ** 2,
  },
  applicationChecks: app.checks,
};
const target = resolve(
  "../test/fixtures/benchmarks/correspondence_corrections_acceptance.json",
);
await writeFile(target, JSON.stringify(summary, null, 2) + "\n");
console.log(
  JSON.stringify({
    target,
    catalog: summary.catalogId,
    units,
    browsers,
    campaigns: campaigns.map(({ model, draws }) => ({ model, draws })),
  }),
);
