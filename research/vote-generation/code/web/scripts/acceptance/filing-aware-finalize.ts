/** Curate the completed browser measurement without accepting wider FV-08 gates. */
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import { cpus } from "node:os";
import {
  implementationKey,
  readBytes,
} from "../data-preparation/fast-share-release-io";
import { loadCandidate } from "../../src/lib/politics/reform-2026/vote-share/release";
const root = resolve(".."),
  base = resolve(root, "data/derived/filing-aware-research"),
  directory = resolve("static/data/reform-2026/fast-share");
const bytes = await readFile(
    resolve(base, "browser/production-512-bounded-mass-pair-field-v2.json"),
  ),
  browser = JSON.parse(bytes.toString()),
  r = browser.result,
  release = JSON.parse(
    await readFile(resolve(base, "selected-release.json"), "utf8"),
  ),
  pointer = JSON.parse(
    await readFile(resolve(directory, "candidate.json"), "utf8"),
  ),
  generation = await implementationKey(
    root,
    "web/src/lib/politics/reform-2026/vote-share/generation.ts",
  ),
  loaded = await loadCandidate(pointer, readBytes(directory), {
    expectedGenerationImplementationId: generation,
    validateGeneration: false,
  });
if (
  browser.request.implementations.generation !== generation ||
  release.catalog !== pointer.catalogId ||
  r.model !== "bounded-mass-pair-field-v2" ||
  r.scale !== 1.5 ||
  r.profileRegionCorrelation !== 0.25
)
  throw new Error("Browser/current candidate provenance mismatch");
if (
  !r.ok ||
  r.requested !== 512 ||
  r.completed !== 512 ||
  r.failed !== 0 ||
  r.nonComputable !== 0 ||
  r.milliseconds >= 1800000 ||
  r.ledger.length !== 512 ||
  r.ledger.some(
    (row: any, i: number) =>
      row.id !== i + 1 || row.seats !== 588 || row.filled !== 588,
  )
)
  throw new Error("Full browser computation did not pass");
if (
  loaded.scenario.lists.camera.length !== 24 ||
  loaded.scenario.lists.senato.length !== 24 ||
  loaded.scenario.models.profileRegionCorrelation !== 0.25 ||
  loaded.scenario.models.forecast !== "bounded-mass-pair-field-v2"
)
  throw new Error("Unexpected future scenario");
// Engineering failures are preserved evidence, not inputs which a corrected
// rerun should recreate. Reuse the tracked record when its ignored log is absent.
const destination = resolve(
  root,
  "test/fixtures/benchmarks/vote_share_feasible_browser.json",
);
const retained = JSON.parse(await readFile(destination, "utf8"));
let attempts = retained.interruptedHistoricalAttempts;
try {
  attempts = JSON.parse(
    await readFile(
      resolve(base, "full/modular-939576fd812f/attempts.json"),
      "utf8",
    ),
  );
} catch (error) {
  if ((error as NodeJS.ErrnoException).code !== "ENOENT") throw error;
}
const workspace = [];
let preCorrection = retained.preCorrectionWorkspaceCheck;
if (!preCorrection) {
  const beforeFixBytes = await readFile(
      resolve(root, "data/derived/fv08-acceptance/browser/ui-nested.json"),
    ),
    beforeFix = JSON.parse(beforeFixBytes.toString());
  preCorrection = {
    sha256: createHash("sha256").update(beforeFixBytes).digest("hex"),
    stats: beforeFix.stats,
    observedPeakBytes: 618286979,
    qualification:
      "Pre-correction nested check failed the original memory assertion and exposed an archive-import metadata error. Retained as a failed engineering check; corrected functional runs do not erase it.",
  };
}
for (const name of [
  "ui-root",
  "ui-nested",
  "chromium-root",
  "chromium-nested",
  "firefox-root",
  "firefox-nested",
]) {
  const raw = await readFile(resolve(base, "browser", name + ".json")),
    value = JSON.parse(raw.toString());
  if (name.startsWith("ui-")) {
    if (
      value.stats.unexpected !== 0 ||
      value.stats.skipped !== 0 ||
      value.stats.expected !== 8
    )
      throw new Error("Incomplete functional workspace checks: " + name);
    workspace.push({
      name,
      sha256: createHash("sha256").update(raw).digest("hex"),
      stats: value.stats,
    });
  } else {
    const { rows, ...memory } = value.memory;
    workspace.push({
      name,
      sha256: createHash("sha256").update(raw).digest("hex"),
      catalogId: value.catalogId,
      browserVersion: value.browserVersion,
      memory,
      pageErrors: value.pageErrors,
    });
  }
}
await writeFile(
  destination,
  JSON.stringify(
    {
      schemaVersion: 1,
      scope:
        "Measured single-worker default refit plus 512 paired generation/legal runs; wider FV-08 production gates remain open",
      recordedAt: new Date().toISOString(),
      environment: { node: process.version, cpu: cpus()[0].model },
      browserReportSha256: createHash("sha256").update(bytes).digest("hex"),
      catalogAtRun: pointer.catalogId,
      catalogQualification:
        "Local pointer held fixed during measurement; expected generation fingerprint and final selected release verified against the request.",
      generationImplementationId: generation,
      release,
      browser,
      workspace,
      preCorrectionWorkspaceCheck: preCorrection,
      workspaceMemoryQualification:
        "Original FV-08 512 MiB threshold is retained as a diagnostic for this authorized replacement comparison, not accepted by passing functional tests. Runtime reservation policy is unchanged.",
      interruptedHistoricalAttempts: attempts,
      executionNotes: [
        "Project-path replay exposed fractional estimatedWorkerMemoryMiB metadata, which failed the existing result importer. The producer now rounds its MiB estimate up; workspace tests validate the actual exported payload and require absence of result-service errors. The forecast implementation and its measured 512-run fingerprint are unchanged.",
        "Expanded-default browser fixtures were updated from a hard-coded ten-share poll and an unconditional two-consumer request. The real packet permits one consumer under the unchanged budget; the test now requires unsafe parallelism to be rejected and checks identity within supported scheduling.",
        "Browser harness development corrected action load-default to load and marked transparently decompressed HTTP asset bodies as transport:http. These initial harness failures are not successful model requests.",
        "The first full-2022 artifact attempt was interrupted before reporting to replace a potentially oversized JSON string with streaming gzip. Its orphan child process was identified and stopped. The completed retry uses identical inputs, parameters and frozen seeds; no statistical reroll.",
        "A full parallel unit run hit the unchanged five-second reform-compute timeout under concurrent research/browser load. The targeted repeat and complete bounded-worker suite passed.",
        "Future-default integration assertions found malformed UTF-8 display labels and an expected-ID ordering error; authored labels/assertions were corrected and defaults regenerated before the full browser measurement.",
        "Raw predictive arrays remain ignored; earlier failed global-Beta evidence is retained separately and never overwritten by current-catalog production diagnostics.",
      ],
    },
    null,
    2,
  ) + "\n",
);
console.log(
  JSON.stringify({
    catalog: pointer.catalogId,
    minutes: r.milliseconds / 60000,
    requested: r.requested,
    failed: r.failed,
    nonComputable: r.nonComputable,
  }),
);
