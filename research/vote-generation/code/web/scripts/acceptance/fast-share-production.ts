/** Frozen-seed full-country preflight. Does not substitute for browser storage acceptance. */
import { readFile, mkdir, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import {
  loadCandidate,
  type CandidatePointer,
} from "../../src/lib/politics/reform-2026/vote-share/release";
import {
  readBytes,
  implementationKey,
} from "../data-preparation/fast-share-release-io";
import { prepareFastGeneration } from "../../src/lib/politics/reform-2026/vote-share/generation";
import { runPoliticsReformScrutiny } from "../../src/lib/politics/reform-2026/scrutiny";
import { ProjectionCapacityError } from "../../src/lib/politics/reform-2026/vote-share/projection";
const diagnostic = process.argv[2] === "capacity",
  count = diagnostic ? 2 : Number(process.argv[2] ?? 10);
if (!diagnostic && ![10, 1000].includes(count))
  throw new Error(
    "Use 10 (pilot), 1000 (full preflight), or capacity (diagnose frozen failures 33/45).",
  );
const directory = resolve("static/data/reform-2026/fast-share"),
  pointer: CandidatePointer = JSON.parse(
    await readFile(resolve(directory, "candidate.json"), "utf8"),
  ),
  loaded = await loadCandidate(pointer, readBytes(directory), {
    expectedGenerationImplementationId: await implementationKey(
      resolve(".."),
      "web/src/lib/politics/reform-2026/vote-share/generation.ts",
    ),
  }),
  output = loaded.scenario.models.forecast
    ? resolve(
        "../data/derived/filing-aware-research/production",
        pointer.catalogId,
      )
    : resolve("../data/derived/fv08-acceptance/production");
await mkdir(output, { recursive: true });
const report = {
  stage: "FV-08",
  startedAt: new Date().toISOString(),
  node: process.version,
  scope: diagnostic
    ? "Diagnostic repeats of frozen failures 33/45 only; never replaces the original production ledger"
    : count === 1000
      ? "full-country generation/legal preflight; browser storage/export acceptance remains separate"
      : "ten-run resource pilot; not production acceptance",
  catalogId: pointer.catalogId,
  requested: count,
  completed: 0,
  technicalErrors: 0,
  nonComputable: 0,
  elapsedMs: 0,
  runs: [] as object[],
};
const started = performance.now(),
  save = () =>
    writeFile(
      resolve(
        output,
        diagnostic
          ? "capacity.json"
          : count === 1000
            ? "node-1000.json"
            : "node-pilot.json",
      ),
      JSON.stringify(report, null, 2) + "\n",
    );
for (const index of diagnostic
  ? [33, 45]
  : Array.from({ length: count }, (_, i) => i)) {
  const tic = performance.now(),
    seed = "fv08-production-" + index;
  let componentIds: readonly string[] = [];
  try {
    const scenario = { ...loaded.scenario, seed },
      prepared = prepareFastGeneration(scenario, loaded.assets, loaded.context);
    componentIds = prepared.componentIds;
    const generated = prepared.run(1),
      result = runPoliticsReformScrutiny(generated.input);
    report.nonComputable += Number(
      result.status === "non-computable" ||
        result.stages.some((s) => s.status === "non-computable"),
    );
    report.runs.push({
      index,
      seed,
      milliseconds: performance.now() - tic,
      inputHash: createHash("sha256")
        .update(JSON.stringify(generated.input))
        .digest("hex"),
      status: result.status,
      failedStages: result.stages.filter((s) => s.status === "non-computable"),
      filled: result.seats.filter((s) => s.status === "filled").length,
      seats: result.seats.length,
      projectionResidual: generated.audit.projectionResidual,
      decisions: result.subjectDecisions,
      seatCounts: result.seats.reduce(
        (a, s) => {
          const key =
            s.entitlement.chamber + ":" + s.entitlement.listId + ":" + s.status;
          a[key] = (a[key] ?? 0) + 1;
          return a;
        },
        {} as Record<string, number>,
      ),
    });
  } catch (error) {
    report.technicalErrors++;
    report.runs.push({
      index,
      seed,
      milliseconds: performance.now() - tic,
      error: String(error),
      ...(diagnostic && error instanceof Error ? { stack: error.stack } : {}),
      ...(error instanceof ProjectionCapacityError
        ? {
            capacity: error.diagnostics,
            capacityComponents: error.diagnostics.componentIndices.map((j) => ({
              id: j === 0 ? "non-valid" : componentIds[j - 1],
              name:
                j === 0
                  ? "Non-valid"
                  : loaded.scenario.presentation.components.find(
                      (c) => c.id === componentIds[j - 1],
                    )?.name,
            })),
          }
        : {}),
    });
  }
  report.completed++;
  report.elapsedMs = performance.now() - started;
  if (diagnostic || report.completed % 10 === 0) {
    await save();
    console.log(
      JSON.stringify({
        completed: report.completed,
        requested: count,
        elapsedMs: report.elapsedMs,
        technicalErrors: report.technicalErrors,
        nonComputable: report.nonComputable,
      }),
    );
  }
}
await save();
if (report.technicalErrors) process.exitCode = 1;
