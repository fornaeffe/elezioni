/** Replay one frozen future draw without altering generator or scrutiny code. */
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import {
  prepareReformGeneration,
  generateReformSimulation,
} from "../../src/lib/politics/reform-2026/generation";
import { runPoliticsReformScrutiny } from "../../src/lib/politics/reform-2026/scrutiny";
const variant = process.argv[2] ?? "polls",
  id = Number(process.argv[3] ?? 23);
if (
  !["polls", "no-polls"].includes(variant) ||
  !Number.isSafeInteger(id) ||
  id < 1
)
  throw Error("Use polls|no-polls POSITIVE_SIMULATION_ID");
const dir = resolve("../data/derived/legacy-comparison"),
  bytes = await readFile(resolve(dir, "future-" + variant + ".json")),
  payload = JSON.parse(bytes.toString()),
  report: any = {
    variant,
    id,
    inputSha256: createHash("sha256").update(bytes).digest("hex"),
    phase: "prepare",
  };
try {
  const prepared = prepareReformGeneration(payload.scenario, payload.snapshot);
  report.phase = "generation";
  const generated = generateReformSimulation(prepared, id);
  report.generatedInputSha256 = createHash("sha256")
    .update(JSON.stringify(generated.input))
    .digest("hex");
  report.phase = "scrutiny";
  const result = runPoliticsReformScrutiny(generated.input);
  report.result = {
    status: result.status,
    seats: result.seats.length,
    filled: result.seats.filter((s) => s.status === "filled").length,
    nonComputableStages: result.stages.filter(
      (s) => s.status === "non-computable",
    ),
    senateAllocation: result.chambers.flatMap((c) => {
      const colleges = c.senateTerritorial?.colleges;
      return colleges?.status === "non-computable"
        ? [
            {
              chamber: c.chamber,
              reason: colleges.reason,
              affectedRegionId: colleges.affectedRegionId,
            },
          ]
        : [];
    }),
  };
  report.phase = "complete";
} catch (error) {
  report.error = String(error);
  report.stack = error instanceof Error ? error.stack : undefined;
}
await writeFile(
  resolve(dir, `diagnostic-${variant}-${id}.json`),
  JSON.stringify(report, null, 2) + "\n",
);
console.log(JSON.stringify(report, null, 2));
