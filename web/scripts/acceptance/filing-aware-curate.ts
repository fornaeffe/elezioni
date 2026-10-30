/** Keep compact, inspectable research evidence; raw predictive arrays stay derived. */
import { readFile, writeFile, mkdir } from "node:fs/promises";
import { resolve } from "node:path";
import { createHash } from "node:crypto";
import { gzipSync } from "node:zlib";
import { researchDetails } from "./filing-aware-details";
import { mcSummary } from "./filing-aware-monte-carlo";
import frozenSelections from "../../../data/filing-aware-selections.json" with { type: "json" };
const root = resolve(".."),
  base = resolve(root, "data/derived/filing-aware-research");
const modes = [
  "development",
  "validation",
  "reference-development",
  "mass-development",
  "mass-validation",
  "regional-development",
  "regional-validation",
];
const details: any[] = [],
  reports: any[] = [];
for (const mode of modes) {
  const bytes = await readFile(resolve(base, mode, "report.json")),
    report = JSON.parse(bytes.toString());
  if (
    report.rows.length !==
    report.requestedCases *
      (mode === "development"
        ? 7
        : mode === "validation"
          ? 3
          : mode === "mass-development"
            ? 3
            : mode === "regional-development"
              ? 2
              : 1)
  )
    throw new Error("Incomplete campaign " + mode);
  reports.push({
    mode,
    sha256: createHash("sha256").update(bytes).digest("hex"),
    report,
  });
  for (const row of report.rows) {
    if (row.error) continue;
    // Quantify MC error for the selected candidate and its original comparators.
    if (
      ![
        "development",
        "validation",
        "regional-development",
        "regional-validation",
      ].includes(mode)
    )
      continue;
    details.push(await researchDetails(base, mode, report, row));
  }
  console.log(JSON.stringify({ curated: mode, rows: report.rows.length }));
}
for (const mode of ["full", "mass-full", "regional-full"])
  for (const folder of ["modular-1d5b31b17cfb", "modular-939576fd812f"]) {
    const bytes = await readFile(resolve(base, mode, folder, "report.json")),
      report = JSON.parse(bytes.toString());
    if (report.rows.length !== (mode === "full" ? 3 : 1))
      throw new Error("Incomplete full comparison");
    reports.push({
      mode,
      folder,
      sha256: createHash("sha256").update(bytes).digest("hex"),
      report,
    });
    for (const row of report.rows)
      if (
        mode === "regional-full" ||
        (mode === "full" && row.candidate === "legacy-global-beta-v1-scale-1")
      )
        details.push(await researchDetails(base, mode, report, row, folder));
    console.log(JSON.stringify({ curated: mode, folder }));
  }
const comparisons = [];
for (const current of details.filter((d) =>
  ["regional-development", "regional-validation", "regional-full"].includes(
    d.mode,
  ),
)) {
  const old = details.find(
    (d) =>
      d.folder === current.folder &&
      d.target === current.target &&
      d.horizon === current.horizon &&
      d.candidate === "legacy-global-beta-v1-scale-1",
  );
  if (!old) throw new Error("Missing paired comparator");
  comparisons.push({
    ...(current.folder ? { folder: current.folder } : {}),
    target: current.target,
    horizon: current.horizon,
    candidate: current.candidate,
    versus: old.candidate,
    scopes: Object.fromEntries(
      Object.keys(current.mc).map((scope) => [
        scope,
        Object.fromEntries(
          ["error30", "error15", "extreme80"].map((k) => [
            k,
            mcSummary(
              current.mc[scope][k].draws.map(
                (v: number, i: number) => v - old.mc[scope][k].draws[i],
              ),
            ),
          ]),
        ),
      ]),
    ),
  });
}
const selections = frozenSelections;
const output = resolve(root, "test/fixtures/benchmarks");
await mkdir(output, { recursive: true });
const codePaths = [
  "web/scripts/acceptance/fast-share-replay.ts",
  "web/scripts/acceptance/filing-aware-research.ts",
  "web/scripts/acceptance/filing-aware-scores.ts",
  "web/scripts/acceptance/filing-aware-monte-carlo.ts",
  "web/scripts/acceptance/filing-aware-details.ts",
  "web/scripts/acceptance/filing-aware-artifact.ts",
  "web/src/lib/politics/reform-2026/vote-share/feasible-field.ts",
  "web/src/lib/politics/reform-2026/vote-share/reference-noise.ts",
  ...[
    "research-protocol",
    "reference-comparison",
    "mass-comparison",
    "regional-comparison",
    "selections",
  ].map((x) => "data/filing-aware-" + x + ".json"),
];
const codeHashes = Object.fromEntries(
  await Promise.all(
    codePaths.map(async (path) => [
      path,
      createHash("sha256")
        .update(
          (await readFile(resolve(root, path), "utf8")).replaceAll(
            "\r\n",
            "\n",
          ),
        )
        .digest("hex"),
    ]),
  ),
);
const summaryReports = reports.map(({ report, ...meta }) => ({
  ...meta,
  report: {
    ...report,
    rows: report.rows.map((row: any) => ({
      ...row,
      fitDiagnostics: row.fitDiagnostics
        ? Object.fromEntries(
            Object.entries(row.fitDiagnostics).map(
              ([key, value]: [string, any]) => {
                const { coordinates, ...summary } = value;
                return [key, summary];
              },
            ),
          )
        : undefined,
      pooledGroupScores: undefined,
    })),
  },
}));
await writeFile(
  resolve(output, "vote_share_feasible_research.json"),
  JSON.stringify(
    {
      schemaVersion: 1,
      scope:
        "Iterative research, reused political elections; not public production acceptance",
      mcQualification:
        "Tail-rate MCSE and paired normal 95% intervals use whole joint draws conditional on these fixed cases. They are not uncertainty about unseen elections. WIS/coverage/mean-error comparisons are point estimates.",
      selections,
      codeHashes,
      reports: summaryReports,
      pairedTailComparisons: comparisons,
    },
    null,
    2,
  ) + "\n",
);
await writeFile(
  resolve(output, "vote_share_feasible_details.json.gz"),
  gzipSync(JSON.stringify({ details, originalReports: reports })),
);
