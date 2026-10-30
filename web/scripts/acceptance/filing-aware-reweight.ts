/** Recompute the predeclared equal-unit summary from unchanged saved draws. */
import { readFile, writeFile } from "node:fs/promises";
import { resolve } from "node:path";
import { gunzipSync } from "fflate";
import { createHash } from "node:crypto";
import { distributionScores, summarizeUnits } from "./filing-aware-scores";
import { readResearchArtifact } from "./filing-aware-artifact";
const mode = process.argv[2];
if (!["smoke", "development", "validation", "full"].includes(mode))
  throw new Error("Specify a campaign mode");
const root = resolve(".."),
  dir = resolve(
    root,
    "data/derived/filing-aware-research",
    mode,
    ...(mode === "full" ? [process.argv[3]] : []),
  ),
  report = JSON.parse(await readFile(resolve(dir, "report.json"), "utf8")),
  cases: any[] = [];
for (const phase of ["development", "confirmation"]) {
  const r = JSON.parse(
    await readFile(
      resolve(
        root,
        "research/vote-generation/fv01/evidence/data/derived/party-drift/fast-" +
          phase +
          "-final-20261027/cases.json",
      ),
      "utf8",
    ),
  );
  cases.push(
    ...r.cases.filter((x: any) => x.variant === "kind-common-conservative"),
  );
}
for (const row of report.rows) {
  if (row.error) continue;
  const c = cases.find(
      (c) => c.target === row.target && c.horizon === row.horizon,
    ),
    folder =
      mode === "full"
        ? process.argv[3]
        : c.inputDirectory.split(/[\\/]/).at(-1),
    source = JSON.parse(
      await readFile(
        resolve(root, "data/derived/party-drift", folder, "profiles.json"),
        "utf8",
      ),
    ),
    truth = new Map<string, any>(source.truth.map((t: any) => [t.id, t]));
  const municipal: any[] = [],
    college: any[] = [],
    constituency: any[] = [];
  for await (const { section, record: r } of readResearchArtifact(
    resolve(
      dir,
      row.target + "-" + row.horizon + "-" + row.candidate + ".json.gz",
    ),
  )) {
    if (section === "metrics" && r.scope === "municipal") {
      const t = truth.get(r.municipality);
      municipal.push({
        unit: r.municipality,
        score: distributionScores(r.values, r.truth, t.electors - t.nonValid),
      });
    } else if (section === "college")
      college.push({ unit: r.area, score: r.score });
    else if (section === "constituency")
      constituency.push({ unit: r.area, score: r.score });
  }
  row.pooledGroupScores ??= row.scores;
  row.scores = {
    municipality: summarizeUnits(municipal),
    college: summarizeUnits(college),
    constituency: summarizeUnits(constituency),
    sampledArea: row.scores.sampledArea,
  };
}
report.scoreAggregation =
  "equal groups within unit, equal units within case; corrected from stored identical predictive draws, without rerun or reselection";
report.scoringSourceHash = createHash("sha256")
  .update(await readFile(resolve("scripts/acceptance/filing-aware-scores.ts")))
  .digest("hex");
await writeFile(
  resolve(dir, "report.json"),
  JSON.stringify(report, null, 2) + "\n",
);
console.log(
  JSON.stringify({
    mode,
    cases: report.requestedCases,
    rows: report.rows.length,
    scoreAggregation: report.scoreAggregation,
  }),
);
