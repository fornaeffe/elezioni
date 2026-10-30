import { resolve } from "node:path";
import { readResearchArtifact } from "./filing-aware-artifact";
import { JointTailScores } from "./filing-aware-monte-carlo";
/** One bounded record at a time, including country-sized artifacts. */
export async function researchDetails(
  base: string,
  mode: string,
  report: any,
  row: any,
  folder?: string,
) {
  const accumulators = new Map<string, JointTailScores>(),
    bias = new Map<string, any>();
  for await (const { section, record: r } of readResearchArtifact(
    resolve(
      base,
      mode,
      ...(folder ? [folder] : []),
      row.target + "-" + row.horizon + "-" + row.candidate + ".json.gz",
    ),
  )) {
    const scope = section === "metrics" ? r.scope : section,
      unit = section === "metrics" ? (r.municipality ?? "country") : r.area;
    let acc = accumulators.get(scope);
    if (!acc) {
      acc = new JointTailScores(report.draws);
      accumulators.set(scope, acc);
    }
    acc.add(unit, r.values, r.truth);
    const key = scope + ":" + r.members.join("+"),
      b = bias.get(key) ?? {
        scope,
        members: r.members,
        units: 0,
        truth: 0,
        expected: 0,
        signedError: 0,
        absoluteError: 0,
      };
    const mean =
      r.values.reduce((a: number, b: number) => a + b, 0) / r.values.length;
    b.units++;
    b.truth += r.truth;
    b.expected += mean;
    b.signedError += mean - r.truth;
    b.absoluteError += Math.abs(mean - r.truth);
    bias.set(key, b);
  }
  const mc = Object.fromEntries(
    [...accumulators].map(([scope, a]) => [scope, a.finish()]),
  );
  const scopes: Record<string, string> = {
    municipal: "municipality",
    national: "sampledArea",
    college: "college",
    constituency: "constituency",
  };
  for (const [scope, rates] of Object.entries(mc))
    if (
      Math.abs(rates.error30.mean - row.scores[scopes[scope]].error30) > 1e-12
    )
      throw new Error(
        "Stored joint-draw score differs from report: " + mode + ":" + scope,
      );
  return {
    mode,
    ...(folder ? { folder } : {}),
    target: row.target,
    horizon: row.horizon,
    candidate: row.candidate,
    mc,
    bias: [...bias.values()].map((b) => ({
      ...b,
      truth: b.truth / b.units,
      expected: b.expected / b.units,
      signedError: b.signedError / b.units,
      absoluteError: b.absoluteError / b.units,
    })),
  };
}
