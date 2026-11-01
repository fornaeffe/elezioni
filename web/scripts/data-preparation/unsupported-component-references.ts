/** User-approved default anchors, 1 November 2026. Never inferred by legacy. */
import type { ReleasedScenario } from "../../src/lib/politics/reform-2026/vote-share/release";
import type { FastVoteAssets } from "../../src/lib/politics/reform-2026/vote-share/forecast-types";
import type { ReformAssemblyContext } from "../../src/lib/politics/reform-2026/generation-types";
import { prepareFastGeneration } from "../../src/lib/politics/reform-2026/vote-share/generation";
export interface UnsupportedReferencePolicy {
  policy: "selected-reference-moments-v1";
  components: string[];
  draws: number;
}
export function unsupportedComponentReferences(
  s: ReleasedScenario,
  assets: FastVoteAssets,
  context: ReformAssemblyContext,
  policy: UnsupportedReferencePolicy,
) {
  if (
    policy.policy !== "selected-reference-moments-v1" ||
    !Number.isSafeInteger(policy.draws) ||
    policy.draws < 2 ||
    policy.draws > 4096
  )
    throw new Error("INVALID_UNSUPPORTED_REFERENCE_POLICY");
  const prepared = prepareFastGeneration(s, assets, context),
    ids = prepared.componentIds,
    values = new Map<string, number[]>();
  for (let draw = 1; draw <= policy.draws; draw++) {
    const support = prepared.referenceSupport(draw);
    for (const ch of ["camera", "senato"] as const)
      for (const id of policy.components) {
        const j = ids.indexOf(id) + 1,
          row = support[ch];
        if (j === 0 || !row || !(1 - row[0] > 0))
          throw new Error(
            "UNSUPPORTED_REFERENCE_SOURCE_MISSING:" + ch + "/" + id,
          );
        const key = ch + "/" + id,
          list = values.get(key) ?? [];
        list.push(row[j] / (1 - row[0]));
        values.set(key, list);
      }
  }
  const estimates = [...values].map(([key, list]) => {
    const [channel, id] = key.split("/"),
      mean = list.reduce((a, v) => a + v, 0) / list.length,
      sd = Math.sqrt(
        list.reduce((a, v) => a + (v - mean) ** 2, 0) / (list.length - 1),
      );
    return {
      channel: channel as "camera" | "senato",
      component: id,
      mean,
      sdPercentagePoints: 100 * sd,
    };
  });
  for (const r of estimates) {
    if (
      s.references.some(
        (x) =>
          x.subject.kind === "component" &&
          x.subject.id === r.component &&
          x.scope.kind === "national" &&
          (x.channel === "common" || x.channel === r.channel),
      )
    )
      continue;
    s.references.push({
      id: "selected-reference-anchor:" + r.channel + ":" + r.component,
      scope: { kind: "national" },
      channel: r.channel,
      subject: { kind: "component", id: r.component },
      mean: r.mean,
      sdPercentagePoints: r.sdPercentagePoints,
    });
  }
  return {
    policy: policy.policy,
    sourceFit: assets.fittedStateId,
    sourceProfiles: assets.profileStateId,
    draws: policy.draws,
    seed: s.seed,
    estimates,
    assumption:
      "Explicit default anchors adopted by Luca: reference-date component means and sample SDs from deterministic reference-only draws of the selected model before these anchors. These are model estimates, not additional observations or calibrated forecasts. SDs apply only to selected generation; legacy uses fixed means. Existing scoped overrides retain precedence.",
  };
}
