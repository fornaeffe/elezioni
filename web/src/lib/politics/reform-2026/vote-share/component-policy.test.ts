import { describe, it, expect } from "vitest";
import {
  poolConditional,
  completeLocal,
  requireReferenceCoverage,
} from "./component-policy";
import { removeAbsent } from "./interventions";
import { fastGenerationFixture } from "../../../../../tests/fixtures/fast-generation-input";
import { reducedShareInput } from "../../../../../tests/fixtures/fast-share-input";
import { prepareFastGeneration } from "./generation";
import { buildPoliticalTarget, fitReference } from "./information";
import { physicalCells } from "./physical-cells";
import { legacySurfaces } from "./legacy-components";
import type { CompiledLegacy } from "./legacy-adapter";
import { COMPONENT_POLICY } from "./component-policy";
import { projectMargins } from "./projection";
import { unidentifiedDirections } from "./identification";
import { componentIntervention } from "./reference-groups";
function legacyFixture() {
  const f = fastGenerationFixture(),
    ids = f.scenario.components.map((c) => c.id).sort();
  const compiled: CompiledLegacy = {
    inputKey: "constructed",
    policy: COMPONENT_POLICY,
    componentIds: ids,
    componentMap: {},
    historicalDate: "2022-09-25",
    pollCollegeIds: { camera: [], senato: [] },
    rows: { camera: [], senato: [] },
    polls: [],
    compositions: {},
    requiredReferences: [],
    audit: {
      baseline: { camera: "c22", senato: "s22" },
      missingHistory: [],
      policy: COMPONENT_POLICY,
      fallbackCounts: {},
    },
  };
  for (const ch of ["camera", "senato"] as const) {
    compiled.rows[ch] = f.assets.electorates.map((t) => ({
      code: t.code,
      probabilities: [0.4, 0.12, 0.18, 0.15, 0.15],
    }));
    compiled.pollCollegeIds[ch] = f.context.seats.geography.colleges
      .filter((c) => c.chamber === ch && c.kind === "ordinary")
      .map((c) => c.id);
  }
  return { ...f, compiled };
}

describe("adopted component/choice-set policy", () => {
  it("does not identify disjoint choice-set masses through an observed zero", () => {
    const rows = [
      {
        code: "x",
        region: "r",
        electors: 100,
        counts: [0, 100, 0],
        offered: [true, true, false],
      },
      {
        code: "y",
        region: "r",
        electors: 200,
        counts: [0, 0, 200],
        offered: [true, false, true],
      },
    ];
    expect(poolConditional(rows, 3).unidentified).toEqual([false, true, true]);
    const borrowed = poolConditional(rows, 3, [0, 0.7, 0.3]);
    expect(borrowed.probabilities[1]).toBeCloseTo(0.7, 12);
    expect(borrowed.probabilities[2]).toBeCloseTo(0.3, 12);
  });

  it("pools compatible offered sets instead of averaging conditional percentages", () => {
    const pooled = poolConditional(
      [
        {
          code: "full",
          region: "r",
          electors: 100,
          counts: [40, 20, 40],
          offered: [true, true, true],
        },
        {
          code: "partial",
          region: "r",
          electors: 90,
          counts: [60, 30, 0],
          offered: [true, true, false],
        },
      ],
      3,
    );
    expect([...pooled.probabilities]).toEqual(
      expect.arrayContaining([
        expect.closeTo(0.4, 9),
        expect.closeTo(0.2, 9),
        expect.closeTo(0.4, 9),
      ]),
    );
    const local = completeLocal(
      {
        code: "missing",
        region: "r",
        electors: 100,
        counts: [50, 0, 50],
        offered: [true, false, true],
      },
      pooled.probabilities,
    );
    expect([...local]).toEqual([
      expect.closeTo(0.4, 9),
      expect.closeTo(0.2, 9),
      expect.closeTo(0.4, 9),
    ]);
  });
  it("keeps a usable regional zero and borrows national mass only for unobserved coordinates", () => {
    const r = poolConditional(
      [
        {
          code: "x",
          region: "r",
          electors: 100,
          counts: [50, 0, 50, 0],
          offered: [true, true, true, false],
        },
      ],
      4,
      [0.4, 0.2, 0.3, 0.1],
    );
    expect(r.probabilities[1]).toBe(0);
    expect(r.probabilities[3]).toBe(0.1);
    expect(r.observed).toEqual([true, true, true, false]);
  });
  it("regularizes observed zeros only, with original denominators and unchanged tiny positive fractions", () => {
    const raw = {
      code: "x",
      region: "r",
      electors: 1000,
      counts: [999.9, 0, 0.1, 0],
      offered: [true, true, true, false],
    };
    const q = completeLocal(raw, [0.7, 0.1, 0.1, 0.1]);
    expect(q[1]).toBeCloseTo((0.9 * 0.5) / 1000, 12);
    expect(q[2]).toBeCloseTo((0.9 * 0.1) / 1000, 12);
    expect(q[3]).toBe(0.1);
    expect(raw.counts[1]).toBe(0);
  });
  it("conditions deterministically including non-valid support and reports undefined remaining support", () => {
    const p = removeAbsent([0.4, 0.27, 0.33], [true, false, true]);
    expect(p[0]).toBeCloseTo(0.4 / 0.73);
    expect(p[2]).toBeCloseTo(0.33 / 0.73);
    expect(() => removeAbsent([0, 1, 0], [true, false, true])).toThrow(
      "EMPTY_REMAINING_SUPPORT",
    );
    expect(() => removeAbsent([0.5, -0.2, 0.7], [true, false, true])).toThrow(
      "INVALID_SUPPORT",
    );
  });
  it("anchors national component support before future filing and counts only on physical ballots", () => {
    const f = fastGenerationFixture();
    f.scenario.renewal = "camera";
    f.scenario.references = [
      {
        id: "a",
        scope: { kind: "national" },
        channel: "camera",
        subject: { kind: "component", id: "a" },
        mean: 0.9,
        sdPercentagePoints: 0,
      },
    ];
    const all = prepareFastGeneration(f.scenario, f.assets, f.context);
    f.scenario.coverage
      .filter((c) => c.chamber === "camera")
      .slice(1)
      .forEach((c) => (c.listIds = c.listIds.slice(1)));
    const restricted = prepareFastGeneration(f.scenario, f.assets, f.context);
    expect(restricted.centralReference).toEqual(all.centralReference);
    const mean = (rows: Float64Array[]) =>
      rows.reduce(
        (a, r, i) => a + restricted.cells.camera[i].electors * r[1],
        0,
      ) /
      rows.reduce(
        (a, r, i) => a + restricted.cells.camera[i].electors * (1 - r[0]),
        0,
      );
    expect(mean(restricted.centralReference.camera)).toBeCloseTo(0.9, 8);
  });
  it("requires reference coverage for every filed physical cell and applicable chamber", () => {
    const f = fastGenerationFixture(),
      cells = {
        camera: physicalCells(
          f.assets.geography,
          f.assets.electorates,
          ["camera"],
          "coverage",
        ),
        senato: physicalCells(
          f.assets.geography,
          f.assets.electorates,
          ["senato"],
          "coverage",
        ),
      };
    f.scenario.references = [
      {
        id: "a",
        scope: { kind: "municipality", code: cells.camera[0].municipality },
        channel: "camera",
        subject: { kind: "component", id: "a" },
        mean: 0.1,
        sdPercentagePoints: 0,
      },
    ];
    expect(() => requireReferenceCoverage(f.scenario, cells, ["a"])).toThrow(
      "COVERAGE_REQUIRED",
    );
    f.scenario.references[0].scope = { kind: "national" };
    f.scenario.references[0].channel = "common";
    expect(() =>
      requireReferenceCoverage(f.scenario, cells, ["a"]),
    ).not.toThrow();
  });
  it("orthogonal elimination agrees with the independent unstratified Gaussian covariance calculation", () => {
    const input = reducedShareInput().national,
      baseline = buildPoliticalTarget(input),
      stratified = structuredClone(input);
    stratified.elections.forEach(
      (e) =>
        (e.strata = [
          { electors: e.electors, groups: e.groups, counts: e.counts },
        ]),
    );
    const actual = buildPoliticalTarget(stratified);
    actual.target.precision.forEach((v, i) =>
      expect(
        Math.abs(v - baseline.target.precision[i]) / Math.max(1, Math.abs(v)),
      ).toBeLessThan(1e-8),
    );
    expect(
      actual.audit.iterations.every((i) => i.solver === "orthogonal-strata"),
    ).toBe(true);
  });
  it("fits unidentified joint support as one group and requires explicit references for the missing contrasts", () => {
    const input = reducedShareInput().national;
    input.components = ["a", "b"];
    input.elections = input.elections.map((e) => ({
      ...e,
      groups: [["a", "b"]],
      counts: [e.counts[0], e.electors - e.counts[0]],
    }));
    input.polls = [];
    expect(() => fitReference(input, "unidentified")).toThrow("UNRESOLVED");
    input.referenceRequired = ["a", "b"];
    const fit = fitReference(input, "unidentified");
    expect(fit.referenceRequired).toEqual(["a", "b"]);
    expect(fit.identificationGroups).toEqual([["a", "b"]]);
    expect(fit.political.values[0] + fit.political.values[1]).toBe(1);
    expect(input.elections[0].groups).toEqual([["a", "b"]]);
  });
  it("legacy fixes component references inside joint tickets and ignores scenario-reference SDs", () => {
    const f = legacyFixture();
    f.scenario.references = [
      {
        id: "a",
        scope: { kind: "national" },
        channel: "camera",
        subject: { kind: "component", id: "a" },
        mean: 0.3,
        sdPercentagePoints: 2,
      },
    ];
    const first = prepareFastGeneration(f.scenario, f.assets, f.context, {
      legacy: f.compiled,
    });
    const r = first.run(2);
    expect(r.audit.referenceTargets[0].groups[0].share).toBe(0.3);
    f.scenario.references[0].sdPercentagePoints = 90;
    expect(
      prepareFastGeneration(f.scenario, f.assets, f.context, {
        legacy: f.compiled,
      }).run(2),
    ).toEqual(r);
  });
  it("legacy has no future movement at the reference date, while integer counts can vary", () => {
    const f = legacyFixture();
    f.scenario.electionDate = f.scenario.referenceDate;
    const p = prepareFastGeneration(f.scenario, f.assets, f.context, {
        legacy: f.compiled,
      }),
      a = p.run(1),
      b = p.run(2);
    for (const ch of ["camera", "senato"] as const)
      a.audit.countryForecast[ch]!.forEach((v, j) =>
        expect(v).toBeCloseTo(a.audit.countryReference[ch]![j], 12),
      );
    expect(a.audit.countryForecast).toEqual(b.audit.countryForecast);
  });
  it("legacy reuses one poll-group measurement error across components, ticket labels and chambers", () => {
    const f = legacyFixture();
    f.compiled.polls = [
      {
        id: "joint",
        date: "2026-10-18",
        end: "2026-10-18",
        available: "2026-10-19",
        context: "camera",
        sample: 500,
        source: "constructed",
        enabled: true,
        complete: true,
        groups: [
          { label: "ab", members: ["a", "b"], share: 0.4 },
          { label: "cd", members: ["c", "d"], share: 0.6 },
        ],
      },
    ];
    f.compiled.compositions = {
      "poll:joint:ab": {
        fractions: { a: 0.6, b: 0.4 },
        provenance: "fixed test split",
      },
      "poll:joint:cd": {
        fractions: { c: 0.5, d: 0.5 },
        provenance: "fixed test split",
      },
    };
    const totals = [] as number[][];
    for (const ch of ["camera", "senato"] as const) {
      const cells = physicalCells(
          f.assets.geography,
          f.assets.electorates,
          [ch],
          "poll-test",
        ),
        rows = legacySurfaces(f.scenario, f.compiled, cells, ch, 3, false),
        mean = Array(5).fill(0) as number[];
      let valid = 0;
      cells.forEach((c, i) => {
        if (f.compiled.pollCollegeIds[ch].includes(c.collegeId)) {
          valid += c.electors * (1 - rows[i][0]);
          rows[i].forEach((p, j) => (mean[j] += c.electors * p));
        }
      });
      totals.push(mean.map((v) => v / valid));
      expect(mean[1] / (mean[1] + mean[2])).toBeCloseTo(0.6, 10);
    }
    totals[0]
      .slice(1)
      .forEach((v, j) => expect(v).toBeCloseTo(totals[1][j + 1], 10));
  });
  it("keeps latent support outside a complete poll choice set and retains a partial unreported residual", () => {
    const f = legacyFixture(),
      cells = physicalCells(
        f.assets.geography,
        f.assets.electorates,
        ["camera"],
        "conditional-poll",
      );
    f.compiled.polls = [
      {
        id: "ab",
        date: "2026-10-18",
        end: "2026-10-18",
        available: "2026-10-19",
        context: "camera",
        sample: 1000,
        source: "test",
        enabled: true,
        complete: true,
        groups: [
          { label: "a", members: ["a"], share: 0.8 },
          { label: "b", members: ["b"], share: 0.2 },
        ],
      },
    ];
    const rows = legacySurfaces(
        f.scenario,
        f.compiled,
        cells,
        "camera",
        1,
        true,
      ),
      i = cells.findIndex((c) =>
        f.compiled.pollCollegeIds.camera.includes(c.collegeId),
      );
    expect(rows[i][0]).toBeCloseTo(0.4, 12);
    expect(rows[i][3]).toBeCloseTo(0.15, 12);
    expect(rows[i][4]).toBeCloseTo(0.15, 12);
    expect(rows[i][1]).toBeCloseTo(0.24, 10);
    expect(rows[i][2]).toBeCloseTo(0.06, 10);
    f.compiled.polls[0].complete = false;
    f.compiled.polls[0].groups = [{ label: "a", members: ["a"], share: 0.2 }];
    const partial = legacySurfaces(
      f.scenario,
      f.compiled,
      cells,
      "camera",
      1,
      true,
    );
    expect(partial[i][1] / (1 - partial[i][0])).toBeCloseTo(0.2, 10);
    expect(partial[i][2] + partial[i][3] + partial[i][4]).toBeCloseTo(0.48, 10);
  });
  it("preserves tiny positive reference support without a flow-capacity cutoff", () => {
    const p = projectMargins([0.5, 0.5, 1e-18], 2, () => ({
      weight: 1,
      scores: [0, 0, 0],
    }));
    expect(p.probabilities(0)[2]).toBeGreaterThan(0);
    expect(p.probabilities(0)[2]).toBeCloseTo(1e-18, 25);
  });
  it("detects overlapping unidentified contrasts even when no columns are identical", () => {
    const directions = unidentifiedDirections(
      ["a", "b", "c", "d"],
      [
        {
          groups: [
            ["a", "b"],
            ["c", "d"],
          ],
        },
        {
          groups: [
            ["a", "c"],
            ["b", "d"],
          ],
        },
      ],
    );
    expect(directions).toHaveLength(1);
    const d = directions[0];
    expect(d[0] + d[1]).toBeCloseTo(0);
    expect(d[0] + d[2]).toBeCloseTo(0);
    expect(Math.hypot(...d)).toBeCloseTo(1);
  });
  it("calibrates unidentified splits without changing each cell group total or duplicating its uncertainty", () => {
    const rows = [
        Float64Array.from([0.2, 0.6, 0.1, 0.1]),
        Float64Array.from([0.2, 0.05, 0.05, 0.7]),
      ],
      before = rows.map((r) => r[1] + r[2]);
    componentIntervention(
      rows,
      [100, 100],
      [[1], [2]],
      [0.1, 0.4],
      [[1, 2]],
      () => 0,
    );
    rows.forEach((r, i) => {
      expect(r[1] + r[2]).toBeCloseTo(before[i], 10);
      expect(r[0]).toBe(0.2);
    });
    expect((rows[0][1] + rows[1][1]) / 1.6).toBeCloseTo(0.1, 9);
    expect((rows[0][2] + rows[1][2]) / 1.6).toBeCloseTo(0.4, 9);
  });
});
