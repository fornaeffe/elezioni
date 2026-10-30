import { it, expect } from "vitest";
import {
  distributionScores,
  scoreAreas,
  summarizeUnits,
} from "./filing-aware-scores";
it("weights units equally when their numbers of observable groups differ", () => {
  const good = distributionScores([0.5, 0.5], 0.5, 100),
    bad = distributionScores([0, 0], 0.5, 100),
    result = summarizeUnits([
      { unit: "a", score: good },
      { unit: "b", score: bad },
      { unit: "b", score: bad },
    ])!;
  expect(result.meanMae).toBe(0.25);
  expect(result.units).toBe(2);
  expect(result.variables).toBe(3);
});
it("matches an independent hand-computed empirical CRPS", () => {
  const result = distributionScores([0, 0.2, 0.4, 0.6], 0.4, 1000);
  expect(result.crps).toBeCloseTo(0.075, 12);
  expect(result.nearTruth).toBe(0.25);
  expect(result.coverage["0.9"]).toBe(1);
});
it("scores a common observed college partition without inventing internal joint-list truth", () => {
  const source = {
      truth: [
        {
          id: "a",
          region: "r",
          electors: 100,
          nonValid: 50,
          groups: [["x", "y"]],
          counts: [50],
        },
        {
          id: "b",
          region: "r",
          electors: 100,
          nonValid: 50,
          groups: [["x"], ["y"]],
          counts: [10, 40],
        },
      ],
    },
    out = scoreAreas(
      source,
      ["a", "b"],
      ["x", "y"],
      [Uint32Array.from([50, 20, 30, 50, 10, 40])],
      [
        { municipality: "a", areas: [{ id: "college", weight: 1 }] },
        { municipality: "b", areas: [{ id: "college", weight: 1 }] },
      ],
    );
  expect(out.areas).toBe(1);
  expect(out.rows).toHaveLength(1);
  expect(out.rows[0].members).toEqual(["x", "y"]);
  expect(out.rows[0].truth).toBe(1);
  expect(out.rows[0].values).toEqual([1]);
});
