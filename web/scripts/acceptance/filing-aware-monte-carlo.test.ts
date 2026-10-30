import { it, expect } from "vitest";
import { JointTailScores } from "./filing-aware-monte-carlo";
it("treats common-shock towns as one joint predictive draw, with equal unit weights", () => {
  const one = new JointTailScores(4),
    many = new JointTailScores(4);
  one.add("a", [0, 0, 1, 1], 0);
  for (const town of ["a", "b", "c"]) {
    many.add(town, [0, 0, 1, 1], 0);
    if (town === "b") many.add(town, [0, 0, 1, 1], 0);
  }
  const a = one.finish(),
    b = many.finish();
  expect(b.error30).toEqual(a.error30);
  expect(a.error30.mean).toBe(0.5);
  expect(a.error30.mcse).toBeCloseTo(Math.sqrt(1 / 12));
});
