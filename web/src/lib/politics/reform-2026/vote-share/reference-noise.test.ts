import { describe, it, expect } from "vitest";
import { referenceNoise } from "./reference-noise";
import { forecastRandom } from "./forecast-random";

describe("reference geographic covariance", () => {
  it("retains the previous independent streams exactly when rho is zero", () => {
    for (const ch of ["camera", "senato"] as const) {
      const rng = forecastRandom("noise", 7, "profile:town", ch, 0.7);
      expect([
        ...referenceNoise("noise", 7, 8, 0.7)(ch, "town", "region"),
      ]).toEqual(Array.from({ length: 8 }, () => rng.normal()));
    }
  });
  it("has unit marginal variance and the specified within-region covariance", () => {
    const rows = Array.from({ length: 4000 }, (_, i) => {
      const noise = referenceNoise("moments", i, 1, 0, 0.5);
      return ["a", "b", "c"].map(
        (town, j) => noise("camera", town, j === 2 ? "r2" : "r1")[0],
      );
    });
    const mean = (j: number) =>
      rows.reduce((s, r) => s + r[j], 0) / rows.length;
    const covariance = (j: number, k: number) =>
      rows.reduce((s, r) => s + (r[j] - mean(j)) * (r[k] - mean(k)), 0) /
      rows.length;
    for (let j = 0; j < 3; j++) {
      expect(Math.abs(mean(j))).toBeLessThan(0.06);
      expect(covariance(j, j)).toBeGreaterThan(0.9);
      expect(covariance(j, j)).toBeLessThan(1.1);
    }
    expect(covariance(0, 1)).toBeGreaterThan(0.44);
    expect(covariance(0, 1)).toBeLessThan(0.56);
    expect(Math.abs(covariance(0, 2))).toBeLessThan(0.06);
  });
  it("shares municipality noise across physical splits and is independent of request order", () => {
    const a = referenceNoise("split", 3, 4, 0.5, 0.25),
      b = referenceNoise("split", 3, 4, 0.5, 0.25);
    a("camera", "other", "r1");
    expect(a("camera", "town", "r1")).toEqual(b("camera", "town", "r1"));
    expect(a("camera", "town", "r1")).toBe(a("camera", "town", "r1"));
    expect([
      ...referenceNoise("split", 3, 4, 0.5, 0.25, true)("camera", "town", "r1"),
    ]).toEqual([0, 0, 0, 0]);
    expect(() => referenceNoise("bad", 1, 1, 0, 1.1)).toThrow();
  });
});
