import { describe, it, expect } from "vitest";
import { createHash } from "node:crypto";
import { feasibleField, FIELD_MODELS, type FieldInput } from "./feasible-field";
const fixture = (model: FieldInput["model"]): FieldInput => ({
  model,
  reference: [
    Float64Array.from([0.2, 0.3, 0.5, 0]),
    Float64Array.from([0.4, 0.4, 0, 0.2]),
    Float64Array.from([0.3, 0.1, 0.6, 0]),
  ],
  cells: [
    { id: "a", region: "r1", electors: 100 },
    { id: "b", region: "r2", electors: 20 },
    { id: "c", region: "r1", electors: 200 },
  ],
  politicalScales: [0.4, 0.4, 0.4],
  localScales: [0.1, 0.2, 0.2, 0.2],
  participationScale: 0.2,
  discrepancy: 0.2,
  horizon: 2,
  seed: "feasible-fixture",
  simulationId: 1,
});
describe("filing-aware forecast candidates", () => {
  it("retains default draws with explicit unit multipliers and scalar discrepancy coordinates", () => {
    for (const model of FIELD_MODELS) {
      const input = fixture(model);
      expect(
        feasibleField({
          ...input,
          localMultipliers: input.reference.map((r) =>
            new Float64Array(r.length).fill(1),
          ),
          discrepancyScales: new Float64Array(3).fill(input.discrepancy),
        }),
      ).toEqual(feasibleField(input));
    }
  });
  it("honours separately supplied local and discrepancy scales in bounded fields", () => {
    const input = fixture("bounded-mass-pair-field-v2");
    const out = feasibleField({
      ...input,
      politicalScales: [0, 0, 0],
      participationScale: 0,
      discrepancyScales: [0, 0, 0],
      localMultipliers: input.reference.map((r, i) =>
        new Float64Array(r.length).fill(i === 0 ? 0 : 1),
      ),
    });
    expect(out.probabilities[0]).toEqual(input.reference[0]);
    expect(out.probabilities[1]).not.toEqual(input.reference[1]);
  });
  it("does not freeze major-party variance when a small residual is represented by many components", () => {
    const variances = [1, 14].map((tiny) => {
      const p = Float64Array.from([
        0.2,
        0.24,
        0.24,
        0.24,
        ...new Array(tiny).fill(0.08 / tiny),
      ]);
      let total = 0,
        square = 0;
      for (let i = 1; i <= 2000; i++) {
        const x = feasibleField({
          model: "bounded-mass-pair-field-v2",
          reference: [p],
          cells: [{ id: "a", region: "r", electors: 100 }],
          politicalScales: new Float64Array(p.length - 1).fill(0.35),
          localScales: new Float64Array(p.length),
          participationScale: 0,
          discrepancy: 0.25,
          horizon: 1,
          seed: "mass-variance-fixture",
          simulationId: i,
        }).probabilities[0][1];
        total += x;
        square += x * x;
      }
      return square / 2000 - (total / 2000) ** 2;
    });
    expect(variances[1] / variances[0]).toBeGreaterThan(0.4);
    expect(variances[1] / variances[0]).toBeLessThan(2.5);
  });
  it("preserves all earlier v1 fixture draws across the performance optimization", () => {
    const hash = createHash("sha256");
    for (let simulationId = 1; simulationId <= 64; simulationId++)
      for (const p of feasibleField({
        ...fixture("bounded-pair-field-v1"),
        simulationId,
      }).probabilities)
        hash.update(new Uint8Array(p.buffer));
    expect(hash.digest("hex")).toBe(
      "38ef3bfca7543c91937d41ddc3a42a77d3effb8efc224e9b0d1d9b7514cd593b",
    );
  });
  it("mass-weighted transfers preserve means with state-dependent matching", () => {
    const input = fixture("bounded-mass-pair-field-v2"),
      draws = 2400,
      total = new Float64Array(12),
      square = new Float64Array(12);
    for (let s = 1; s <= draws; s++)
      feasibleField({ ...input, simulationId: s }).probabilities.forEach(
        (p, i) =>
          p.forEach((x, j) => {
            total[i * 4 + j] += x;
            square[i * 4 + j] += x * x;
          }),
      );
    input.reference.forEach((p, i) =>
      p.forEach((x, j) => {
        const k = i * 4 + j,
          mean = total[k] / draws,
          mcse = Math.sqrt(
            Math.max(0, square[k] / draws - mean * mean) / draws,
          );
        expect(Math.abs(mean - x)).toBeLessThan(4 * mcse + 1e-12);
      }),
    );
  });
  for (const model of FIELD_MODELS) {
    it(
      model + " preserves every structural zero and physical-cell total",
      () => {
        const input = fixture(model),
          out = feasibleField(input);
        for (let i = 0; i < input.reference.length; i++) {
          expect(
            [...out.probabilities[i]].reduce((a, b) => a + b, 0),
          ).toBeCloseTo(1, 12);
          expect(out.probabilities[i].every((x) => x >= 0 && x <= 1)).toBe(
            true,
          );
          input.reference[i].forEach((p, j) => {
            if (!p) expect(out.probabilities[i][j]).toBe(0);
          });
        }
        expect(out.country[3]).toBeLessThanOrEqual(20 / 320);
        const again = feasibleField(input);
        expect(again.probabilities).toEqual(out.probabilities);
        const reversed = feasibleField({
          ...input,
          reference: [...input.reference].reverse(),
          cells: [...input.cells].reverse(),
        });
        reversed.probabilities
          .reverse()
          .forEach((p, i) =>
            p.forEach((x, j) =>
              expect(x).toBeCloseTo(out.probabilities[i][j], 12),
            ),
          );
      },
    );
    it(model + " returns the reference with no movement or discrepancy", () => {
      const input = fixture(model),
        out = feasibleField({ ...input, horizon: 0, discrepancy: 0 });
      out.probabilities.forEach((p, i) =>
        p.forEach((x, j) => expect(x).toBeCloseTo(input.reference[i][j], 13)),
      );
    });
  }
  it("bounded transfers preserve each municipality mean, including turnout and restricted support", () => {
    const input = fixture("bounded-pair-field-v1"),
      total = new Float64Array(12),
      square = new Float64Array(12),
      draws = 2400;
    for (let s = 1; s <= draws; s++)
      feasibleField({ ...input, simulationId: s }).probabilities.forEach(
        (p, i) =>
          p.forEach((x, j) => {
            total[i * 4 + j] += x;
            square[i * 4 + j] += x * x;
          }),
      );
    input.reference.forEach((p, i) =>
      p.forEach((x, j) => {
        const k = i * 4 + j,
          mean = total[k] / draws,
          mcse = Math.sqrt(
            Math.max(0, square[k] / draws - mean * mean) / draws,
          );
        expect(Math.abs(mean - x)).toBeLessThan(4 * mcse + 1e-12);
      }),
    );
  });
  it("rejects invalid probability fields and scale parameters", () => {
    expect(() =>
      feasibleField({
        ...fixture("bounded-pair-field-v1"),
        scaleMultiplier: -1,
      }),
    ).toThrow("INVALID_FEASIBLE_FIELD");
  });
});
