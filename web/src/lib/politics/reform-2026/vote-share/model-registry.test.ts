import { describe, it, expect } from "vitest";
import {
  modelRegistry,
  generationModels,
  DEFAULT_MODEL,
  requireGenerationModel,
} from "./model-registry";
import { fastGenerationFixture } from "../../../../../tests/fixtures/fast-generation-input";
import { requireFastScenario } from "./scenario";
describe("public model identities", () => {
  it("keeps two independent explicit versions and M02 as default", () => {
    expect(DEFAULT_MODEL).toBe("M02");
    expect(generationModels.map((m) => [m.id, m.version])).toEqual([
      ["M02", "2.0.0"],
      ["M01", "2.0.0"],
    ]);
    expect(
      requireGenerationModel({ model: "M01", modelVersion: "2.0.0" }),
    ).toBe(modelRegistry.M01);
  });
  it("rejects unsupported IDs and versions instead of silently selecting a replacement law", () => {
    expect(() =>
      requireGenerationModel({ model: "selected", modelVersion: "2.0.0" }),
    ).toThrow("MODEL_ID_UNSUPPORTED");
    expect(() =>
      requireGenerationModel({ model: "M02", modelVersion: "1.0.0" }),
    ).toThrow("MODEL_VERSION_UNSUPPORTED");
    const s = fastGenerationFixture().scenario;
    (s.generation as { modelVersion?: string }).modelVersion = undefined;
    expect(() => requireFastScenario(s)).toThrow();
  });
});
