/** Public probability-law identities. Data, numerical and implementation IDs are separate. */
export const modelRegistry = {
  M01: {
    id: "M01",
    version: "2.0.0",
    name: "Corrispondenze e shock per lista",
    label: "M01 2.0.0 — corrispondenze e shock per lista",
    specification: "docs/models/M01.md",
  },
  M02: {
    id: "M02",
    version: "2.0.0",
    name: "Stima congiunta e trasferimenti limitati",
    label: "M02 2.0.0 — stima congiunta e trasferimenti limitati",
    specification: "docs/models/M02.md",
  },
} as const;
export type ModelId = keyof typeof modelRegistry;
export const DEFAULT_MODEL: ModelId = "M02";
export const generationModels = [modelRegistry.M02, modelRegistry.M01] as const;
export function requireGenerationModel(
  choice: { model: string; modelVersion: string } | undefined,
) {
  if (!choice || !Object.hasOwn(modelRegistry, choice.model))
    throw new Error("MODEL_ID_UNSUPPORTED: choose M01 or M02");
  const definition = modelRegistry[choice.model as ModelId];
  if (choice.modelVersion !== definition.version)
    throw new Error(
      "MODEL_VERSION_UNSUPPORTED: " +
        definition.id +
        " requires " +
        definition.version,
    );
  return definition;
}
