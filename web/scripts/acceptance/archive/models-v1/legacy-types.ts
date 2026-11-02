import { COMPONENT_POLICY, type ComponentObservation } from "./component-policy";
import type { LegacyPollObservation } from "./legacy-polls";
import type { SourceComposition } from "./correspondences";
export interface CompiledLegacy {
  inputKey: string;
  policy: typeof COMPONENT_POLICY;
  componentIds: string[];
  componentMap: Record<string, string>;
  pollCollegeIds: Record<"camera" | "senato", string[]>;
  historicalDate: string;
  rows: Record<
    "camera" | "senato",
    { code: string; probabilities: number[]; original?: ComponentObservation }[]
  >;
  polls: LegacyPollObservation[];
  compositions: Record<string, SourceComposition>;
  requiredReferences: string[];
  audit: {
    baseline: { camera: string; senato: string };
    missingHistory: string[];
    policy: string;
    fallbackCounts: Record<string, number>;
  };
}
