import { candidatePlacements } from "./candidate-preview";
import { generateReformSimulation, prepareReformGeneration } from './generation';
import type { ReformGenerationScenario, ReformGenerationSnapshot } from './generation-types';
import type { Chamber, ReferenceScope } from './input';

export interface PreviewSelection { chamber: Chamber; role: 'ordinary' | 'bonus' | 'special'; locationId: string; ownerId: string }
/** A single deterministic sample checks generation compatibility. It is not a
 * scrutiny result, nor proof that every future simulation is computable. */
export function previewReformScenario(s: ReformGenerationScenario, snapshot: ReformGenerationSnapshot, selection?: PreviewSelection) {
  const prepared = prepareReformGeneration(s, snapshot);
  const { input, trace } = generateReformSimulation(prepared, 1);
  const placements=candidatePlacements(input,selection);
  const scopes = [...new Map([{ kind: 'national' } as ReferenceScope, ...s.abstention.map(r => r.scope)].map(scope => [JSON.stringify(scope), scope])).values()];
  const abstention = scopes.map(scope => {
    const group = prepared.references.units.filter(u => scope.kind === 'national' || (scope.kind === 'municipality' ? u.municipality.code : scope.kind === 'province' ? u.municipality.provinceId : u.municipality.regionId) === scope.code);
    const value = (ch: Chamber) => {
      if (s.renewal !== 'joint' && s.renewal !== ch) return null;
      const electors = group.reduce((n, u) => n + u.municipality[ch].electors, 0);
      return electors ? group.reduce((n, u) => n + u.municipality[ch].electors * u[ch].abstention, 0) / electors : null;
    };
    return { scope, camera: value('camera'), senato: value('senato') };
  });
  return { references: trace.references, abstention, warnings: trace.warnings, normalization: trace.normalization, placements,
    bonusSelection: trace.bonusSelection };
}
export type ReformEditorPreview = ReturnType<typeof previewReformScenario>;
