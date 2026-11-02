import type { ScenarioList } from '../core/types';
import { generatedPoliticsCandidateSlots } from '../scenario/politics-candidate-slots.generated';
const allowed = {
  camera: new Set(generatedPoliticsCandidateSlots.uninominal.filter(r => r.ramo === 'camera').map(r => r.circCode)),
  senato: new Set(generatedPoliticsCandidateSlots.uninominal.filter(r => r.ramo === 'senato').map(r => r.circCode)),
};

/** Legacy geography uses 101/102 for Camera and 1 for Senate, not reform IDs. */
export function isFiled(list: Pick<ScenarioList, 'filing'>, chamber: 'camera' | 'senato', constituency: string | number): boolean {
  return !list.filing || list.filing[chamber].includes(String(constituency));
}

export function validateListFiling(lists: readonly ScenarioList[]): string[] {
  const errors: string[] = [];
  for (const list of lists) {
    if (list.filing !== undefined) {
      if (!list.filing || typeof list.filing !== 'object') { errors.push(`Presentazione territoriale non valida: ${list.name}.`); continue; }
      for (const ch of ['camera', 'senato'] as const) {
        const codes = list.filing[ch];
        if (!Array.isArray(codes) || codes.length === 0 || codes.some(c => !allowed[ch].has(c)) || new Set(codes).size !== codes.length)
          errors.push(`Presentazione territoriale non valida: ${list.name} (${ch}).`);
      }
    }
    if (list.minorityRegion !== undefined && (typeof list.minorityRegion !== 'string' || !/^\d{2}$/.test(list.minorityRegion) || !list.filing ||
      !Array.isArray(list.filing.camera) || !Array.isArray(list.filing.senato) ||
      list.filing.camera.some(c => String(Math.floor(Number(c) / 100)).padStart(2, '0') !== list.minorityRegion) ||
      list.filing.senato.some(c => String(c).padStart(2, '0') !== list.minorityRegion)))
      errors.push(`La dichiarazione di minoranza deve corrispondere a una sola regione: ${list.name}.`);
  }
  return errors;
}

export function copyScenarioList(list: ScenarioList): ScenarioList {
  return { ...list, ...(list.filing ? { filing: { camera: [...list.filing.camera], senato: [...list.filing.senato] } } : {}) };
}
