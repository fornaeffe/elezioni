import type { ScenarioCandidateTemplateRamo } from '$lib/core/types';

export interface PoliticsUninominalCandidateSlot {
  ramo: ScenarioCandidateTemplateRamo;
  circCode: string;
  circLabel: string;
  pluriCode: string;
  pluriLabel: string;
  uninominalCode: string;
  uninominalLabel: string;
}

export interface PoliticsPlurinominalCandidateSlot {
  ramo: ScenarioCandidateTemplateRamo;
  circCode: string;
  circLabel: string;
  pluriCode: string;
  pluriLabel: string;
  seats: number;
  maxCandidates: number;
}

export interface PoliticsCandidateSlotCatalog {
  uninominal: PoliticsUninominalCandidateSlot[];
  plurinominal: PoliticsPlurinominalCandidateSlot[];
}

export type PoliticsCandidateSlot = PoliticsUninominalCandidateSlot | PoliticsPlurinominalCandidateSlot;

export function candidateSlotCode(slot: PoliticsCandidateSlot): string {
  return 'uninominalCode' in slot ? slot.uninominalCode : slot.pluriCode;
}

export function candidateSlotLabel(slot: PoliticsCandidateSlot): string {
  if ('uninominalCode' in slot) {
    return slot.uninominalLabel || `UNI ${slot.uninominalCode}`;
  }

  return slot.pluriLabel || `PLURI ${slot.pluriCode}`;
}

export function candidateSlotSublabel(slot: PoliticsCandidateSlot): string {
  const ramo = slot.ramo === 'camera' ? 'Camera' : 'Senato';
  const circ = slot.circLabel || `Circ. ${slot.circCode}`;

  if ('uninominalCode' in slot) {
    const pluri = slot.pluriLabel || (slot.pluriCode ? `Pluri ${slot.pluriCode}` : '');
    return [ramo, circ, pluri].filter(Boolean).join(' - ');
  }

  return `${ramo} - ${circ} - ${slot.maxCandidates} candidati`;
}

export function candidateSlotSearchText(slot: PoliticsCandidateSlot): string {
  return [
    slot.ramo,
    slot.circCode,
    slot.circLabel,
    'pluriCode' in slot ? slot.pluriCode : '',
    'pluriLabel' in slot ? slot.pluriLabel : '',
    'uninominalCode' in slot ? slot.uninominalCode : '',
    'uninominalLabel' in slot ? slot.uninominalLabel : ''
  ]
    .join(' ')
    .trim();
}
