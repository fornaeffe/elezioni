import { describe, expect, test } from 'vitest';
import {
  candidateSlotCode,
  candidateSlotLabel,
  candidateSlotSearchText,
  candidateSlotSublabel,
  type PoliticsUninominalCandidateSlot
} from './politics-candidate-slots';
import { generatedPoliticsCandidateSlots } from './politics-candidate-slots.generated';

describe('politics candidate slot catalog', () => {
  test('contains generated uninominal and plurinominal slots with friendly labels', () => {
    expect(generatedPoliticsCandidateSlots.uninominal.length).toBeGreaterThan(200);
    expect(generatedPoliticsCandidateSlots.plurinominal.length).toBeGreaterThan(70);

    const uninominal = generatedPoliticsCandidateSlots.uninominal.find((slot) => slot.uninominalLabel);
    const plurinominal = generatedPoliticsCandidateSlots.plurinominal.find((slot) => slot.pluriLabel);

    expect(uninominal).toBeDefined();
    expect(plurinominal).toBeDefined();
    expect(candidateSlotLabel(uninominal ?? generatedPoliticsCandidateSlots.uninominal[0])).not.toMatch(/^UNI /);
    expect(candidateSlotLabel(plurinominal ?? generatedPoliticsCandidateSlots.plurinominal[0])).not.toMatch(/^PLURI /);
    expect(candidateSlotSearchText(uninominal ?? generatedPoliticsCandidateSlots.uninominal[0])).toContain(
      candidateSlotCode(uninominal ?? generatedPoliticsCandidateSlots.uninominal[0])
    );
  });

  test('falls back to code labels when a slot has no friendly name', () => {
    const slot: PoliticsUninominalCandidateSlot = {
      ramo: 'camera',
      circCode: '1',
      circLabel: '',
      pluriCode: '',
      pluriLabel: '',
      uninominalCode: '10',
      uninominalLabel: ''
    };

    expect(candidateSlotLabel(slot)).toBe('UNI 10');
    expect(candidateSlotSublabel(slot)).toBe('Camera - Circ. 1');
  });
});
