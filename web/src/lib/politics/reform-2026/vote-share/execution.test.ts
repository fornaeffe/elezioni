import { describe, it, expect } from 'vitest';
import { createHash } from 'node:crypto';
import { reducedShareInput } from '../../../../../tests/fixtures/fast-share-input';
import { fitReference, fitReferenceSteps } from './information';
import { fitProfiles, fitProfilesSteps } from './profiles';
import { cooperativeSteps } from './execution';
const hash = (x: unknown) =>
  createHash('sha256').update(JSON.stringify(x)).digest('hex');
describe('FV-06 shared synchronous/cooperative arithmetic', () => {
  it('matches independently frozen pre-FV06 outputs exactly across scheduling boundaries', async () => {
    const input = reducedShareInput(),
      phases = new Set<string>(),
      checkpoint = async (p: { phase: string }) => {
        phases.add(p.phase);
        await Promise.resolve();
      };
    const fit = await cooperativeSteps(
        fitReferenceSteps(input.national, 'fv06-parity'),
        checkpoint,
      ),
      profiles = await cooperativeSteps(
        fitProfilesSteps(input.profiles),
        checkpoint,
      );
    expect(hash(fit)).toBe(
      '24e01c03459af202f35f584357c912d1d57580c8124b3d0fc1b121638d32271e',
    );
    expect(hash(profiles)).toBe(
      '7b59b2dd6b9fec57b7e001532c8e8934a3fd5ceaa74ccee81223ebccb951d700',
    );
    expect(hash(fitReference(input.national, 'fv06-parity'))).toBe(hash(fit));
    expect(hash(fitProfiles(input.profiles))).toBe(hash(profiles));
    for (const p of [
      'covariance-solves',
      'sampling',
      'diagnostics',
      'profile-features',
      'profile-filter',
    ])
      expect(phases.has(p)).toBe(true);
  });
});
