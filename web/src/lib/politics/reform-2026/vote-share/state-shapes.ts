/** One portable layout contract for offline and interactive fitted states. */
import type { GeographicProfile, ReferenceFit } from './forecast-types';
export function fittedStateShapes(fit: ReferenceFit) {
  return Object.fromEntries(
    (['political', 'participation'] as const).map((key) => [
      '/fit/' + key + '/values',
      [fit[key].chains, fit[key].drawsPerChain, fit[key].dimensions],
    ]),
  );
}
export function profileStateShapes(profile: GeographicProfile) {
  return Object.fromEntries(
    ['/cameraProfile', '/senateAdapter/profile'].flatMap((path) =>
      ['means', 'profileSd', 'fallback'].map((key) => [
        path + '/' + key,
        [profile.municipalityIds.length, profile.componentIds.length + 1],
      ]),
    ),
  );
}
