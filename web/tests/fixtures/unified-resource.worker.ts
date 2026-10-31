/** Synthetic maximum-dimension engineering probe; never a production data source. */
import { fitReference } from '../../src/lib/politics/reform-2026/vote-share/information';
import { fitProfiles } from '../../src/lib/politics/reform-2026/vote-share/profiles';
import { syntheticNational, syntheticProfiles } from './fast-share-scale';
const scope = self as unknown as {
  onmessage:
    | ((
        e: MessageEvent<{
          components: number;
          polls: number;
          municipalities: number;
        }>,
      ) => void)
    | null;
  postMessage: (x: unknown) => void;
};
scope.onmessage = (e) => {
  const started = performance.now();
  try {
    const { components, polls, municipalities } = e.data,
      fitted = fitReference(
        syntheticNational(components, polls),
        'fv08-resource-' + components + '-' + polls,
        { solver: 'auto' },
      ),
      nationalMs = performance.now() - started;
    const profileStarted = performance.now(),
      profiles = municipalities
        ? fitProfiles(syntheticProfiles(components, municipalities))
        : undefined;
    scope.postMessage({
      ok: true,
      components,
      polls,
      municipalities,
      nationalMs,
      profileMs: performance.now() - profileStarted,
      totalMs: performance.now() - started,
      workspaceBytes: fitted.audit.workspaceBytes,
      profileAudit: profiles?.audit,
      political: fitted.political.diagnostics,
      participation: fitted.participation.diagnostics,
      retainedDraws: fitted.political.drawsPerChain,
    });
  } catch (error) {
    scope.postMessage({
      ok: false,
      error: String(error),
      totalMs: performance.now() - started,
    });
  }
};
