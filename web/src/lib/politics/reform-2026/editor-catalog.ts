import type { ReformGenerationSnapshot } from './generation-types';

/** Compact editor-only projection; historical vote matrices stay in workers. */
export function createReformEditorCatalog(snapshot: ReformGenerationSnapshot) {
  return { schemaVersion: 1 as const, snapshotVersion: snapshot.version, modelVersion: snapshot.models.version,
    historicalDate: snapshot.historicalDate, historicalLists: snapshot.historicalLists,
    regions: snapshot.seats.geography.regions,
    territories: snapshot.seats.geography.territories.map(({ id, name, chamber, regionId, kind }) => ({ id, name, chamber, regionId, kind })),
    colleges: snapshot.seats.geography.colleges.map(({ id, name, chamber, territoryId, kind }) => ({ id, name, chamber, territoryId, kind })),
    municipalities: snapshot.municipalities.map(({ code, name, provinceId, regionId }) => ({ code, name, provinceId, regionId })) };
}
export type ReformEditorCatalog = ReturnType<typeof createReformEditorCatalog>;
