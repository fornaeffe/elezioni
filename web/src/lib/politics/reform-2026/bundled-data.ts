import data from './data/r26-seats-census-2021-i08-v1-9672c20529b2.json';
import { validateSeatTableBundle, type SeatTableBundle } from './endowments';

/** Small tracked artifact, usable without downloaded archives, R, scenarios or a network. */
export function getBundledReformSeatTables(): SeatTableBundle {
  const bundle = structuredClone(data) as SeatTableBundle;
  const errors = validateSeatTableBundle(bundle);
  if (errors.length) throw new Error(`Invalid bundled reform seat tables: ${errors.join('; ')}`);
  return bundle;
}
