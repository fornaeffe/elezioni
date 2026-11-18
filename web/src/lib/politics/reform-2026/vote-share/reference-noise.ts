import { forecastRandom } from "./forecast-random";

/** One draw's reference-profile field. A regional covariance reallocates the
 * existing variance; each municipality still has standard Normal marginals.
 * Create a new factory for every draw so caches cannot accumulate across runs. */
export function referenceNoise(
  seed: string,
  simulationId: number,
  dimensions: number,
  chamberCorrelation = 0,
  regionCorrelation = 0,
  central = false,
) {
  if (
    !Number.isFinite(regionCorrelation) ||
    regionCorrelation < 0 ||
    regionCorrelation > 1
  )
    throw new RangeError("Invalid reference-profile region correlation");
  const regions = new Map<string, Float64Array>(),
    municipalities = new Map<string, Float64Array>();
  const normals = (entity: string, chamber: "camera" | "senato") => {
    const rng = forecastRandom(
      seed,
      simulationId,
      entity,
      chamber,
      chamberCorrelation,
    );
    return Float64Array.from({ length: dimensions }, () =>
      central ? 0 : rng.normal(),
    );
  };
  return (
    chamber: "camera" | "senato",
    municipality: string,
    region: string,
  ) => {
    const key = chamber + ":" + municipality;
    let out = municipalities.get(key);
    if (out) return out;
    out = normals("profile:" + municipality, chamber);
    if (regionCorrelation > 0 && !central) {
      const regionKey = chamber + ":" + region;
      let shared = regions.get(regionKey);
      if (!shared) {
        shared = normals("reference-region:" + region, chamber);
        regions.set(regionKey, shared);
      }
      for (let j = 0; j < dimensions; j++)
        out[j] =
          Math.sqrt(1 - regionCorrelation) * out[j] +
          Math.sqrt(regionCorrelation) * shared[j];
    }
    municipalities.set(key, out);
    return out;
  };
}
