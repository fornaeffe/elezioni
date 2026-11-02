/** Law-neutral original geography. Reform overlays belong to normalize.ts only. */
import { archiveMember, dbf, delimited } from './formats';
import { readSource, type SourceManifest } from './sources';

export async function readDefinitiveGeography(manifest: SourceManifest, directory: string) {
  const source = async (id: string) => {
    const entry = manifest.sources.find(s => s.id === id);
    if (!entry) throw new Error(`Missing geography source ${id}`);
    return readSource(directory, entry);
  };
  const [bytes, census, municipalities] = await Promise.all([
    source('electoral-geography-2022'), source('legal-population-2021'), source('municipalities-2021'),
  ]);
  const rows = dbf(archiveMember(bytes, 'UT_Collegi2020/UT_Collegi2020.dbf'));
  const populations = new Map(delimited(census).filter(r => /^\d{6}$/.test(r.REF_AREA) && r.TIME_PERIOD === '2021' && r.INDICATOR === 'RESPOP_AV').map(r => [r.REF_AREA, Number(r.OBS_VALUE)]));
  const raw = JSON.parse(municipalities.toString('utf8')).resultset as Record<string, string>[];
  const metadata = raw.map(m => ({
    code: m.PRO_COM_T, name: m.COMUNE, province: m.DEN_UTS, provinceId: m.COD_PROV_STORICO,
    region: m.DEN_REG, regionId: m.COD_REG, population: populations.get(m.PRO_COM_T)!,
  })).sort((a, b) => a.code < b.code ? -1 : 1);
  if (metadata.length !== 7904 || populations.size !== metadata.length || metadata.some(m => !Number.isSafeInteger(m.population) || m.population < 0) || metadata.reduce((n, m) => n + m.population, 0) !== 59030133)
    throw new Error('Incomplete pinned municipal census');
  return { rows, municipalities: metadata };
}
export type DefinitiveGeography = Awaited<ReturnType<typeof readDefinitiveGeography>>;
