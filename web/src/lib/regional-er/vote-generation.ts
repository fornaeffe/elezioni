import { generateVotes, type VoteGenerationListRow, type VoteGenerationLocalRow } from '$lib/core/vote-generation';
import type { NormalSampler } from '$lib/core/rng';
import type { RegionalErGeneratedVoteRow, RegionalErMunicipalListParameterRow, RegionalErPipelineSource } from './types';

type RegionalErVoteGenerationLocalRow = VoteGenerationLocalRow & RegionalErMunicipalListParameterRow;

export function generateRegionalErVotes(
  source: RegionalErPipelineSource,
  options: {
    normal?: NormalSampler;
    seed?: string | number;
  } = {}
): RegionalErGeneratedVoteRow[] {
  const localRows: RegionalErVoteGenerationLocalRow[] = source.comuni_liste.map((row) => ({ ...row }));
  const listRows: VoteGenerationListRow[] = source.liste.map((row) => ({
    DATA: row.DATA,
    LISTA: row.LISTA,
    LOGIT_P: row.LOGIT_P,
    SIGMA_GLOBAL: row.SIGMA_GLOBAL
  }));

  return generateVotes(localRows, listRows, {
    electionDate: source.data_elezione,
    localityColumn: 'CODICE_COMUNE',
    normal: options.normal,
    seed: options.seed,
    simulations: source.simulazioni
  }).map((row) => row as unknown as RegionalErGeneratedVoteRow);
}
