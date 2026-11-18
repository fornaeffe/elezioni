import { adaptGeneratedPoliticsRamo } from './adapter';
import { generatePoliticsCandidates, type CandidateSampler } from './candidate-generation';
import { generatePoliticsVotes } from './vote-generation';
import type { NormalSampler } from '$lib/core/rng';
import type {
  PoliticsDirectScrutinySnapshot,
  PoliticsGeneratedRamoSource,
  PoliticsPipelineCandidatePluriRow,
  PoliticsPipelineSource,
  PoliticsVoteGenerationRamoSource,
  Ramo
} from './types';

function asVoteGenerationRamoSource(
  source: PoliticsPipelineSource,
  ramo: Ramo,
  candidatiUniSim: PoliticsVoteGenerationRamoSource['candidati_uni_sim']
): PoliticsVoteGenerationRamoSource {
  const ramoSource = source[ramo];

  return {
    uni: ramoSource.uni,
    candidati_uni_sim: candidatiUniSim,
    candidati_pluri_template: ramoSource.candidati_pluri.map((row) => ({
      LISTA: row.LISTA,
      PLURI_COD: row.PLURI_COD,
      CIRC_COD: row.CIRC_COD,
      MINORANZA: row.MINORANZA
    }))
  };
}

function asGeneratedRamoSource(
  source: PoliticsPipelineSource,
  ramo: Ramo,
  candidateRows: ReturnType<typeof generatePoliticsCandidates>[Ramo],
  voteRows: ReturnType<typeof generatePoliticsVotes>[Ramo]
): PoliticsGeneratedRamoSource {
  const ramoSource = source[ramo];

  return {
    uni_liste_sim: voteRows.uni_liste_sim,
    candidati_uni_sim: voteRows.candidati_uni_sim,
    candidati_pluri_sim: candidateRows.candidati_pluri_sim,
    uni: ramoSource.uni,
    pluri: ramoSource.pluri,
    liste: source.liste,
    candidati_pluri_template: ramoSource.candidati_pluri.map((row: PoliticsPipelineCandidatePluriRow) => ({
      LISTA: row.LISTA,
      PLURI_COD: row.PLURI_COD,
      CIRC_COD: row.CIRC_COD,
      MINORANZA: row.MINORANZA
    }))
  };
}

export function buildPoliticsDirectScrutinySnapshot(
  source: PoliticsPipelineSource,
  options: {
    candidateSample?: CandidateSampler;
    voteNormal?: NormalSampler;
    seed?: string | number;
  } = {}
): PoliticsDirectScrutinySnapshot {
  const candidates = generatePoliticsCandidates(source, {
    sample: options.candidateSample,
    seed: options.seed
  });
  const votes = generatePoliticsVotes(
    {
      data_elezione: source.data_elezione,
      simulazioni: source.simulazioni,
      liste: source.liste,
      comuni_liste: source.comuni_liste,
      base_dati: source.base_dati,
      camera: asVoteGenerationRamoSource(source, 'camera', candidates.camera.candidati_uni_sim),
      senato: asVoteGenerationRamoSource(source, 'senato', candidates.senato.candidati_uni_sim)
    },
    {
      normal: options.voteNormal,
      seed: options.seed
    }
  );
  const camera = adaptGeneratedPoliticsRamo(
    'camera',
    asGeneratedRamoSource(source, 'camera', candidates.camera, votes.camera)
  );
  const senato = adaptGeneratedPoliticsRamo(
    'senato',
    asGeneratedRamoSource(source, 'senato', candidates.senato, votes.senato)
  );

  return {
    schema_version: 1,
    source_schema_version: 1,
    source: 'generated politics TypeScript pipeline',
    purpose: 'Direct-scrutiny snapshot produced by the composed politics generation pipeline.',
    rami: {
      camera: {
        totali_pluri: camera.context.totali_pluri,
        liste_naz: camera.context.liste_naz,
        totale_seggi: camera.context.totale_seggi,
        simulations: camera.simulations
      },
      senato: {
        totali_pluri: senato.context.totali_pluri,
        liste_naz: senato.context.liste_naz,
        totale_seggi: senato.context.totale_seggi,
        simulations: senato.simulations
      }
    }
  };
}
