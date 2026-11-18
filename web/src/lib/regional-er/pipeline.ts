import type {
  RegionalErGeneratedVoteRow,
  RegionalErPipelineSource,
  RegionalErScrutinyInputRow
} from './types';
import { generateRegionalErVotes } from './vote-generation';

export interface RegionalErDirectScrutinySimulation {
  sim: number;
  input: {
    comuni_liste: RegionalErScrutinyInputRow[];
  };
}

export interface RegionalErDirectScrutinySnapshot {
  schema_version: number;
  source_schema_version: number;
  source: string;
  purpose: string;
  simulations: RegionalErDirectScrutinySimulation[];
}

function scrutinyRow(row: RegionalErGeneratedVoteRow): RegionalErScrutinyInputRow {
  return {
    CODICE_COMUNE: row.CODICE_COMUNE,
    COMUNE: row.COMUNE,
    LISTA: row.LISTA,
    CODICE_PROVINCIA: row.CODICE_PROVINCIA,
    PROVINCIA: row.PROVINCIA,
    VOTI_LISTA_SIM: row.VOTI_LISTA_SIM
  };
}

export function buildRegionalErDirectScrutinySnapshot(
  source: RegionalErPipelineSource,
  options: {
    seed?: string | number;
  } = {}
): RegionalErDirectScrutinySnapshot {
  const votes = generateRegionalErVotes(source, { seed: options.seed });
  const rowsBySimulation = new Map<number, RegionalErScrutinyInputRow[]>();

  for (const row of votes) {
    const rows = rowsBySimulation.get(row.SIM) ?? [];
    rows.push(scrutinyRow(row));
    rowsBySimulation.set(row.SIM, rows);
  }

  return {
    schema_version: 1,
    source_schema_version: 1,
    source: 'generated Emilia-Romagna regional TypeScript pipeline',
    purpose: 'Direct-scrutiny snapshot produced by the regional vote generation pipeline.',
    simulations: [...rowsBySimulation.entries()]
      .sort(([left], [right]) => left - right)
      .map(([sim, rows]) => ({
        sim,
        input: {
          comuni_liste: rows
        }
      }))
  };
}
