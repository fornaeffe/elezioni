#!/usr/bin/env node

import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname } from 'node:path';

const inputPath = process.argv[2] ?? 'web/static/data/v1/politics-pipeline-source-debug.json';
const outputPath = process.argv[3] ?? 'web/static/data/v1/politics-static-debug.json';

const input = JSON.parse(readFileSync(inputPath, 'utf8'));
const source = input.source;

if (!source) {
  throw new Error(`Missing source in ${inputPath}`);
}

const snapshot = {
  metadata: {
    schema_version: 1,
    source: input.metadata?.source ?? inputPath,
    created: new Date().toISOString(),
    purpose:
      'Production-shaped politics static snapshot bridge. Reusable election data is split from the default scenario; still derived from the debug source until production data packaging is implemented.'
  },
  data: {
    base_dati: source.base_dati,
    camera: {
      uni: source.camera.uni,
      pluri: source.camera.pluri
    },
    senato: {
      uni: source.senato.uni,
      pluri: source.senato.pluri
    }
  },
  default_scenario: {
    id: 'politiche-2027-debug-default',
    name: 'Politiche 2027 debug default',
    data_elezione: source.data_elezione,
    frazione_uni_in_pluri: source.frazione_uni_in_pluri,
    frazioni_pluricandidature: source.frazioni_pluricandidature,
    default_data_nascita: source.default_data_nascita,
    liste: source.liste,
    comuni_liste: source.comuni_liste,
    camera: {
      candidati_uni: source.camera.candidati_uni,
      candidati_pluri: source.camera.candidati_pluri
    },
    senato: {
      candidati_uni: source.senato.candidati_uni,
      candidati_pluri: source.senato.candidati_pluri
    }
  }
};

mkdirSync(dirname(outputPath), { recursive: true });
writeFileSync(outputPath, `${JSON.stringify(snapshot)}\n`);
console.log(`Wrote politics static snapshot: ${outputPath}`);
