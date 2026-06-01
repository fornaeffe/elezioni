import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname } from 'node:path';

const sourcePath = process.argv[2] ?? 'test/fixtures/politiche/debug_scrutinio.json';
const outputPath = process.argv[3] ?? 'web/static/data/v1/politics-debug-scrutiny.json';

const fixture = JSON.parse(readFileSync(sourcePath, 'utf8'));

const snapshot = {
  schema_version: 1,
  source_schema_version: fixture.metadata.schema_version,
  source: sourcePath,
  purpose:
    'Compact direct-scrutiny browser snapshot derived from the politics golden fixture. ' +
    'This is a bridge until scenario-to-vote generation is ported.',
  rami: Object.fromEntries(
    Object.entries(fixture.rami).map(([ramo, data]) => [
      ramo,
      {
        totali_pluri: data.totali_pluri,
        liste_naz: data.liste_naz,
        totale_seggi: data.totale_seggi,
        simulations: data.simulations.map((simulation) => ({
          sim: simulation.sim,
          input: simulation.input
        }))
      }
    ])
  )
};

mkdirSync(dirname(outputPath), { recursive: true });
writeFileSync(outputPath, `${JSON.stringify(snapshot)}\n`);

console.log(`Wrote politics worker snapshot: ${outputPath}`);
