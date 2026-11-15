import { expectStatusError, openCommon, openResultTransfers, runCommon, selectedCommon } from '../fixtures/common-page';
import { ROSATELLUM } from '../../src/lib/politics/scenario/schema';
import { expect, test } from '@playwright/test';
import { gzipSync } from 'node:zlib';

function bundledResultJson(): string {
  return JSON.stringify(
    {
      schema_version: 2,
      exportedAt: '2026-06-05T10:00:00.000Z',
      scenario: {
        id: 'imported-scenario',
        name: 'Imported Scenario',
        electionDate: '2027-05-01',
        defaultSource: {
          kind: 'manual',
          electionKind: 'politiche',
          territory: 'Italia',
          dataVersion: 'v1'
        },
        globalShareMode: 'mean',
        abstentionShare: 40,
        abstentionOverride: false,
        coalitions: [{ id: 'coal-a', name: 'Coalizione A', color: '#224466' }],
        lists: [
          {
            id: 'list-a',
            name: 'Lista A',
            coalition: 'Coalizione A',
            color: '#aa2222',
            startingShare: 50,
            shareOverride: true
          }
        ],
        listCorrespondences: [],
        localShareOverrides: [],
        candidateTemplates: []
      },
      result: {
        type: 'result',
        status: 'completed',
        tables: [
          {
            name: 'Election overview',
            columns: ['Ramo', 'Simulazioni'],
            rows: [['camera', 1]]
          }
        ],
        warnings: [],
        benchmark: {
          startedAt: '2026-06-05T09:59:00.000Z',
          elapsedMs: 42,
          simulations: 1,
          dataVersion: 'v1'
        }
      }
    },
    null,
    2
  );
}

test.setTimeout(180000);
for (const input of ['Importa risultati', 'Carica scenario']) {
  test(`rejects old politics archives from ${input} without changing the draft or results`, async ({page})=>{
    await openCommon(page, ROSATELLUM, 'M01');
    const before=await runCommon(page);
    await page.getByLabel('Nome scenario',{exact:true}).fill('Current draft'); await page.getByLabel('Nome scenario',{exact:true}).blur();
    if (input === 'Importa risultati') await openResultTransfers(page);
    await page.getByLabel(input,{exact:true}).setInputFiles({name:'old.json.gz',mimeType:'application/gzip',buffer:gzipSync(Buffer.from(bundledResultJson()))});
    await expectStatusError(page, /precedente non supportato/);
    await expect(page.getByLabel('Nome scenario',{exact:true})).toHaveValue('Current draft');
    expect((await selectedCommon(page)).id).toBe(before.id);
  });
}
