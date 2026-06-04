import { describe, expect, test } from 'vitest';
import { createDefaultPoliticsScenario } from '$lib/scenario/politics';
import type { ResultTable, SimulationResult } from './types';
import { createSimulationResultExport, resultTablesToCsv } from './result-export';

describe('result export helpers', () => {
  test('serializes result tables to sectioned CSV with escaped cells', () => {
    const tables: ResultTable[] = [
      {
        name: 'Summary',
        columns: ['Lista', 'Note', 'Voti'],
        rows: [
          { Lista: 'Lista A', Note: 'plain', Voti: 10 },
          { Lista: 'Lista, B', Note: 'quote "and" newline\ninside', Voti: null }
        ]
      },
      {
        name: 'Warnings',
        columns: ['Code', 'Active'],
        rows: [{ Code: 'OK', Active: true }]
      }
    ];

    expect(resultTablesToCsv(tables)).toBe(
      [
        'Table,Summary',
        'Lista,Note,Voti',
        'Lista A,plain,10',
        '"Lista, B","quote ""and"" newline\ninside",',
        '',
        'Table,Warnings',
        'Code,Active',
        'OK,true'
      ].join('\r\n')
    );
  });

  test('creates a stable JSON export payload without sharing mutable references', () => {
    const scenario = createDefaultPoliticsScenario();
    const result: SimulationResult = {
      type: 'result',
      status: 'completed',
      tables: [
        {
          name: 'Summary',
          columns: ['Value'],
          rows: [{ Value: 1 }]
        }
      ],
      warnings: [],
      benchmark: {
        startedAt: '2026-06-04T00:00:00.000Z',
        elapsedMs: 12,
        simulations: 1,
        dataVersion: 'v1'
      }
    };

    const payload = createSimulationResultExport({
      result,
      scenario,
      exportedAt: '2026-06-04T01:00:00.000Z'
    });

    result.tables[0].rows[0].Value = 2;
    scenario.name = 'Changed after export';

    expect(payload.schema_version).toBe(1);
    expect(payload.exportedAt).toBe('2026-06-04T01:00:00.000Z');
    expect(payload.result.tables[0].rows[0].Value).toBe(1);
    expect(payload.scenario.name).not.toBe('Changed after export');
  });
});
