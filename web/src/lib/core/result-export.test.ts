import { describe, expect, test } from 'vitest';
import { createDefaultPoliticsScenario } from '$lib/scenario/politics';
import type { ResultTable, SimulationResult } from './types';
import { createSimulationResultExport, parseSimulationResultExport, resultTablesToCsv } from './result-export';

function sampleResult(): SimulationResult {
  return {
    type: 'result',
    status: 'completed',
    tables: [
      {
        name: 'Summary',
        columns: ['Value'],
        rows: [{ Value: 1 }]
      }
    ],
    warnings: [
      {
        code: 'RUN_NOTE',
        electionKind: 'politiche',
        severity: 'info',
        message: 'Imported fixture note'
      }
    ],
    benchmark: {
      startedAt: '2026-06-04T00:00:00.000Z',
      elapsedMs: 12,
      simulations: 1,
      dataVersion: 'v1'
    }
  };
}

describe('result export helpers', () => {
  test('preserves execution resources and phase timings through import while rejecting invalid metadata', () => {
    const result = sampleResult();
    result.benchmark.execution = { requestedWorkers: 4, computeWorkers: 2, maximumWorkers: 5,
      estimatedWorkerMemoryMiB: 1536, estimateVersion: 'r26-worker-memory-v1',
      generationMs: 10, scrutinyMs: 5, presentationMs: 2, storageMs: 3 };
    const exported = createSimulationResultExport({ scenario: createDefaultPoliticsScenario(), result, exportedAt: '2026-10-20T00:00:00Z' });
    expect(parseSimulationResultExport(JSON.stringify(exported)).result.benchmark.execution).toEqual(result.benchmark.execution);
    for (const change of [{ computeWorkers: 6 }, { requestedWorkers: 0 }, { generationMs: -1 }, { maximumWorkers: 1.5 }]) {
      const invalid = structuredClone(exported);
      Object.assign(invalid.result.benchmark.execution!, change);
      expect(() => parseSimulationResultExport(JSON.stringify(invalid))).toThrow();
    }
  });
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
    const result = sampleResult();

    const payload = createSimulationResultExport({
      result,
      scenario,
      exportedAt: '2026-06-04T01:00:00.000Z'
    });

    result.tables[0].rows[0].Value = 2;
    scenario.name = 'Changed after export';

    expect(payload.schema_version).toBe(2);
    expect(payload.exportedAt).toBe('2026-06-04T01:00:00.000Z');
    expect(payload.result.tables[0].rows[0]).toEqual([1]);
    expect(payload.scenario.name).not.toBe('Changed after export');
  });

  test('creates compact columnar tables and parses the bundled JSON export payload', () => {
    const payload = createSimulationResultExport({
      result: sampleResult(),
      scenario: createDefaultPoliticsScenario(),
      exportedAt: '2026-06-04T01:00:00.000Z'
    });

    expect(payload.schema_version).toBe(2);
    expect(payload.result.tables[0].rows).toEqual([[1]]);
    expect(parseSimulationResultExport(JSON.stringify(payload))).toEqual({
      schema_version: 2,
      exportedAt: payload.exportedAt,
      scenario: payload.scenario,
      result: { ...sampleResult(), benchmark: { ...sampleResult().benchmark, scrutinyAlgorithmId: 'politiche-r-parity-v1' } }
    });
  });

  test('rejects result JSON without the generating scenario', () => {
    expect(() => parseSimulationResultExport(JSON.stringify(sampleResult()))).toThrow(
      /must include the scenario/
    );
  });

  test('rejects malformed result tables', () => {
    const payload = createSimulationResultExport({
      result: sampleResult(),
      scenario: createDefaultPoliticsScenario(),
      exportedAt: '2026-06-04T01:00:00.000Z'
    });

    expect(() =>
      parseSimulationResultExport(
        JSON.stringify({
          ...payload,
          result: {
            ...payload.result,
            tables: [
              {
                name: 'Broken',
                columns: ['Value'],
                rows: [[]]
              }
            ]
          }
        })
      )
    ).toThrow(/must have 1 cells, found 0/);
  });

  test('rejects unsupported result export schemas', () => {
    const payload = createSimulationResultExport({
      result: sampleResult(),
      scenario: createDefaultPoliticsScenario(),
      exportedAt: '2026-06-04T01:00:00.000Z'
    });

    expect(() => parseSimulationResultExport(JSON.stringify({ ...payload, schema_version: 999 }))).toThrow(
      /Unsupported result export schema_version/
    );
  });

  test('rejects schema-v1 result export payloads', () => {
    expect(() =>
      parseSimulationResultExport(
        JSON.stringify({
          schema_version: 1,
          exportedAt: '2026-06-04T01:00:00.000Z',
          scenario: createDefaultPoliticsScenario(),
          result: { ...sampleResult(), benchmark: { ...sampleResult().benchmark, scrutinyAlgorithmId: 'politiche-r-parity-v1' } }
        })
      )
    ).toThrow(/Unsupported result export schema_version: 1/);
  });
});
