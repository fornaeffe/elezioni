import { describe, expect, test } from 'vitest';
import type { ResultTable, Scenario } from '$lib/core/types';
import { createDefaultPoliticsScenario } from '$lib/scenario/politics';
import { buildPoliticsResultCharts } from './result-charts';

function scenario(): Scenario {
  const base = createDefaultPoliticsScenario();
  return {
    ...base,
    lists: [
      { id: 'a', name: 'Lista A', coalition: null, color: '#112233', startingShare: 60, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: null, color: 'bad-color', startingShare: 40, shareOverride: false }
    ]
  };
}

describe('politics result charts', () => {
  test('builds sorted list charts from result tables', () => {
    const tables: ResultTable[] = [
      {
        name: 'Average plurinominal seats by list',
        columns: ['Ramo', 'Lista', 'Media'],
        rows: [
          { Ramo: 'camera', Lista: 'Lista B', Media: 2 },
          { Ramo: 'camera', Lista: 'Lista A', Media: 4 },
          { Ramo: 'senato', Lista: 'Lista A', Media: 1 }
        ]
      },
      {
        name: 'Vote share by list',
        columns: ['Ramo', 'Lista', 'Media %'],
        rows: [
          { Ramo: 'camera', Lista: 'Lista A', 'Media %': 60 },
          { Ramo: 'camera', Lista: 'Lista B', 'Media %': 40 }
        ]
      }
    ];

    const charts = buildPoliticsResultCharts(tables, scenario());

    expect(charts.map((chart) => chart.id)).toEqual(['plurinominal-seats-by-list', 'vote-share-by-list']);
    expect(charts[0].groups[0]).toEqual({
      label: 'camera',
      bars: [
        {
          label: 'Lista A',
          value: 4,
          displayValue: '4',
          color: '#112233',
          widthPercent: 100
        },
        {
          label: 'Lista B',
          value: 2,
          displayValue: '2',
          color: '#5f7f92',
          widthPercent: 50
        }
      ]
    });
    expect(charts[1].groups[0].bars.map((bar) => [bar.label, bar.displayValue, bar.widthPercent])).toEqual([
      ['Lista A', '60%', 100],
      ['Lista B', '40%', 66.66666666666666]
    ]);
  });

  test('returns no charts when result tables are missing', () => {
    expect(buildPoliticsResultCharts([], scenario())).toEqual([]);
  });
});
