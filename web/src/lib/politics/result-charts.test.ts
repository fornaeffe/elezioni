import { describe, expect, test } from 'vitest';
import type { ResultTable, Scenario } from '$lib/core/types';
import { createDefaultPoliticsScenario } from '$lib/scenario/politics';
import {
  buildPoliticsPlurinominalChart,
  buildPoliticsPlurinominalChartOptions,
  buildPoliticsResultCharts
} from './result-charts';

function scenario(): Scenario {
  const base = createDefaultPoliticsScenario();
  return {
    ...base,
    coalitions: [
      { id: 'ca', name: 'Coalizione A', color: '#884422' },
      { id: 'cb', name: 'Coalizione B', color: '#226688' }
    ],
    lists: [
      { id: 'a', name: 'Lista A', coalition: 'Coalizione A', color: '#112233', startingShare: 60, shareOverride: false },
      { id: 'b', name: 'Lista B', coalition: 'Coalizione B', color: 'bad-color', startingShare: 40, shareOverride: false }
    ]
  };
}

function tables(): ResultTable[] {
  return [
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
    },
    {
      name: 'List seat-vote plot data',
      columns: ['Ramo', 'Sim', 'Lista', 'Percentuale %', 'Seggi'],
      rows: [
        { Ramo: 'camera', Sim: 1, Lista: 'Lista A', 'Percentuale %': 60, Seggi: 4 },
        { Ramo: 'camera', Sim: 2, Lista: 'Lista A', 'Percentuale %': 62, Seggi: 5 },
        { Ramo: 'camera', Sim: 1, Lista: 'Lista B', 'Percentuale %': 40, Seggi: 2 },
        { Ramo: 'camera', Sim: 2, Lista: 'Lista B', 'Percentuale %': 38, Seggi: 1 }
      ]
    },
    {
      name: 'Coalition seat-vote plot data',
      columns: ['Ramo', 'Sim', 'Soggetto', 'Percentuale liste %', 'Seggi'],
      rows: [
        { Ramo: 'camera', Sim: 1, Soggetto: 'Coalizione A', 'Percentuale liste %': 60, Seggi: 8 },
        { Ramo: 'camera', Sim: 2, Soggetto: 'Coalizione A', 'Percentuale liste %': 62, Seggi: 9 }
      ]
    },
    {
      name: 'Plurinominal seat-vote plot data',
      columns: ['Ramo', 'Sim', 'Circoscrizione', 'Collegio pluri', 'Lista', 'Percentuale %', 'Numero max'],
      rows: [
        {
          Ramo: 'camera',
          Sim: 1,
          Circoscrizione: '1',
          'Collegio pluri': '10',
          Lista: 'Lista A',
          'Percentuale %': 60,
          'Numero max': 1
        },
        {
          Ramo: 'camera',
          Sim: 2,
          Circoscrizione: '1',
          'Collegio pluri': '10',
          Lista: 'Lista A',
          'Percentuale %': 62,
          'Numero max': 2
        },
        {
          Ramo: 'camera',
          Sim: 1,
          Circoscrizione: '1',
          'Collegio pluri': '10',
          Lista: 'Lista B',
          'Percentuale %': 40,
          'Numero max': 0
        }
      ]
    }
  ];
}

describe('politics result charts', () => {
  test('builds sorted summary bars and R-style distribution charts from result tables', () => {
    const charts = buildPoliticsResultCharts(tables(), scenario());

    expect(charts.map((chart) => [chart.id, chart.kind])).toEqual([
      ['plurinominal-seats-by-list', 'bar'],
      ['vote-share-by-list', 'bar'],
      ['vote-share-boxplot', 'boxplot'],
      ['seat-vote-scatter-by-list', 'scatter'],
      ['seat-vote-scatter-by-coalition', 'scatter'],
      ['seat-vote-spine-by-list', 'spine']
    ]);

    const seatBars = charts[0];
    expect(seatBars.kind).toBe('bar');
    if (seatBars.kind !== 'bar') return;
    expect(seatBars.groups[0]).toEqual({
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

    const boxplot = charts.find((chart) => chart.id === 'vote-share-boxplot');
    expect(boxplot?.kind).toBe('boxplot');
    if (!boxplot || boxplot.kind !== 'boxplot') return;
    expect(boxplot.panels[0].items[0].label).toBe('Lista A');
    expect(boxplot.panels[0].items[0].median).toBe(61);

    const listScatter = charts.find((chart) => chart.id === 'seat-vote-scatter-by-list');
    expect(listScatter?.kind).toBe('scatter');
    if (!listScatter || listScatter.kind !== 'scatter') return;
    expect(listScatter.panels[0].label).toBe('camera - Lista A');
    expect(listScatter.panels[0].points.map((point) => [point.x, point.y])).toEqual([
      [60, 4],
      [62, 5]
    ]);
  });

  test('builds selectable plurinominal spine charts', () => {
    const options = buildPoliticsPlurinominalChartOptions(tables(), scenario());

    expect(options).toEqual([
      {
        id: 'camera\u001fLista A\u001f1\u001f10',
        ramo: 'camera',
        list: 'Lista A',
        circoscrizione: '1',
        collegioPlurinominale: '10',
        label: 'camera - Lista A - 10'
      }
    ]);

    const chart = buildPoliticsPlurinominalChart(tables(), scenario(), options[0]);
    expect(chart?.kind).toBe('spine');
    expect(chart?.panels[0].label).toBe('camera - Lista A - 10');
    expect(chart?.panels[0].bins.flatMap((bin) => bin.cells.map((cell) => cell.seats))).toEqual([1, 2]);
  });

  test('returns no charts when result tables are missing', () => {
    expect(buildPoliticsResultCharts([], scenario())).toEqual([]);
    expect(buildPoliticsPlurinominalChartOptions([], scenario())).toEqual([]);
  });
});
