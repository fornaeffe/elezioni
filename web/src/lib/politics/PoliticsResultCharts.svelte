<script lang="ts">
  import ResultCharts from '../results/ResultCharts.svelte';
  import type { PoliticsPlurinominalChartOption, PoliticsResultChart, PoliticsSpineResultChart } from './result-charts';
  let props: { charts: PoliticsResultChart[]; plurinominalOptions: PoliticsPlurinominalChartOption[];
    selectedPlurinominalOption: PoliticsPlurinominalChartOption | null; plurinominalChart: PoliticsSpineResultChart | null;
    onSelectPlurinominalOption: (id: string) => void } = $props();
</script>

<ResultCharts {...props} />
