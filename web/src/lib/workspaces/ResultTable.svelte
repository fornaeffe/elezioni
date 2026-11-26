<script lang="ts">
  import type { ResultTable } from '../core/types';
  let { table }: { table: ResultTable } = $props();
</script>
<div class="table-scroll">
  <table>
    <caption>{table.name}</caption>
    <thead><tr>{#each table.columns as column (column)}<th>{column}</th>{/each}</tr></thead>
    <tbody>{#each table.rows as row, i (i)}<tr>{#each table.columns as column (column)}<td>{row[column] ?? ''}</td>{/each}</tr>{/each}</tbody>
  </table>
</div>
<style>
  .table-scroll {
    overflow: auto;
    border: var(--result-table-border, 0);
    border-radius: var(--radius-panel);
  }
  table { width: 100%; border-collapse: collapse; font-size: .9rem; }
  caption { text-align: left; font-weight: 650; padding: .75rem 0; }
  th, td { padding: var(--space-2); text-align: start; border-block-end: 1px solid var(--color-border); max-width: 40rem; overflow-wrap: anywhere; }
  th { background: var(--color-surface-muted); }
</style>
