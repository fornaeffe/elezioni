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
  .table-scroll { overflow: auto; }
  table { width: 100%; border-collapse: collapse; font-size: .9rem; }
  caption { text-align: left; font-weight: 650; padding: .75rem 0; }
  th, td { padding: .5rem; text-align: left; border-bottom: 1px solid #d7e0e5; max-width: 40rem; overflow-wrap: anywhere; }
  th { background: #f3f6f8; }
</style>
