<script lang="ts">
  import { Download, Upload, ChevronDown, ChevronUp } from '@lucide/svelte';
  import * as tokens from '$lib/design/tokens';
  import type { ResultTable } from '$lib/core/types';

  interface Props {
    hasResult: boolean;
    tables: ResultTable[];
    diagnosticTableNames?: Set<string>;
    internalTableNames?: Set<string>;
    showingDiagnostics: boolean;
    onDownloadJson: () => Promise<void>;
    onDownloadCsv: () => Promise<void>;
    onUploadResults: () => void;
    onToggleDiagnostics: () => void;
  }

  let {
    hasResult = false,
    tables = [],
    diagnosticTableNames = new Set(),
    internalTableNames = new Set(),
    showingDiagnostics = false,
    onDownloadJson,
    onDownloadCsv,
    onUploadResults,
    onToggleDiagnostics
  }: Props = $props();

  const primaryTables = $derived(
    tables
      .filter((t) => !diagnosticTableNames.has(t.name) && !internalTableNames.has(t.name))
  );

  const diagnosticTables = $derived(
    tables.filter((t) => diagnosticTableNames.has(t.name))
  );

  let busyDownloadJson = $state(false);
  let busyDownloadCsv = $state(false);

  async function handleDownloadJson() {
    busyDownloadJson = true;
    try {
      await onDownloadJson();
    } finally {
      busyDownloadJson = false;
    }
  }

  async function handleDownloadCsv() {
    busyDownloadCsv = true;
    try {
      await onDownloadCsv();
    } finally {
      busyDownloadCsv = false;
    }
  }
</script>

<div class="results-panel">
  <div class="panel-header">
    <h2>Risultati</h2>
    <div class="button-row">
      <button type="button" disabled={!hasResult || busyDownloadJson} onclick={handleDownloadJson}>
        <Download size={16} aria-hidden="true" />
        Scarica risultati
      </button>
      <button type="button" disabled={!hasResult || busyDownloadCsv} onclick={handleDownloadCsv}>
        <Download size={16} aria-hidden="true" />
        Scarica CSV
      </button>
      <button type="button" onclick={onUploadResults}>
        <Upload size={16} aria-hidden="true" />
        Importa
      </button>
    </div>
  </div>

  {#if primaryTables.length === 0}
    <p class="empty">Esegui una simulazione per vedere i risultati.</p>
  {:else}
    <div class="tables">
      {#each primaryTables as table (table.name)}
        <section class="table-section">
          <h3>{table.name}</h3>
          <div class="table-wrap">
            <table aria-label={table.name}>
              <thead>
                <tr>
                  {#each table.columns as col}
                    <th>{col}</th>
                  {/each}
                </tr>
              </thead>
              <tbody>
                {#each table.rows as row}
                  <tr>
                    {#each table.columns as col}
                      <td>{row[col] ?? ''}</td>
                    {/each}
                  </tr>
                {/each}
              </tbody>
            </table>
          </div>
        </section>
      {/each}
    </div>
  {/if}

  {#if diagnosticTables.length > 0}
    <button class="link-button" type="button" onclick={onToggleDiagnostics}>
      {showingDiagnostics ? 'Nascondi dettagli' : 'Mostra dettagli'}
      {showingDiagnostics ? <ChevronUp size={16} /> : <ChevronDown size={16} />}
    </button>

    {#if showingDiagnostics}
      <div class="diagnostics">
        {#each diagnosticTables as table (table.name)}
          <section class="table-section">
            <h3>{table.name}</h3>
            <div class="table-wrap">
              <table aria-label={table.name}>
                <thead>
                  <tr>
                    {#each table.columns as col}
                      <th>{col}</th>
                    {/each}
                  </tr>
                </thead>
                <tbody>
                  {#each table.rows as row}
                    <tr>
                      {#each table.columns as col}
                        <td>{row[col] ?? ''}</td>
                      {/each}
                    </tr>
                  {/each}
                </tbody>
              </table>
            </div>
          </section>
        {/each}
      </div>
    {/if}
  {/if}
</div>

<style>
  .results-panel {
    display: grid;
    gap: {tokens.spacing.lg};
  }

  .panel-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: {tokens.spacing.md};
    flex-wrap: wrap;
  }

  h2 {
    margin: 0;
    font-size: {tokens.typography.h2.fontSize};
    font-weight: {tokens.typography.h2.fontWeight};
  }

  h3 {
    margin: 0;
    font-size: {tokens.typography.h3.fontSize};
    font-weight: {tokens.typography.h3.fontWeight};
  }

  .button-row {
    display: flex;
    gap: {tokens.spacing.md};
    flex-wrap: wrap;
  }

  button {
    font: inherit;
    min-height: {tokens.sizing.buttonHeight};
    border: 1px solid {tokens.colors.border};
    border-radius: {tokens.borderRadius.medium};
    padding: {tokens.spacing.sm} {tokens.spacing.md};
    display: inline-flex;
    align-items: center;
    gap: {tokens.spacing.sm};
    color: {tokens.colors.textPrimary};
    background: {tokens.colors.background};
    cursor: pointer;
    transition: {tokens.transitions.normal};
  }

  button:hover:not(:disabled) {
    background: {tokens.colors.surface};
  }

  button:disabled {
    opacity: 0.55;
    cursor: not-allowed;
  }

  .link-button {
    border: 0;
    padding: 0;
    min-height: auto;
    background: transparent;
    color: {tokens.colors.primary};
    margin-top: {tokens.spacing.md};
  }

  .link-button:hover {
    text-decoration: underline;
    background: transparent;
  }

  .empty {
    color: {tokens.colors.textMuted};
    margin: {tokens.spacing.lg} 0;
  }

  .tables,
  .diagnostics {
    display: grid;
    gap: {tokens.spacing.lg};
  }

  .table-section {
    display: grid;
    gap: {tokens.spacing.md};
  }

  .table-wrap {
    overflow: auto;
    border: 1px solid {tokens.colors.border};
    border-radius: {tokens.borderRadius.medium};
  }

  table {
    width: 100%;
    border-collapse: collapse;
    font-size: {tokens.typography.bodySmall.fontSize};
  }

  th,
  td {
    padding: {tokens.spacing.md};
    border-bottom: 1px solid {tokens.colors.borderLighter};
    text-align: left;
    white-space: nowrap;
  }

  th {
    background: {tokens.colors.surfaceAlt};
    font-weight: 700;
  }

  tr:last-child td {
    border-bottom: 0;
  }
</style>
