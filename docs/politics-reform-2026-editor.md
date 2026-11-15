# Politics editor: user workflow

Open **Politiche** from the home page. The default selects reform/M02, both
chambers and ten simulations. The model loads automatically; its status and
preparation progress appear at the top of the editor.

1. Edit shared lists, coalitions, reference percentages, dates and chamber renewal.
   Empty references mean no override; explicit zero is retained. Expand each
   subject's options for filings, candidate pins and bonus links. **Attiva Altre liste**
   requires an explicit choice of free residual components; its identity is fixed
   and it receives no candidates or seats.
2. Choose **Algoritmo di scrutinio**. Under **Impostazioni avanzate dello scenario**,
   choose **Generatore dei voti**. Rosatellum supports M01; reform supports M01/M02.
   Each law remembers its model. Switching keeps shared and inactive values.
3. Review **Numero di simulazioni**, the visible fitting/simulation seeds and
   **Worker richiesti**. Automatic workers follow the resource estimate. Explicit
   counts retain CPU choices and show memory warnings. Each seed button changes
   only its field.
4. Select **Avvia simulazione**. Execution locks scenario edits. **Annulla esecuzione**
   stops dispatch and retains the assigned committed prefix. A successful ten-run
   session recommends 1,000 runs; it never launches them automatically.
5. Browse **Sessione dei risultati** independently of the draft. Tables and charts
   query saved data in bounded pages. Later edits mark results stale while
   preserving the executed scenario and seeds.
6. Export a scenario as JSON/JSON.gz, a result archive as JSON.gz, or tables as CSV.
   **Importa risultati** adds a session without replacing the draft. **Carica lo
   scenario di questa sessione** explicitly restores its scenario.

Under M02, **Fitting e checkpoint** permits refitting and exact checkpoint files.
Imported checkpoints bind the full scenario, implementation and dependencies.
Automatic fitting reuse can still ignore cosmetic/inactive edits.

**Ripristina scenario predefinito** asks for confirmation and replaces the draft;
saved results keep their identities. Reload restores the common draft and selected
session. Former politics draft keys are ignored and untouched. Old politics files
are rejected without changing current data. Regional routing/persistence remains
independent.

See the [component guide](common-politics-scenario/editor.md) for form ownership
and the [lifecycle guide](common-politics-scenario/lifecycle.md) for preparation,
resource limits and recovery. Deferred D14 fields survive validation, switching
and files; their observation/poll, supplied-input and model-specific editors
remain separate migration-plan tasks.
