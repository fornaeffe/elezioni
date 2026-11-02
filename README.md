# elezioni

Simulatore delle elezioni italiane: applicazione web statica in SvelteKit e
TypeScript, con generazione dei voti e scrutinio eseguiti nel browser.

Sono disponibili le politiche con legge precedente, la proposta di riforma 2026
e le regionali Emilia-Romagna. La riforma simula la proposta documentata, con
interpretazioni e limiti espliciti. Il workflow comunale resta da implementare.
La pubblicazione del sito dipende dal controllo finale previsto nel
[piano di migrazione](MIGRATION_PLAN.md).

## Avvio locale

Servono **Node 24 e npm**. Dalla radice del repository:

```powershell
cd web
npm ci
npm run data:prepare -- --download
npm run dev
```

Aprire l'indirizzo locale indicato da Vite. La prima preparazione acquisisce
le fonti fissate dai manifest e genera tutti i dati richiesti. Con le fonti
già presenti, omettere `--download` per rigenerare offline.

Nell'app scegliere lo spazio di lavoro, modificare lo scenario, impostare
seme e numero di simulazioni, quindi avviare il calcolo. Esaminare risultati,
avvisi e limiti; usare esportazione/importazione per conservare e riprendere
scenari e risultati. Elaborazioni e archivi locali dipendono dal browser
e dal dispositivo usati.

## Sviluppo e verifica

Eseguire i comandi npm da `web/`. La [guida alla preparazione](docs/data-preparation.md)
descrive fonti, rigenerazione, controlli e riproduzione da un clone pulito.
La [guida agli spazi di lavoro](docs/web-workspaces.md) identifica moduli,
scenari predefiniti e punti di intervento.

```powershell
npm run data:check
npm run check:data
npm run check
npm test -- --maxWorkers=2
npm run build
npm run preview
```

Per la verifica completa usare la [procedura di accettazione](docs/politics-reform-2026-acceptance.md).
Build e test browser vanno eseguiti in sequenza, anche con `BASE_PATH=/elezioni`.
I dati di produzione generati e non tracciati sono obbligatori per l'accettazione.

Il codice applicativo resta in `web/`; `data/` contiene fonti e preparazione
condivise, `docs/` le specifiche, `test/fixtures/` i riferimenti di regressione
e `research/` gli studi separati dalla produzione. R, RStudio e Quarto non sono
necessari: implementazione, cache e scenari Excel precedenti sono stati ritirati.
I riferimenti JSON congelati rimangono per verificare lo scrutinio; il codice
storico è disponibile in Git, nella revisione indicata nella
[cronologia](docs/migration-history.md#2026-11-02--r-and-temporary-bridge-retirement).

## Modelli di generazione della riforma

- [M01 2.0.0 — corrispondenze e shock per lista](docs/models/M01.md).
- [M02 2.0.0 — stima congiunta e trasferimenti limitati](docs/models/M02.md), predefinito.

Le specifiche includono equazioni, assunzioni e regole di versionamento. Dati,
numerica e scrutinio legale hanno identità separate; i vecchi identificatori
rimangono nei risultati storici. Attività aperte e verifiche effettuate sono
nel [piano](MIGRATION_PLAN.md) e nella [cronologia](docs/migration-history.md).
