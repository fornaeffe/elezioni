# elezioni

Simulatore delle elezioni italiane: applicazione web statica in SvelteKit e
TypeScript, con generazione dei voti e scrutinio eseguiti nel browser.

Sono disponibili le simulazioni delle elezioni politiche con lo "Stabilicum"
(Legge 9 ottobre 2026, n. 180), il "Rosatellum" (Legge 3 novembre 2017, n. 165)
e le regionali Emilia-Romagna.

## Avvio locale

Servono **Node 24 e npm**. Dopo aver clonato il repository, dalla root:

```powershell
cd web
npm ci
npm run data:prepare -- --download
npm run dev
```

Aprire in un browser l'indirizzo locale indicato da Vite.

La prima preparazione (`data:prepare`) acquisisce dal web
le fonti fissate dai manifest e genera tutti i dati richiesti. Con le fonti
già presenti, omettere `--download` per rigenerare offline il dataset.

Nell'app scegliere lo spazio di lavoro, modificare lo scenario se necessario,
impostare il numero di simulazioni, quindi avviare il calcolo. Esaminare
risultati e avvisi; usare esportazione/importazione per conservare e riprendere
scenari e risultati.

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
I dati di produzione generati e non tracciati sono necessari per l'accettazione.
La [guida all'hosting](docs/hosting.md) descrive verifica Linux, misure del sito
e pubblicazione esplicita su GitHub Pages dopo il merge in `main`.

Il codice applicativo resta in `web/`; `data/` contiene fonti e preparazione
condivise, `docs/` le specifiche, `test/fixtures/` i riferimenti di regressione
e `research/` gli studi separati dalla produzione.

Attività aperte e verifiche effettuate sono
nel [TODO](TODO.md) e nella [cronologia](docs/history/README.md).

## Modelli di generazione dei voti

- [M01 2.0.0 — corrispondenze e shock per lista](docs/models/M01.md).
- [M02 3.0.0 — campo politico condiviso e trasferimenti limitati](docs/models/M02.md), predefinito.

Le specifiche includono equazioni, assunzioni e regole di versionamento. Dati,
numerica e scrutinio legale hanno identità separate.
