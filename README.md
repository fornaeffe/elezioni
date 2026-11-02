# elezioni
Simulatore per le elezioni italiane (politiche, comunali e regionali)

In futuro ho intenzione di trasformarlo in un pacchetto, per ora per usarlo
bisogna arrangiarsi un po', usando i file .qmd come esempio.

Gli script necessari al funzionamento si trovano nella cartella R/.

Il workflow tipico per l'utilizzo di questi script è:

1 - Caricamento dei dati (funzione carica_dati() in R/caricamento_dati.R):
la funzione scarica i dati necessari da internet, li salva in un file di cache
(consiglio di indicare un percorso permanente per il salvataggio così da non
doverli scaricare ogni volta) e li restituisce come lista di data.table
2 - Creazione e modifica manuale di un file Excel di scenario (R/creazione_file_scenario.R)
3 - Calcolo dei parametri di input (funzione calcola_parametri_input() in R/generazione_voti.R):
la funzione, partendo dai dati e dal file di scenario, calcola le percentuali
di partenza di ogni lista in ogni comune, e la loro variabilità.
4 - Generazione dei voti simulati (funzione genera_voti() in R/generazione_voti.R)
5 - Scrutinio dei voti simulati (ogni tipo di elezione ha il suo file R/scrutinio_...)
6 - Presentazione grafica dei risultati con le varie funzioni in R/presentazione_risultati.R.

Quasi tutti i dati necessari dovrebbero essere scaricati automaticamente da carica_dati(),
ad eccezione della base dati dei collegi delle elezioni politiche: in questo caso è
necessario caricare manualmente un file csv "BaseDati_Proposta_Commissione.csv"
ottenuto esportando l'omonima tabella dal file http://www.riformeistituzionali.gov.it/media/1367/proposta_commissione_13112020.zip

## Modelli di generazione della riforma web

- [M01 2.0.0 — corrispondenze e shock per lista](docs/models/M01.md): modello per lista ripristinato allo stato precedente a `4ee5a91`.
- [M02 2.0.0 — stima congiunta e trasferimenti limitati](docs/models/M02.md): modello predefinito.

Le specifiche includono equazioni, assunzioni e regole di versionamento. I vecchi
identificatori rimangono nei risultati storici; dati, numerica e scrutinio legale
hanno identità separate. Per il workflow web vedere [MIGRATION_PLAN.md](MIGRATION_PLAN.md).
