# elezioni
Simulatore per le elezioni politiche italiane

Lo script principale è elezioni.Rmd.

Questo script va a cercare alcuni dati:

- elenco di liste che si presenteranno alle elezioni e loro percentuali, dalla 
cartella dati_2023
- risultati delle passate elezioni, dalle cartelle dati_2018 e 
dati_amministrative
- dati relativi ai collegi elettorali, dalla cartella dati_collegi

in base a questi dati, simula i voti ricevuti da ogni lista in ogni collegio,
dopodiché chiama la funzione Scrutinio, presente nel file scrutinio.R, per 
stabilire i seggi attribuiti a ogni coalizione e lista in base ai voti ottenuti.

Infine presenta i risultati in un report chiamato elezioni.html.

Oltre a questo script sono presenti script secondari, in particolare:

- elaborazione_2018_camera.R ed elaborazione_2018_senato.R utilizzano la 
funzione Scrutinio sui risultati delle precedenti elezioni politiche, per 
verificare la correttezza dei risultati.
- conversione_collegi.R è uno script che, a partire dalla base di dati della
proposta della Commissione per la definizione dei nuovi collegi elettorali 
(ricavata dalla pagina 
<https://www.riformeistituzionali.gov.it/it/i-nuovi-collegi-elettorali/>)
prepara i dataframe relativi ai collegi di Camera e Senato, e in particolare
stima il numero di seggi nei collegi plurinominali.

Dati sugli scrutini delle elezioni 2018 da 
https://github.com/ondata/elezionipolitiche2018
