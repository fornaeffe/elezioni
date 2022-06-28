# 6. Qualora al termine delle operazioni di cui al comma 5 residuino
# ancora seggi da assegnare ad una lista, questi sono attribuiti ai
# candidati della lista nei collegi uninominali non proclamati eletti
# nelle altre circoscrizioni, secondo la graduatoria di cui
# all'articolo 77, comma 1, lettera h). A tale fine si procede con le
# modalita' previste dal comma 4.

donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  cat(
    "Devo ancora spostare ",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    " seggi della lista ",
    as.character(ammesse_pluri$LISTA[i]),
    " da ",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    ", ricorro ai candidati uninominali di altre circoscrizioni.\n",
    sep = ""
  )
  
  circ_accettrici <- aggregate(
    RESTO ~ CIRCOSCRIZIONE,
    ammesse_pluri[
      ammesse_pluri$LISTA == ammesse_pluri$LISTA[i] &
        !ammesse_pluri$DECIMALI_USATI,
    ],
    max
  )
  
  circ_accettrici <- circ_accettrici[order(
    circ_accettrici$RESTO,
    decreasing = TRUE
  ),]
  
  for (k in seq_along(circ_accettrici$CIRCOSCRIZIONE)) {
    
    accettori <- which(
      cifre_ind$CIRCOSCRIZIONE == circ_accettrici$CIRCOSCRIZIONE[k] &
        cifre_ind$ELETTO == FALSE &
        cifre_ind$CANDIDATO %in% cifre_uni$CANDIDATO[
          cifre_uni$LISTA == ammesse_pluri$LISTA[i]
        ]
    )
    
    accettori <- accettori[order(
      cifre_ind$CIFRA_PERCENTUALE[accettori],
      cifre_ind$DATA_NASCITA[accettori],
      decreasing = c(TRUE, TRUE)
    )]
    
    seggi_spostati <- min(
      ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
      length(accettori)
    )
    
    if (seggi_spostati < 1) next
    
    for (j in 1:seggi_spostati) {
      candidati_pluri <- rbind(
        candidati_pluri,
        data.frame(
          LISTA = ammesse_pluri$LISTA[i],
          CIRCOSCRIZIONE = ammesse_pluri$CIRCOSCRIZIONE[i],
          COLLEGIOPLURINOMINALE = ammesse_pluri$COLLEGIOPLURINOMINALE[i],
          NUMERO = 1 + max(candidati_pluri$NUMERO[
            candidati_pluri$LISTA == ammesse_pluri$LISTA[i] &
              candidati_pluri$COLLEGIOPLURINOMINALE == ammesse_pluri$COLLEGIOPLURINOMINALE[i]
          ]),
          CANDIDATO = cifre_ind$CANDIDATO[j]
        )
      )
      
      ammesse_pluri$CANDIDATI[i] <- ammesse_pluri$CANDIDATI[i] + 1
      ammesse_pluri$ELETTI[i] <- ammesse_pluri$ELETTI[i] + 1
      
      
      cat(
        "Ho ripescato",
        as.character(cifre_ind$CANDIDATO[accettori[j]]),
        "\n"
      )
    }
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
  
}

donatori <- which(ammesse_pluri$SEGGI - ammesse_pluri$ELETTI > 0)

for (i in donatori) {
  cat(
    "Devo ancora spostare ",
    ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
    " seggi della lista ",
    as.character(ammesse_pluri$LISTA[i]),
    " da ",
    as.character(ammesse_pluri$COLLEGIOPLURINOMINALE[i]),
    ", ricorro ai candidati uninominali di altre circoscrizioni.\n",
    sep = ""
  )
  
  circ_accettrici <- aggregate(
    RESTO ~ CIRCOSCRIZIONE,
    ammesse_pluri[
      ammesse_pluri$LISTA == ammesse_pluri$LISTA[i] &
        ammesse_pluri$DECIMALI_USATI,
    ],
    max
  )
  
  circ_accettrici <- circ_accettrici[order(
    circ_accettrici$RESTO,
    decreasing = TRUE
  ),]
  
  for (k in seq_along(circ_accettrici$CIRCOSCRIZIONE)) {
    
    accettori <- which(
      cifre_ind$CIRCOSCRIZIONE == circ_accettrici$CIRCOSCRIZIONE[k] &
        cifre_ind$ELETTO == FALSE &
        cifre_ind$CANDIDATO %in% cifre_uni$CANDIDATO[
          cifre_uni$LISTA == ammesse_pluri$LISTA[i]
        ]
    )
    
    accettori <- accettori[order(
      cifre_ind$CIFRA_PERCENTUALE[accettori],
      cifre_ind$DATA_NASCITA[accettori],
      decreasing = c(TRUE, TRUE)
    )]
    
    seggi_spostati <- min(
      ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i],
      length(accettori)
    )
    
    if (seggi_spostati < 1) next
    
    for (j in 1:seggi_spostati) {
      candidati_pluri <- rbind(
        candidati_pluri,
        data.frame(
          LISTA = ammesse_pluri$LISTA[i],
          CIRCOSCRIZIONE = ammesse_pluri$CIRCOSCRIZIONE[i],
          COLLEGIOPLURINOMINALE = ammesse_pluri$COLLEGIOPLURINOMINALE[i],
          NUMERO = 1 + max(candidati_pluri$NUMERO[
            candidati_pluri$LISTA == ammesse_pluri$LISTA[i] &
              candidati_pluri$COLLEGIOPLURINOMINALE == ammesse_pluri$COLLEGIOPLURINOMINALE[i]
          ]),
          CANDIDATO = cifre_ind$CANDIDATO[j]
        )
      )
      
      ammesse_pluri$CANDIDATI[i] <- ammesse_pluri$CANDIDATI[i] + 1
      ammesse_pluri$ELETTI[i] <- ammesse_pluri$ELETTI[i] + 1
      
      
      cat(
        "Ho ripescato",
        as.character(cifre_ind$CANDIDATO[accettori[j]]),
        "\n"
      )
    }
    
    if (ammesse_pluri$SEGGI[i] - ammesse_pluri$ELETTI[i] == 0) break
  }
  
}

# 7. Qualora al termine delle operazioni di cui al comma 6 residuino
# ancora seggi da assegnare ad una lista, questi sono attribuiti alle
# liste facenti parte della medesima coalizione della lista deficitaria
# nelle altre circoscrizioni. A tale fine si procede con le modalita'
# previste dai commi 4 e 5.
# 8. Nell'effettuare le operazioni di cui ai precedenti commi, in
# caso di parita' della parte decimale del quoziente, si procede
# mediante sorteggio.
# 9. Dell'avvenuta proclamazione effettuata ai sensi del presente
# articolo il presidente dell'Ufficio centrale circoscrizionale invia
# attestato ai deputati proclamati e ne da' immediata notizia alla
# segreteria generale della Camera dei deputati nonche' alle singole
# prefetture-uffici territoriali del Governo, che la portano a
# conoscenza del pubblico)).