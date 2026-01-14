superamento_soglia_comunali <- function(
    risultato
) {
  soglie <- risultato$liste_sim[
    ,
    .(PROB_ELEGGERE = mean(ELETTI > 0)),
    by = .(LISTA)
  ]
  soglie_coalizioni <- risultato$coalizioni_sim[
    ,
    .(
      PROB_ELEGGERE = mean(SINDACO + SEGGIO_CANDIDATO_SINDACO),
      LISTA = paste("Cand. sindaco", COALIZIONE)
    ),
    by = .(COALIZIONE)
  ]
  
  soglie <- rbind(
    soglie[, .(LISTA, PROB_ELEGGERE)],
    soglie_coalizioni[, .(LISTA, PROB_ELEGGERE)]
  )
  
  soglie$PROB_ELEGGERE <- formattable::percent(soglie$PROB_ELEGGERE, 0)
  tbl <- kableExtra::kbl(
    soglie,
    col.names = c("LISTA", "Prob."),
    caption = "Prob. di eleggere qualcuno",
    row.names = FALSE
  )
  tbl <- kableExtra::kable_minimal(tbl)
  tbl
}