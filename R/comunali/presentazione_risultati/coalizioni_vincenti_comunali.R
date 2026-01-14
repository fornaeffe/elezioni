coalizioni_vincenti_comunali <- function(
    risultato
){
  vittoria <- risultato$coalizioni_sim[
    ,
    .(SINDACO = mean(SINDACO)),
    by = .(COALIZIONE)
  ]
  
  vittoria$SINDACO <- formattable::percent(vittoria$SINDACO, 0)
  
  
  tbl <- kableExtra::kbl(
    vittoria,
    col.names = c("CANDIDATO SINDACO", "Prob."),
    caption = "Probabilità di vittoria",
    row.names = FALSE
  )
  
  tbl <- kableExtra::kable_minimal(tbl)
  
  tbl
}