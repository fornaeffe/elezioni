camera_pluri <- unique(camera[, c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE")])
camera_pluri <- camera_pluri[
  order(
    camera_pluri$CIRCOSCRIZIONE,
    camera_pluri$COLLEGIOPLURINOMINALE
  ),
]
write.csv2(camera_pluri, "camera_pluri.csv")

totali_pluri <- merge(totali_pluri, camera_pluri)

ammesse_circ <- cifre_circ[
  cifre_circ$SOGLIA3M,
  c(
    "CIRCOSCRIZIONE",
    "SOGGETTO_RIPARTO",
    "LISTA",
    "CIFRA"
  )
]