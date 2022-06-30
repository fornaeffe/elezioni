camera_pluri <- unique(camera[, c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE")])
camera_pluri <- camera_pluri[
  order(
    camera_pluri$CIRCOSCRIZIONE,
    camera_pluri$COLLEGIOPLURINOMINALE
  ),
]
write.csv2(camera_pluri, "camera_pluri.csv")

pluri <- merge(pluri, camera_pluri)

ammesse_circ <- liste_circ[
  liste_circ$SOGLIA3M,
  c(
    "CIRCOSCRIZIONE",
    "SOGGETTO_RIPARTO",
    "LISTA",
    "CIFRA"
  )
]

riparto_circ <- merge(
  riparto_circ,
  riparto_naz[,c("SOGGETTO_RIPARTO", "SEGGI_ECCEDENTI")]
)

riparto_circ$DEFICIT <- 
  riparto_circ$SEGGI_ECCEDENTI < 0 & !riparto_circ$SEGGIO_DA_RESTI

totali_circ <- merge(
  totali_circ,
  aggregate(
    DEFICIT ~ CIRCOSCRIZIONE,
    data = riparto_circ,
    sum
  )
)

totali_circ$DEFICIT_PRESENTE <- totali_circ$DEFICIT > 0

riparto_circ <- merge(
  riparto_circ,
  totali_circ[,c("CIRCOSCRIZIONE", "DEFICIT", "DEFICIT_PRESENTE")],
  by = "CIRCOSCRIZIONE",
  suffixes = c("","_CIRC")
)

riparto_circ$FLAG_ORDINE_ECC <- 
  riparto_circ$SEGGI_ECCEDENTI > 0 & riparto_circ$SEGGIO_DA_RESTI

riparto_circ <- riparto_circ[
  order(
    riparto_circ$FLAG_ORDINE_ECC,
    riparto_circ$SEGGI_ECCEDENTI,
    riparto_circ$CIFRA_NAZ,
    riparto_circ$DEFICIT_PRESENTE,
    riparto_circ$RESTO,
    decreasing = c(TRUE, TRUE, TRUE, TRUE, FALSE)
  ),
]

riparto_circ$ORDINE_ECC[riparto_circ$FLAG_ORDINE_ECC] <- ave(
  seq_along(riparto_circ$SOGGETTO_RIPARTO[riparto_circ$FLAG_ORDINE_ECC]),
  riparto_circ$SOGGETTO_RIPARTO[riparto_circ$FLAG_ORDINE_ECC],
  FUN = seq_along
)

riparto_naz$SEGGI_ECCEDENTI_CONTATORE <- riparto_naz$SEGGI_ECCEDENTI
totali_circ$DEFICIT_CONTATORE <- totali_circ$DEFICIT




riparto_circ$CIRCOSCRIZIONE %in% riparto_circ$CIRCOSCRIZIONE[
  riparto_circ$SOGGETTO_RIPARTO %in% riparto_naz$SOGGETTO_RIPARTO[
    riparto_naz$SEGGI_ECCEDENTI_CONTATORE < 0
  ] & ( riparto_circ$SEGGIO_DA_RESTI + riparto_circ$FLIPPER <= 0)
]
