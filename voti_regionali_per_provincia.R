liste_comune <- read.csv2(
  "dati_amministrative/regionali-20200126.txt",
  fileEncoding = "utf-8"
)

liste_provincia <- aggregate(
  VOTI_LISTA ~ PROVINCIA + LISTA,
  liste_comune,
  sum,
  subset = REGIONE == "EMILIA-ROMAGNA"
)

write.csv2(liste_provincia, file = "output/voti_regionali_per_provincia.csv")
