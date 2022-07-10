## TODO: aggiungere area "Altro/Astensione"

softmax <- function(x) exp(x) / sum(exp(x))

#### Parametri ####

# Scenario
scenario <- "01"

# Numero di iterazioni
iterazioni <- 200

# Variabilità dei voti
variab <- 0.5


#### Carico librerie e script ####

library(data.table)
library(readxl)
library(stringr)
source("scrutinio.R")

#### Importazione dati ####

##### Base dati #####

base_dati <- read.csv2(
  "dati_collegi/BaseDati_Proposta_Commissione.csv",
  fileEncoding = "utf-8",
  colClasses = c(
    CIRCOCAM_20_DEN = "factor",
    CP20_DEN = "factor",
    CU20_DEN = "factor"
  ),
  na.strings = ""
)

base_dati$SP20_DEN[base_dati$DEN_REG20 == "Trentino-Alto Adige"] <- 
  "Trentino-Alto Adige/Südtirol - P01"

names(base_dati)[names(base_dati) == "DEN_PRO_CM20"] <- "PROVINCIA"
names(base_dati)[names(base_dati) == "DEN_COM20"] <- "COMUNE"

base_dati$PROVINCIA <- toupper(base_dati$PROVINCIA)
base_dati$COMUNE <- toupper(base_dati$COMUNE)

province <- aggregate(
  POP_2011 ~ PROVINCIA,
  base_dati,
  sum
)


comuni <- unique(base_dati[, c("PROVINCIA", "COMUNE")])

##### Politiche #####

camera_2018 <- read.csv2(
  "dati_2018/camera-20180304_2.txt",
  fileEncoding = "utf-8"
)

camera_2018$PROV_TEMP <- str_remove(camera_2018$COLLEGIOUNINOMINALE, "\\A[0-9]{2} (- )?")
camera_2018$PROV_TEMP <- str_remove(camera_2018$PROV_TEMP, " - .*\\Z")
camera_2018$PROV_TEMP <- str_remove(camera_2018$PROV_TEMP, " AREA STATISTICA .*\\Z")

camera_2018 <- merge(
  camera_2018,
  comuni,
  by.x = "PROV_TEMP",
  by.y = "COMUNE",
  all.x = TRUE
)

camera_2018$PROVINCIA[camera_2018$PROV_TEMP == ""] <- "AOSTA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "BOLZANO/BOZEN"] <- "BOLZANO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "BRESSANONE/BRIXEN"] <- "BOLZANO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "CANT+"] <- "COMO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "CORIGLIANO CALABRO"] <- "COSENZA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "FORL¦"] <- "FORLI'-CESENA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "MERANO/MERAN"] <- "BOLZANO"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "NARDÊ"] <- "LECCE"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "PATERNÊ"] <- "CATANIA"
camera_2018$PROVINCIA[camera_2018$PROV_TEMP == "SAN DONA' DI PIAVE"] <- "VENEZIA"

# Checks
setdiff(unique(camera_2018$PROVINCIA), province$PROVINCIA)
sum(is.na(camera_2018$PROVINCIA))

##### Amministrative #####

lista_files <- list.files("dati_amministrative")

lista_dataframes <- lapply(
  paste0("dati_amministrative/", lista_files),
  read.csv2,
  fileEncoding = "utf-8"
)

lista_dataframes <- mapply(
  function(df, nome_file) {
    names(df) <- toupper(names(df))
    df$ANNO <- substr(nome_file, 11, 14)
    df$MESE <- substr(nome_file, 15, 16)
    df
  },
  df = lista_dataframes,
  nome_file = lista_files,
  SIMPLIFY = FALSE
)

amministrative <- rbindlist(lista_dataframes)
lista_dataframes <- NULL

amministrative$PROVINCIA[amministrative$PROVINCIA == "REGGIO NELL' EMILIA"] <- "REGGIO NELL'EMILIA"

# Checks
setdiff(unique(amministrative$PROVINCIA), province$PROVINCIA)

# Questo è servito per esportare i nomi delle liste
# write.csv2(
#   amministrative[!duplicated(amministrative$LISTA), ],
#   "output/liste.csv",
#   fileEncoding = "utf-8"
# )

##### Corrispondenza liste - aree #####

liste <- read_xlsx("sandbox/liste.xlsx", "aree")
liste <- liste[
  !duplicated(liste$LISTA) & !is.na(liste$LISTA) & !is.na(liste$AREA)
  ,]

##### Liste nazionali e collegi ######

liste_naz <- read_xlsx(paste0("dati_2023/", scenario, ".xlsx"), "liste_naz")
liste_naz$CL <- as.character(liste_naz$COALIZIONE)
liste_naz$CL[is.na(liste_naz$CL)] <- 
  as.character(liste_naz$LISTA[is.na(liste_naz$CL)])

altri_dati <- read_xlsx(paste0("dati_2023/", scenario, ".xlsx"), "altri_dati")

load("dati_collegi/collegi.RData")

#### Calcolo distribuzione spaziale elettori di area ####

camera_2018$VOTAZIONE <- "politiche 2018"
amministrative$VOTAZIONE <- paste("regionali", amministrative$ANNO, amministrative$MESE)

camera_2018$AREA <- factor(camera_2018$LISTA, levels = liste$LISTA, labels = liste$AREA)
amministrative$AREA <- factor(amministrative$LISTA, levels = liste$LISTA, labels = liste$AREA)

prov_area_voto <- rbind(
  aggregate(
    VOTI_LISTA ~ PROVINCIA + AREA + VOTAZIONE,
    camera_2018,
    sum
  ),
  aggregate(
    VOTI_LISTA ~ PROVINCIA + AREA + VOTAZIONE,
    amministrative,
    sum
  )
)

prov_area <- aggregate(
  VOTI_LISTA ~ PROVINCIA + AREA,
  prov_area_voto,
  sum
)

prov_area <- merge(
  prov_area,
  aggregate(
    VOTI_LISTA ~ PROVINCIA,
    prov_area,
    sum
  ),
  by = "PROVINCIA",
  suffixes = c("", "_TOT")
)

prov_area$PERCENTUALE_STORICA <- prov_area$VOTI_LISTA / prov_area$VOTI_LISTA_TOT

prov_area <- merge(
  prov_area,
  province
)

prov_area$POP_AREA <- prov_area$PERCENTUALE_STORICA * prov_area$POP_2011

aree <- aggregate(
  POP_AREA ~ AREA,
  prov_area,
  sum
)

prov_area <- merge(
  prov_area,
  aree,
  by = "AREA",
  suffixes = c("", "_TOT")
)

prov_area$PERCENTUALE_AREA <- prov_area$POP_AREA / prov_area$POP_AREA_TOT

#### Calcolo percentuali per provincia ####

votanti <- sum(province$POP_2011) * (1 - altri_dati$Astensione)

aree <- merge(
  aree,
  aggregate(
    PERCENTUALE ~ AREA,
    liste_naz,
    sum
  )
)

aree$VOTANTI <- aree$PERCENTUALE * votanti

prov_area <- merge(
  prov_area,
  aree[,c("AREA", "VOTANTI")]
)

prov_area$VOTANTI_LOCALI <- prov_area$VOTANTI * prov_area$PERCENTUALE_AREA

prov_area <- merge(
  prov_area,
  aggregate(
    VOTANTI_LOCALI ~ PROVINCIA,
    prov_area,
    sum
  ),
  by = "PROVINCIA",
  suffixes = c("", "_TOT")
)

prov_area$PERCENTUALE <- prov_area$VOTANTI_LOCALI / prov_area$VOTANTI_LOCALI_TOT

comuni_aree <- merge(
  base_dati,
  aree[c("AREA", "PERCENTUALE")]
)

comuni_aree <- merge(
  comuni_aree,
  prov_area[, c("PROVINCIA", "AREA", "PERCENTUALE")],
  all.x = TRUE,
  by = c("PROVINCIA", "AREA"),
  suffixes = c("", "_PROV")
)

comuni_aree$PERCENTUALE[!is.na(comuni_aree$PERCENTUALE_PROV)] <-
  comuni_aree$PERCENTUALE_PROV[!is.na(comuni_aree$PERCENTUALE_PROV)]

comuni_aree$VOTANTI <- 
  comuni_aree$POP_2011 * (1 - altri_dati$Astensione) * comuni_aree$PERCENTUALE

camera$aree_uni <- aggregate(
  cbind(VOTANTI, POP_2011) ~ CIRCOCAM_20_DEN + CP20_DEN + CU20_DEN + AREA,
  comuni_aree,
  sum
)

names(camera$aree_uni)[1] <- "CIRCOSCRIZIONE"
names(camera$aree_uni)[2] <- "COLLEGIOPLURINOMINALE"
names(camera$aree_uni)[3] <- "COLLEGIOUNINOMINALE"

senato$aree_uni <- aggregate(
  cbind(VOTANTI, POP_2011) ~ CIRCOCAM_20_DEN + CP20_DEN + CU20_DEN + AREA,
  comuni_aree,
  sum
)

names(senato$aree_uni)[1] <- "CIRCOSCRIZIONE"
names(senato$aree_uni)[2] <- "COLLEGIOPLURINOMINALE"
names(senato$aree_uni)[3] <- "COLLEGIOUNINOMINALE"

liste_naz <- merge(
  liste_naz,
  aree[, c("AREA", "PERCENTUALE")],
  by = "AREA",
  suffixes = c("", "_AREA")
)

liste_naz$PERC_IN_AREA <- liste_naz$PERCENTUALE / liste_naz$PERCENTUALE_AREA

simula <- function(
    ramo,
    scenario,
    dati,
    liste_naz,
    
    iterazioni = 200,
    
    sd_naz = .4,
    sd_circ = .1,
    sd_pluri = .2
) {
  names(dati$collegi_pluri)[names(dati$collegi_pluri) == "SEGGI_PLURI"] <-
    "SEGGI"
  
  
  #### Calcolo il numero massimo di candidati per collegio pluri ####
  dati$collegi_pluri$CANDIDATI_MAX <- 
    pmin(4, dati$collegi_pluri$SEGGI)
  if (ramo == "Senato") {
    dati$collegi_pluri$CANDIDATI_MAX[dati$collegi_pluri$SEGGI == 1] <- 1
  }
  
  
  #### Preparo il data frame dei candidati ####
  n_cand <- sum(dati$collegi_pluri$CANDIDATI_MAX) + nrow(dati$collegi_uni)
  dati$candidati <- data.frame(
    LISTA = rep(liste_naz$LISTA, each = n_cand),
    CL = rep(liste_naz$CL, each = n_cand)
  )
  
  dati$candidati$CANDIDATO <- 
    factor(paste(dati$candidati$LISTA, seq_along(dati$candidati$LISTA)))
  
  
  #### Preparo i data frame delle liste ai diversi livelli ####
  
  dati$liste_circ <- merge(
    dati$circoscrizioni,
    liste_naz[, c("LISTA", "PERCENTUALE")]
  )
  
  dati$liste_pluri <- merge(
    dati$collegi_pluri,
    dati$liste_circ[, c("CIRCOSCRIZIONE", "LISTA")]
  )
  
  dati$liste_uni <- merge(
    dati$collegi_uni,
    liste_naz[, c("LISTA", "CL", "AREA", "PERC_IN_AREA")]
  )
  
  dati$liste_uni <- merge(
    dati$liste_uni,
    dati$aree_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOUNINOMINALE",
      "COLLEGIOPLURINOMINALE",
      "AREA",
      "VOTANTI"
    )]
  )
  
  dati$liste_uni$VOTANTI_LISTA_BASE <- 
    dati$liste_uni$VOTANTI * dati$liste_uni$PERC_IN_AREA
  
  dati$liste_uni <- merge(
    dati$liste_uni,
    aggregate(
      VOTANTI_LISTA_BASE ~ 
        CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + COLLEGIOUNINOMINALE,
      dati$liste_uni,
      sum
    ),
    by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE", "COLLEGIOUNINOMINALE"),
    suffixes = c("", "_TOT")
  )
  
  dati$liste_uni$PERC_LISTA_BASE <- 
    dati$liste_uni$VOTANTI_LISTA_BASE / dati$liste_uni$VOTANTI_LISTA_BASE_TOT
  
  dati$liste_uni$LOG_PERC_LISTA_BASE <- log(dati$liste_uni$PERC_LISTA_BASE)
  
  dati$liste_uni$CAND_MINORANZA <- FALSE
  dati$liste_uni$MINORANZA <- FALSE
  
  #### Inizio iterazioni ####
  
  for (j in seq_len(iterazioni)) {
    ##### Randomizzo i voti ####
    
    dati$liste_uni$LOG_P <- rnorm(
      dati$liste_uni$LOG_PERC_LISTA_BASE,
      dati$liste_uni$LOG_PERC_LISTA_BASE,
      variab
    )
    
    dati$liste_uni$PERCENTUALE_UNI <- ave(
      dati$liste_uni$LOG_P,
      paste(
        dati$liste_uni$CIRCOSCRIZIONE,
        dati$liste_uni$COLLEGIOPLURINOMINALE,
        dati$liste_uni$COLLEGIOUNINOMINALE
      ),
      FUN = softmax
    )
    
    dati$liste_uni$VOTI_LISTA <- 
      dati$liste_uni$POP_2011 * dati$liste_uni$PERCENTUALE_UNI
    
    
    ##### Sorteggio i candidati ####
    
    dati$candidati_uni <- unique(dati$liste_uni[, c(
      "CIRCOSCRIZIONE",
      "COLLEGIOPLURINOMINALE",
      "COLLEGIOUNINOMINALE",
      "CL"
    )])
    
    dati$candidati$SCELTO_UNI <- FALSE
    dati$candidati_uni$CANDIDATO <- 
      factor(NA, levels = levels(dati$candidati$CANDIDATO))
    
    
    for (i in seq_along(dati$candidati_uni$CL)) {
      lista <- sample(
        liste_naz$LISTA[liste_naz$CL == dati$candidati_uni$CL[i]], 
        1, 
        prob = liste_naz$FRAZ_UNI[liste_naz$CL == dati$candidati_uni$CL[i]]
      )
      candidato <- sample(which(
        dati$candidati$LISTA == lista &
          !dati$candidati$SCELTO_UNI
      ), 1)
      
      dati$candidati_uni$CANDIDATO[i] <- dati$candidati$CANDIDATO[candidato]
      dati$candidati$SCELTO_UNI[candidato] <- TRUE
    }
    
    if (sum(duplicated(dati$candidati_uni$CANDIDATO)) > 0) stop(
      "Candidati uninominali duplicati"
    )
    
    
    dati$candidati_pluri <- dati$liste_pluri[
      rep(
        seq_along(dati$liste_pluri$CANDIDATI_MAX), 
        dati$liste_pluri$CANDIDATI_MAX
      ),
      c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "LISTA"
      )
    ]
    
    dati$candidati_pluri$NUMERO <- ave(
      seq_along(dati$candidati_pluri$LISTA),
      paste(
        dati$candidati_pluri$CIRCOSCRIZIONE,
        dati$candidati_pluri$COLLEGIOPLURINOMINALE,
        dati$candidati_pluri$LISTA
      ),
      FUN = seq_along
    )
    
    dati$candidati$SCELTO_PLURI <- 0
    dati$candidati_pluri$CANDIDATO <- 
      factor(NA, levels = levels(dati$candidati$CANDIDATO))
    
    for (i in seq_along(dati$candidati_pluri$NUMERO)) {
      if (runif(1) < liste_naz$FRAZ_PLURICAND[
        liste_naz$LISTA == dati$candidati_pluri$LISTA[i]
      ]) {
        papabili <- which(
          dati$candidati$LISTA == dati$candidati_pluri$LISTA[i] &
            dati$candidati$SCELTO_PLURI < 5 &
            (dati$candidati$SCELTO_UNI | dati$candidati$SCELTO_PLURI > 0)
        )
        
        if (length(papabili) > 0) {
          candidato <- sample(papabili, 1)
          
          dati$candidati_pluri$CANDIDATO[i] <- dati$candidati$CANDIDATO[candidato]
          dati$candidati$SCELTO_PLURI[candidato] <- 
            dati$candidati$SCELTO_PLURI[candidato] + 1
          
          next
        }
      }
      
      candidato <- which(
        dati$candidati$LISTA == dati$candidati_pluri$LISTA[i] &
          dati$candidati$SCELTO_PLURI == 0 &
          !dati$candidati$SCELTO_UNI
      )[1]
      
      dati$candidati_pluri$CANDIDATO[i] <- dati$candidati$CANDIDATO[candidato]
      dati$candidati$SCELTO_PLURI[candidato] <- 1
    }
    
    
    ##### Preparo i data frame per lo scrutinio ####
    dati$liste_uni$CANDIDATO <- NULL
    dati$liste_uni <- merge(
      dati$liste_uni,
      dati$candidati_uni
    )
    
    
    dati$candidati_uni <- merge(
      dati$candidati_uni,
      aggregate(
        VOTI_LISTA ~ 
          CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + COLLEGIOUNINOMINALE + CANDIDATO,
        dati$liste_uni,
        sum
      )
    )
    names(dati$candidati_uni)[names(dati$candidati_uni) == "VOTI_LISTA"] <-
      "VOTI_CANDIDATO"
    
    dati$candidati_uni$DATA_NASCITA <- as.POSIXct("1990-01-01")
    
    scrutinio <- Scrutinio(
      ramo,
      dati$liste_uni[, c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "COLLEGIOUNINOMINALE",
        "CANDIDATO",
        "CAND_MINORANZA",
        "LISTA",
        "MINORANZA",
        "VOTI_LISTA"
      )],
      liste_naz[, c(
        "LISTA",
        "COALIZIONE",
        "MINORANZA"
      )],
      dati$candidati_uni[, c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "COLLEGIOUNINOMINALE",
        "CANDIDATO",
        "DATA_NASCITA",
        "VOTI_CANDIDATO"
      )],
      dati$candidati_pluri[, c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "LISTA",
        "NUMERO",
        "CANDIDATO"
      )],
      dati$collegi_pluri[, c(
        "CIRCOSCRIZIONE",
        "COLLEGIOPLURINOMINALE",
        "SEGGI"
      )],
      ifelse(ramo == "Camera", 392, 296)
    )
    
    scrutinio$liste_pluri <- merge(
      scrutinio$liste_pluri,
      aggregate(
        VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE + LISTA,
        dati$liste_uni,
        sum
      )
    )
    
    scrutinio$liste_pluri <- merge(
      scrutinio$liste_pluri,
      aggregate(
        VOTI_LISTA ~ CIRCOSCRIZIONE + COLLEGIOPLURINOMINALE,
        scrutinio$liste_pluri,
        sum
      ),
      by = c("CIRCOSCRIZIONE", "COLLEGIOPLURINOMINALE"),
      suffixes = c("", "_TOT")
    )
    
    scrutinio$liste_pluri$PERCENTUALE <-
      scrutinio$liste_pluri$VOTI_LISTA / scrutinio$liste_pluri$VOTI_LISTA_TOT
    
    scrutinio$liste_naz <- aggregate(
      VOTI_LISTA ~ LISTA,
      scrutinio$liste_pluri,
      sum
    )
    
    scrutinio$liste_naz$PERCENTUALE <- 
      scrutinio$liste_naz$VOTI_LISTA / sum(scrutinio$liste_naz$VOTI_LISTA)
    
    scrutinio$liste_naz <- merge(
      scrutinio$liste_naz,
      aggregate(
        ELETTI ~ LISTA,
        scrutinio$liste_pluri,
        sum
      )
    )
    
    scrutinio$liste_naz <- merge(
      scrutinio$liste_naz,
      liste_naz[, c("LISTA", "CL")]
    )
    
    scrutinio$candidati_uni <- merge(
      scrutinio$candidati_uni,
      dati$candidati_uni[, c("CANDIDATO", "CL")]
    )
    
    scrutinio$cl_naz <- aggregate(
      VOTI_LISTA ~ CL,
      scrutinio$liste_naz,
      sum
    )
    
    scrutinio$cl_naz$PERCENTUALE <- 
      scrutinio$cl_naz$VOTI_LISTA / sum(scrutinio$cl_naz$VOTI_LISTA)
    
    scrutinio$cl_naz <- merge(
      scrutinio$cl_naz,
      aggregate(
        ELETTI ~ CL,
        scrutinio$liste_naz,
        sum
      )
    )
    
    scrutinio$cl_naz <- merge(
      scrutinio$cl_naz,
      aggregate(
        ELETTO ~ CL,
        scrutinio$candidati_uni,
        sum
      )
    )
    
    scrutinio$cl_naz$ELETTI_TOT <- 
      scrutinio$cl_naz$ELETTI + scrutinio$cl_naz$ELETTO
    
    scrutinio$cl_naz <- scrutinio$cl_naz[order(
      scrutinio$cl_naz$CL
    ), ]
    
    scrutinio$liste_pluri$ITER <- j
    scrutinio$liste_naz$ITER <- j
    scrutinio$cl_naz$ITER <- j
    
    if (j == 1) {
      risultato <- list()
      risultato$liste_pluri <- scrutinio$liste_pluri
      risultato$liste_naz <- scrutinio$liste_naz
      risultato$cl_naz <- scrutinio$cl_naz
    } else {
      risultato$liste_pluri <- rbind(risultato$liste_pluri, scrutinio$liste_pluri)
      risultato$liste_naz <- rbind(risultato$liste_naz, scrutinio$liste_naz)
      risultato$cl_naz <- rbind(risultato$cl_naz, scrutinio$cl_naz)
    }
  }
  
  liste_naz$COL <- "#DDDDDD"
  liste_naz$COL[!is.na(liste_naz$COLORE)] <- 
    hsv(liste_naz$COLORE[!is.na(liste_naz$COLORE)] / 360, 1, .8, 1)
  
  risultato$liste_naz$COL <- NULL
  risultato$liste_naz <- merge(
    risultato$liste_naz,
    liste_naz[, c("LISTA", "COL")]
  )
  
  png(
    paste0("output/", scenario, substr(ramo, 1, 1), "_l_naz.png"),
    width = 800,
    height = 800,
    res = 120
  )
  plot(
    ELETTI ~ I(PERCENTUALE * 100),
    data = risultato$liste_naz,
    col = COL,
    main = paste0(ramo, ": eletti nei collegi plurinominali"),
    xlab = "Percentuale",
    ylab = "Eletti"
  )
  legend(
    "topleft",
    legend = liste_naz$LISTA[!is.na(liste_naz$COLORE)],
    pch = 19,
    col = liste_naz$COL[!is.na(liste_naz$COLORE)]
  )
  dev.off()
  
  risultato$cl_naz <- merge(
    risultato$cl_naz,
    liste_naz[!duplicated(liste_naz$CL), c("CL", "COL")]
  )
  
  png(
    paste0("output/", scenario, substr(ramo, 1, 1), "_cl_naz.png"),
    width = 800,
    height = 800,
    res = 120
  )
  plot(
    ELETTI_TOT ~ I(PERCENTUALE * 100),
    data = risultato$cl_naz,
    col = COL,
    main = paste0(ramo, ": eletti nei collegi uni e plurinominali"),
    xlab = "Percentuale",
    ylab = "Eletti"
  )
  abline(h = 391 / 2, lty = "dotted")
  legend(
    "topleft",
    legend = liste_naz$CL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)],
    pch = 19,
    col = liste_naz$COL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)]
  )
  dev.off()
  
  png(
    paste0("output/", scenario, substr(ramo, 1, 1), "_cl_naz_magg.png"),
    width = 800,
    height = 800,
    res = 120
  )
  plot(
    ELETTO ~ I(PERCENTUALE * 100),
    data = risultato$cl_naz,
    col = COL,
    main = paste0(ramo, ": eletti nei collegi uninominali"),
    xlab = "Percentuale nazionale",
    ylab = "Eletti"
  )
  abline(h = nrow(dati$collegi_uni) / 2, lty = "dotted")
  legend(
    "topleft",
    legend = liste_naz$CL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)],
    pch = 19,
    col = liste_naz$COL[!is.na(liste_naz$COLORE) & !duplicated(liste_naz$CL)]
  )
  dev.off()
  
  for (lista in liste_naz$LISTA[liste_naz$GRAFICI]) {
    png(
      paste0(
        "output/",
        scenario,
        substr(ramo, 1, 1),
        "_l_naz_",
        liste_naz$ABBREV[liste_naz$LISTA == lista],
        ".png"
      ),
      width = 800,
      height = 800,
      res = 120
    )
    plot(
      ELETTI ~ I(PERCENTUALE * 100),
      data = risultato$liste_naz[risultato$liste_naz$LISTA == lista,],
      col = COL,
      xlab = "Percentuale",
      ylab = "Eletti"
    )
    mytitle = paste0(ramo, ": eletti nei collegi plurinominali")
    mysubtitle = lista
    mtext(side=3, line=2, cex = 1.5, mytitle)
    mtext(side=3, line=1, mysubtitle)
    dev.off()
    
    
    png(
      paste0(
        "output/",
        scenario,
        substr(ramo, 1, 1),
        "_nmax_",
        liste_naz$ABBREV[liste_naz$LISTA == lista],
        ".png"
      ),
      width = 800,
      height = 800,
      res = 120
    )
    nmax <- factor(
      risultato$liste_pluri$NUMERO_MAX[risultato$liste_pluri$LISTA == lista],
      levels = 0:4
    )
    nmax[is.na(nmax)] <- 4
    colori <- c(hcl.colors(4), "#FFFFFF")
    spineplot(
      nmax ~ I(
        risultato$liste_pluri$PERCENTUALE[
          risultato$liste_pluri$LISTA == lista
        ] *100
      ),
      breaks = 20,
      col = colori,
      yaxlabels = NA,
      ylab = NA,
      xlab = "Percentuale nel collegio plurinominale"
    )
    mytitle = "Probabilità di elezone in base alla posizione nel listino"
    mysubtitle = paste0(
      lista,
      "  -  ",
      ramo,
      "  -  Fraz. pluricandature: ", 
      format(
        liste_naz$FRAZ_PLURICAND[liste_naz$LISTA == lista] * 100,
        digits = 2
      ), 
      "%"
    )
    mtext(side=3, line=2, cex = 1.5, mytitle)
    mtext(side=3, line=1, mysubtitle)
    legend(
      "topleft",
      legend = levels(nmax)[-1],
      fill = rev(colori[-5]),
      title = "Posizione"
    )
    dev.off()
    
    png(
      paste0(
        "output/",
        scenario,
        substr(ramo, 1, 1),
        "_nmax_",
        liste_naz$ABBREV[liste_naz$LISTA == lista],
        "_Parma.png"
      ),
      width = 800,
      height = 800,
      res = 120
    )
    nmax <- factor(
      risultato$liste_pluri$NUMERO_MAX[
        risultato$liste_pluri$LISTA == lista &
          risultato$liste_pluri$COLLEGIOPLURINOMINALE == "Emilia-Romagna - P01"
      ],
      levels = 0:4
    )
    nmax[is.na(nmax)] <- 4
    colori <- c(hcl.colors(4), "#FFFFFF")
    spineplot(
      nmax ~ I(
        risultato$liste_pluri$PERCENTUALE[
          risultato$liste_pluri$LISTA == lista &
            risultato$liste_pluri$COLLEGIOPLURINOMINALE == "Emilia-Romagna - P01"
        ] *100
      ),
      breaks = 10,
      col = colori,
      yaxlabels = NA,
      ylab = NA,
      xlab = "Percentuale nel collegio plurinominale Emilia-Romagna - P01"
    )
    mytitle = "Probabilità di elezone in base alla posizione nel listino"
    mysubtitle = paste0(
      lista,
      "  -  ",
      ramo,
      "  -  Fraz. pluricandature: ", 
      format(
        liste_naz$FRAZ_PLURICAND[liste_naz$LISTA == lista] * 100,
        digits = 2
      ), 
      "%  -  Emilia-Romagna - P01"
    )
    mtext(side=3, line=2, cex = 1.5, mytitle)
    mtext(side=3, line=1, mysubtitle)
    legend(
      "topleft",
      legend = levels(nmax)[-1],
      fill = rev(colori[-5]),
      title = "Posizione"
    )
    dev.off()
  }
  
  
}

simula(
  "Camera",
  scenario,
  camera,
  liste_naz,
  iterazioni,
  sd_naz,
  sd_circ,
  sd_pluri
)

simula(
  "Senato",
  scenario,
  senato,
  liste_naz,
  iterazioni,
  sd_naz,
  sd_circ,
  sd_pluri
)
