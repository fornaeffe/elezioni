# Verifico la frequenza di pluricandidature
library(data.table)
library(igraph)

candidati <- fread("dati/camera-20220925/camera2022_candidatilista.csv")
camera_italia_livcomune <- fread("dati/camera-20220925/Camera_Italia_LivComune.csv")

setdiff(candidati$descrlista, camera_italia_livcomune$DESCRLISTA)
setdiff(camera_italia_livcomune$DESCRLISTA, candidati$descrlista)

class(candidati$datanascita)
class(camera_italia_livcomune$DATANASCITA)

candidati[
  ,
  datanascita := as.POSIXct(datanascita, format = "%d/%m/%Y %H:%M:%S")
]
camera_italia_livcomune[
  ,
  DATANASCITA := as.POSIXct(DATANASCITA, format = "%d/%m/%Y")
]

candidati[
  ,
  CANDIDATO_ID := paste(cognome, nome, datanascita)
]
camera_italia_livcomune[
  ,
  CANDIDATO_ID := paste(COGNOME, NOME, DATANASCITA)
]

candidati_uni_liste <- unique(camera_italia_livcomune[
  ,
  .(
    `CIRC-REG`,
    COLLPLURI,
    COLLUNINOM,
    DESCRLISTA,
    CANDIDATO_ID
  )
])

setnames(candidati, "descrlista", "DESCRLISTA")



# Create a unique edge list between DESCRLISTA and CANDIDATO_ID
edges <- unique(candidati_uni_liste[, .(DESCRLISTA, CANDIDATO_ID)])

# Build the graph
g <- graph_from_data_frame(edges, directed = FALSE)

# Find connected components (clusters)
clusters <- components(g)$membership

# Convert cluster results to a lookup table
# We only need the membership for DESCRLISTA values
group_lookup <- data.table(
  DESCRLISTA = names(clusters),
  group_id = clusters
)[DESCRLISTA %in% candidati_uni_liste$DESCRLISTA]


candidati_uni_in_pluri <- candidati[
  candidati_uni_liste[,.(CANDIDATO_ID, DESCRLISTA)],
  on = .(CANDIDATO_ID, DESCRLISTA),
  nomatch = NULL,
  mult = "first"
]

# Controllo che non ci siano duplicati
sum(duplicated(candidati_uni_in_pluri[,.(CANDIDATO_ID, DESCRLISTA)]))
sum(duplicated(candidati_uni_in_pluri[,.(CANDIDATO_ID)]))

# Candidati uninominali totali
n_uni <- length(unique(candidati_uni_liste$CANDIDATO_ID))

# Frazione complessiva di uni in pluri
nrow(candidati_uni_in_pluri) / n_uni


candidati_uni_liste[
  group_lookup,
  on = .(DESCRLISTA),
  group_id := i.group_id
]
candidati_uni_in_pluri[
  group_lookup,
  on = .(DESCRLISTA),
  group_id := i.group_id
]

candidati_uni_coal <- unique(candidati_uni_liste[,.(CANDIDATO_ID, group_id)])
candidati_uni_in_pluri_coal <- unique(candidati_uni_in_pluri[,.(CANDIDATO_ID, group_id)])

candidati_per_coal <- candidati_uni_coal[
  ,
  .(UNI = .N),
  by = group_id
][
  candidati_uni_in_pluri_coal[
    ,
    .(UNI_IN_PLURI = .N),
    by = group_id
  ],
  on = .(group_id)
][
  ,
  f := UNI_IN_PLURI / UNI
]

setorder(candidati_per_coal, group_id)

candidati_per_coal[1:4, mean(f)]

candidati_liste_principali <- candidati[DESCRLISTA %in% group_lookup[group_id %in% 1:4, DESCRLISTA]]

n <- candidati_liste_principali[
  ,
  .(NUM = .N),
  by = .(CANDIDATO_ID)
][
  ,
  .N,
  by = .(NUM)
]

n1 <- sum(n$N)
n2 <- sum(n$N[2:5])
n3 <- sum(n$N[3:5])
n4 <- sum(n$N[4:5])
n5 <- n$N[5]
nTOT <- n1 + n2 + n3 + n4 + n5
n1 / nTOT
n2 / nTOT
n3 / nTOT
n4 / nTOT
n5 / nTOT