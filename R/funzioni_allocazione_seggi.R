Hare.Niemeyer <- function(votes, seats, details = FALSE) {
  if (seats > 0) {
    q <- sum(votes) / seats
    quotients <- votes %/% q
    still.to.assign <- seats - sum(quotients)
    remainders <- votes %% q
    remainders.order <- order(remainders, votes, runif(length(votes)), decreasing = TRUE)
    remainders.seats <- rep(0, length(votes))
    remainders.seats[remainders.order[seq_len(still.to.assign)]] <- 1
    assigned <- quotients + remainders.seats 
  } else {
    assigned <- rep(0, length(votes))
    remainders <- votes
    remainders.seats <- rep(0, length(votes))
  }
  
  if (details) {
    return( data.frame(assigned = assigned, remainders = remainders, remainders.seats = remainders.seats ))
  } else return(assigned)
}

Hare.Niemeyer_con_minimo <- function(voti, seggi, minimi) {
  
  if (!(length(minimi) %in% c(1, length(voti)))) warning(
    "In Hare-Niemeyer con minimo le lunghezze di voti e minimi non coincidono"
  )
  
  if (sum(minimi) > seggi) stop("La somma dei minimi supera i seggi da assegnare")
  
  quoziente <- sum(voti) / sum(seggi)
  voti_per_hn <- pmax(voti - quoziente * minimi, 0)
  
  seggi_da_assegnare <- seggi - sum(minimi)
  
  seggi_variabili <- Hare.Niemeyer(voti_per_hn, seggi_da_assegnare)
  
  seggi <- minimi + seggi_variabili
  
  seggi
}