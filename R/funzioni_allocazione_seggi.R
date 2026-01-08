Hare.Niemeyer <- function(votes, seats, details = FALSE) {
  if (seats > 0) {
    q <- sum(votes) / seats
    quotients <- votes %/% q
    still.to.assign <- seats - sum(quotients)
    remainders <- votes %% q
    remainders.order <- order(remainders, votes, runif(length(votes)), decreasing = TRUE)
    remainders.seats <- rep(0, length(votes))
    remainders.seats[remainders.order[1:still.to.assign]] <- 1
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