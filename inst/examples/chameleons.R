library(BradleyTerry2)
mod_BTm <- BTm(player1 = winner, player2 = loser,
                       formula = ~ prev.wins.2 + ch.res[ID] + prop.main[ID] + (1|ID),
                       id = "ID",
                       data = chameleons)

winner <- chameleons$winner$ID
loser <- chameleons$loser$ID
match <- 1:length(winner)

wins <- matrix(0, nrow = length(levels(winner)), ncol = length(winner))
wins[cbind(as.numeric(winner), match)] <- 1

losses <- matrix(0, nrow = length(levels(winner)), ncol = length(winner))
losses[cbind(as.numeric(loser), match)] <- 1

contests <- wins + losses

find_prev2_it <- function(wins_it, contests_it) {
  m <- min(2, sum(contests_it))
  if(m > 0) {
    res <- sum(wins_it[rev(which(contests_it > 0L))[1:m]])
  } else{
    res <- 0
  }
  res
}

prevwins2 <- matrix(0, nrow = nrow(wins), ncol = ncol(wins))
for(i in 1:nrow(wins)) {
  for(t in 2:ncol(wins)) {
    prevwins2[i, t] <- find_prev2_it(wins[i, 1:(t-1)], contests[i, 1:(t-1)])
  }
}

#check
all.equal(prevwins2[cbind(winner, 1:length(winner))], chameleons$winner$prev.wins.2)
all.equal(prevwins2[cbind(loser, 1:length(loser))], chameleons$loser$prev.wins.2)

subforms <- list(ability[p, t] ~ 0 + prevwins2[p, t] + ch.res[p] + prop.main[p]
                 + (1 | p))

resp <- rep(1, length(winner))
resp[1:5] <- 0
player1 <- c(loser[1:5], winner[-(1:5)])
player2 <- c(winner[1:5], loser[-(1:5)])

data = c(list(resp = resp, player1 = player1, player2 = player2, match = match,
              prevwins2 = prevwins2), as.list(chameleons$predictors))

mod <- glmerSR(resp ~ 0 + Sub(ability[player1, match] - ability[player2, match]),
               family = binomial, data = data,
               subforms = subforms)

