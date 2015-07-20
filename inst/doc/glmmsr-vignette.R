## ---- echo = FALSE-------------------------------------------------------
library(glmmsr)

## ------------------------------------------------------------------------
(mod_Laplace <- lme4::glmer(response ~ covariate + (1 | cluster) + (1 | group),
                           data = three_level, family = binomial))

## ---- error = TRUE-------------------------------------------------------
lme4::glmer(response ~ covariate + (1 | cluster) + (1 | group),
                           data = three_level, family = binomial, nAGQ = 10)

## ------------------------------------------------------------------------
(mod_SR <- glmerSR(response ~ covariate + (1 | cluster) + (1 | group),
                  data = three_level, family = binomial, nAGQ = 10,
                  k = 3))

## ------------------------------------------------------------------------
library(BradleyTerry2)
winner <- chameleons$winner$ID
loser <- chameleons$loser$ID
match <- 1:length(winner)

wins <- matrix(0, nrow = length(levels(winner)), ncol = length(winner))
wins[cbind(as.numeric(winner), match)] <- 1

losses <- matrix(0, nrow = length(levels(winner)), ncol = length(winner))
losses[cbind(as.numeric(loser), match)] <- 1

contests <- wins + losses

find_prev2 <- function(wins_im, contests_im) {
  m <- min(2, sum(contests_im))
  if(m > 0) {
    res <- sum(wins_im[rev(which(contests_im > 0L))[1:m]])
  } else{
    res <- 0
  }
  res
}

prevwins2 <- matrix(0, nrow = nrow(wins), ncol = ncol(wins))
for(i in 1:nrow(wins)) {
  for(m in 2:ncol(wins)) {
    prevwins2[i, m] <- find_prev2(wins[i, 1:(m-1)], contests[i, 1:(m-1)])
  }
}

## ------------------------------------------------------------------------
library(glmmsr)
resp <- rep(1, length(winner))
cham_dat <- c(list(resp = resp, winner = winner, loser = loser, 
                  match = match, prevwins2 = prevwins2), 
             as.list(chameleons$predictors))

cham_mod <- glmerSR(resp ~ 0 + Sub(ability[winner, match] -
                                   ability[loser, match]),
                    ability[i, m] ~ 0 + prevwins2[i, m] + ch.res[i] 
                                    + prop.main[i] + (1 | i),
                    family = binomial, data = cham_dat)

print(summary(cham_mod), correlation = FALSE, show.resids = FALSE)

## ------------------------------------------------------------------------
cham_mod_BTm <- BTm(player1 = winner, player2 = loser,
                    formula = ~ prev.wins.2 + ch.res[ID] + prop.main[ID] + (1|ID),
                    id = "ID", data = chameleons)
summary(cham_mod_BTm)

