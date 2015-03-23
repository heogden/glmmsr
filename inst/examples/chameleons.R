library(BradleyTerry2)
chameleon.model <- BTm(player1 = winner, player2 = loser,
                       formula = ~ prev.wins.2 + ch.res[ID] + prop.main[ID] + (1|ID),
                       id = "ID",
                       data = chameleons)

winner <- chameleons$winner$ID
loser <- chameleons$loser$ID
match <- 1:length(winner)

prevwins2 <- matrix(0, nrow = length(levels(winner)), ncol = length(winner))
subforms <- list(ability[p, t] ~ prevwins2[p, t] + ch.res[p] + prop.main[p] + (1 | p))

data = c(list(winner = winner, loser = loser, match = match, prevwins2 = prevwins2),
         as.list(chameleons$predictors))

mod <- glmerSR(Sub(ability[winner, match] - ability[loser, match],
                   subforms = subforms)

