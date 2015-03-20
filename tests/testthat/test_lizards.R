library(glmmsr)
library(BradleyTerry2)


m <- nrow(flatlizards$contests)

#have to swap around first response, because lme4 disallows constant response
y <- c(0, rep(1, m-1))
n <- nrow(flatlizards$predictors)
loser <- as.character(flatlizards$contests$loser)
winner <- as.character(flatlizards$contests$winner)

player1 <- c(loser[1], winner[-1])
player2 <- c(winner[1], loser[-1])

flatlizardspred <- as.data.frame(flatlizards$predictors)
flatlizardspred[is.na(flatlizardspred)] <- 0
#doesn't work for repro.tactic, since a factor


l <- sort(unique(c(player1, player2)))
Player1 <- factor(player1, levels = l)
Player2 <- factor(player2, levels = l)


data <- c(list(y = y, Player1 = Player1, Player2 = Player2), flatlizardspred)

mod <- glmerSR(y ~ 0 + Sub(ability[Player1] - ability[Player2]),
        family = binomial(link = "probit"), data = data,
        subforms = list(ability[p] ~ 0 + throat.PC1[p] + throat.PC3[p] +
                        head.length[p] + SVL[p] + (1 | p)))

mod

mod_BTm <- BTm(y, Player1, Player2,
               ~ throat.PC1[..] + throat.PC3[..] + head.length[..] + SVL[..] + (1|..),
                      family = binomial(link = "probit"),
                      data = list(contests, predictors))

mod_BTm
