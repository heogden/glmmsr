library(glmmsr)
library(BradleyTerry2)


m <- nrow(flatlizards$contests)

#have to swap around first response, because lme4 disallows constant response
y <- c(0, rep(1, m-1))
n <- nrow(flatlizards$predictors)
loser <- as.character(flatlizards$contests$loser)
winner <- as.character(flatlizards$contests$winner)

player <- rownames(flatlizards$predictors)

player1 <- c(loser[1], winner[-1])
player2 <- c(winner[1], loser[-1])

flatlizardspred <- as.data.frame(flatlizards$predictors)
flatlizardspred[is.na(flatlizardspred)] <- 0
#doesn't work for repro.tactic, since a factor

data <- c(list(y = y, player1 = player1, player2 = player2, player = player),
          flatlizardspred)

mod <- glmerSR(y ~ 0 + Sub(ability[player1] - ability[player2]),
        family = binomia(link = "probit"), data = data,
        subforms = list(ability[player] ~ 0 + throat.PC1[player]
                        + throat.PC3[player] + head.length[player]
                        + SVL[player] + (1 | player)))

mod

Player1 <- factor(player1, levels = player)
Player2 <- factor(player2, levels = player)

mod_BTm <- BTm(y, Player1, Player2,
               ~ throat.PC1[..] + throat.PC3[..] + head.length[..] + SVL[..] + (1|..),
                      family = binomial,
                      data = list(contests, predictors))

mod_BTm
