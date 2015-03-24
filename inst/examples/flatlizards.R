library(BradleyTerry2)

m <- nrow(flatlizards$contests)

# have to swap around first response, because lme4 disallows constant response
y <- c(0, rep(1, m-1))
n <- nrow(flatlizards$predictors)
loser <- flatlizards$contests$loser
winner <- flatlizards$contests$winner

player1 <- c(loser[1], winner[-1])
player2 <- c(winner[1], loser[-1])

# glmmsr does not yet handle missing values
# remove all matches involving player with missing values in throat.PC3
matches_na <- which(is.na(flatlizards$predictors$throat.PC3[player1]) |
                      is.na(flatlizards$predictors$throat.PC3[player2]))
player1_rm <- player1[-matches_na]
player2_rm <- player2[-matches_na]
y_rm <- y[-matches_na]

# remove all missing values from the predictors
flatlizardspred <- as.data.frame(flatlizards$predictors)
flatlizardspred[is.na(flatlizardspred)] <- 0

data <- c(list(y = y_rm, player1 = player1_rm, player2 = player2_rm),
          flatlizardspred)

mod <- glmerSR(y ~ 0 + Sub(ability[player1] - ability[player2]),
               family = binomial(link = "probit"), data = data,
               subforms = list(ability[player] ~ 0 + throat.PC1[player]
                               + throat.PC3[player] + head.length[player]
                               + SVL[player] + (1 | player)))

mod


result <- rep(1, m)
Whiting.model2 <- BTm(result, winner, loser, ~ throat.PC1[..] + throat.PC3[..] +
                        head.length[..] + SVL[..] + (1|..),
                      data = list(contests, predictors))
Whiting.model2
