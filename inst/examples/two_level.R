# fit the two-level model using lme4
# we can use the Laplace approximation
mod_Laplace <- glmm(response ~ covariate + (1 | cluster),
                    data = two_level, family = binomial)
mod_Laplace

# or increase the number of adaptive Gaussian quadrature points
mod_8 <- glmm(response ~ covariate + (1 | cluster),
               data = two_level, family = binomial,
               method = "AGQ", control = list(nAGQ = 8))
mod_8

# we can fit the same model using the sequential reduction approximation
mod_SR <- glmm(response ~ covariate + (1 | cluster),
               data = two_level, family = binomial,
               method = "SR", control = list(nSL = 3))
mod_SR
