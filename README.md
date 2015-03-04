# glmmsr

Fit a generalized linear mixed model with the sequential reduction
approximation to the likelihood. The sequential reduction method provides a
more accurate approximation to the likelihood than the Laplace approximation
(used in lme4). The interface is an extension to lme4, to allow some models
(for examle pairwise comparison models) not easily specified with lme4.
