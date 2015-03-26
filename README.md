<!-- README.md is generated from README.Rmd. Please edit that file -->
`glmmsr`: fit GLMMs with sequential reduction
---------------------------------------------

Generalized linear mixed models (GLMMs) are an important and widely-used model class. In R, we can fit these models with the `lme4` package, but there are some limitations. First, except in very simple cases, lme4 uses a Laplace approximation to the likelihood for inference, which may be of poor quality in some cases. Second, it is difficult to fit some GLMMs, such as pairwise comparison models, with lme4.

The `glmmsr` package aims to offer progress on both of these problems.

The sequential reduction approximation to the likelihood is controlled by a parameter (`k`) which allows the user to trade-off accuracy of the approximation against computation time. I am currently rewriting the code for the sequential reduction approximation, and it is not yet available in `glmmsr`. A previous version of the code is available as supplementary material to Ogden (2015).

Currently, `glmmsr` provides an extended interface to `lme4`. This interface allows easy fitting of pairwise comparison and many other interesting models, which are difficult to fit directly with `lme4`.

The function used to fit a GLMM is `glmerSR`, which is based on `glmer` from `lme4`. A typical call would look like

    glmerSR(formula, subformula, data, family)

where

1.  `formula` may contain one or more terms surrounded with `Sub(.)`. We call the expression contained within `Sub(.)` a **substitution expression**. This is a mathematical expression dictating how the response depends on a **substituted variable**: a dummy variable not contained in `data`.

2.  `subformula` contains a **subformula** for each substituted variable, which describes how the substituted variable depends on covariates.

The vignette gives more details and examples of how to use the subformula interface.

References
----------

Ogden, Helen. 2015. “A sequential reduction method for inference in generalized linear mixed models.” *Electronic Journal of Statistics* 9: 135–52. doi:[10.1214/15-EJS991](http://dx.doi.org/10.1214/15-EJS991).
