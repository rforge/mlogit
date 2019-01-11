# mlogit <img src="logo.png" align="right" alt="" width="120" />

## Random utility models

Random utility models is the reference approach in economics when one
wants to analyze the choice by a decision maker of one among a set of
mutually exclusive alternatives. Since the seminal works of Daniel Mc
Fadden who won the Nobel prize in economics ``for his
development of theory and methods for analyzing discrete choice'', a
large amount of theoretical and empirical literature have been
developed in this field.

These models rely on the hypothesis that the decision maker is able
to rank the different alternatives by an order of preference
represented by a utility function. 

These models are called random utility models because the researcher
is unable to measure the whole level of utility, but only part of
it. 

Different hypothesis on the distribution of the error lead to
different flavors of random utility models. Early developments of
these models were based on the hypothesis of identically and
independent errors following a Gumbel distribution. Much more general
models have since been proposed, based on much less restrictive
distribution hypothesis, and often estimated using simulations.


## The mlogit package

The first version of `mlogit` was posted in 2008, it was the first `R`
package allowing the estimation of random utility models. Since then,
other package have emerged.  `mlogit` still provides the widests set
of estimators for random utility models and, moreover, its syntax has
been adopted by other `R` packages, especially by `gmnl` and `mnlogit`
which, respectively, implements advanced mixed logit models and
estimates efficiently multinomial logit models on large data sets.

## Documentation

A gentle introduction of `mlogit` is available as vignettes. 4
exercises are also provided ; they were written by Kenneth Train and
adapted to `R` by Yves Croissant.
