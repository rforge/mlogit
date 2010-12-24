\name{scoretest}
\alias{scoretest}
\alias{scoretest.mlogit}
\alias{scoretest.default}
\alias{waldtest.mlogit}
\alias{lrtest.mlogit}
\title{The three tests for mlogit models}
\description{
  Three tests for mlogit models
}
\usage{
scoretest(object, ...)
\method{scoretest}{mlogit}(object, ..., heterosc = FALSE, nests = NULL,
        rpar = NULL )
\method{waldtest}{mlogit}(object, ..., hyp = c("fixed", "uncorrelated"))
\method{lrtest}{mlogit}(object, ...)
}

\arguments{
  
  \item{object}{an object of class \code{mlogit} or a formula,}

  \item{hyp}{the hypothesis to be tested when \code{object} is a random
    parameters model,}

  \item{nests}{a named list of characters vectors, each names being a
    nest, the corresponding vector being the set of alternatives that
    belong to this nest,}

  \item{heterosc}{a boolean, if \code{TRUE}, the heteroscedastic logit
    model is estimated,}
  
  \item{rpar}{a named vector whose names are the random parameters and
    values the distribution : \code{'n'} for normal, \code{'l'} for
    log-normal, \code{'t'} for truncated normal, \code{'u' } for uniform,}


  \item{...}{further arguments passed to the testing functions.}
  
}
\value{
  an object of class \code{"htest"}.
}
\details{
  
  The \code{"scoretest"} function and \code{"mlogit"} method for
  \code{"waldtest"} and \code{"lrtest"} from the \code{"lmtest"} package
  provides the infrastructure to compute the three tests of hypothesis
  for \code{"mlogit"} objects.
  
}
\author{Yves Croissant}
\keyword{htest}