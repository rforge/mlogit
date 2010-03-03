\name{correlation}
\alias{cor.mlogit}
\alias{cov.mlogit}
\alias{sd.mlogit}
\title{Correlation structure of the random parameters}
\description{
  Functions that extract the correlation structure of a mlogit object
}
\usage{
cor.mlogit(x)
cov.mlogit(x)
sd.mlogit(x)
}

\arguments{
  \item{x}{an \code{mlogit} object with \code{correlation=TRUE}.}
}
\value{
  A matrix for \code{cor.mlogit} and \code{cov.mlogit}, a vector for \code{sd.mlogit}
}
\details{
  These functions extract the standard deviations (\code{sd.mlogit}), the
  covariance (\code{cov.mlogit}) or the correlations (\code{cor.mlogit}).
}

\author{Yves Croissant}

\keyword{regression}
