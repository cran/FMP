\name{FMPMonotonicityCheck}
\alias{FMPMonotonicityCheck}
\title{Utility function for checking FMP monotonicity}
\description{Utility function for checking whether candidate FMP coefficients yield a monotonically increasing polynomial.}
\usage{ FMPMonotonicityCheck(b, lower = -10, upper = 10)
}
\arguments{
 \item{b}{A vector of 8 polynomial coefficients (\eqn{\beta}) for \eqn{m(\theta)=\beta_0 + 
 \beta_1 \theta + \beta_2 \theta^2 + \beta_3 \theta^3 + \beta_4 \theta^4 + 
 \beta_5 \theta^5 + \beta_6 \theta^6 + \beta_7 \theta^7}.}
 \item{lower, upper}{Theta bounds for monotonicity check.}
}
\value{
 \item{minDeriv}{Minimum value of the derivative for the polynomial.}
}
\author{Niels Waller}
\examples{

## A set of candidate coefficients for an FMP model.
## These coefficients fail the test and thus
## should not be used with genFMPdata to generate
## item response data that are consistent with an 
## FMP model.
 b <- c(1.21, 1.87, -1.02, 0.18, 0.18, 0, 0, 0)
 FMPMonotonicityCheck(b)
}
\keyword{statistics}% at least one, from doc/KEYWORDS

