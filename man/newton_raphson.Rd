\name{newton_raphson}
\alias{newton_raphson}
\title{Newton-Raphson Optimization}
\description{
  Implements the Newton-Raphson method to find the root of a function.
}
\usage{
newton_raphson(f, f_prime, x0, tol = 1e-6, max_iter = 100)
}
\arguments{
  \item{f}{A function for which the root is to be found.}
  \item{f_prime}{The derivative of the function.}
  \item{x0}{Initial guess.}
  \item{tol}{Tolerance level for convergence (default: 1e-6).}
  \item{max_iter}{Maximum number of iterations (default: 100).}
}
\value{
  Returns the estimated root of the function.
}
\examples{
  f <- function(x) x^2 - 4
  f_prime <- function(x) 2 * x
  newton_raphson(f, f_prime, x0 = 1)
}

