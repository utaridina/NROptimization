newton_raphson <- function(f, f_prime, x0, tol = 1e-6, max_iter = 100) {
  x <- x0
  for (i in 1:max_iter) {
    fx <- f(x)
    dfx <- f_prime(x)

    if (abs(dfx) < tol) {
      stop("Derivative is too small, method fails.")
    }

    x_new <- x - fx / dfx
    if (abs(x_new - x) < tol) {
      return(x_new)
    }
    x <- x_new
  }
  stop("Maximum iterations reached without convergence.")
}
