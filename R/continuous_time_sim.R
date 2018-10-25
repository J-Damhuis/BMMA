# Exercise 5
#' @title simulates continuous time model by taking small time steps
#' @param n0 initial number of individuals
#' @param tmax maximum simulation time
#' @param b birth probability rate
#' @param d death probabilty rate
#' @param dt size of time step
#' @examples simcontinuous(n = 1000, t = 10, b = 0.1, d = 0.1, dt = 0.001)
#' @export
simcontinuous <- function(n0, tmax, b, d, dt) {
  n <- n0
  out <- data.frame(matrix(ncol = 2))
  colnames(out) <- c("t", "n")
  out[1, 1] <- 0
  out[1, 2] <- n0
  for (k in 1:(tmax / dt)) {
    pb <- b * n * dt
    pd <- d * n * dt
    dn <- sample(x = c(1, -1, 0), size = 1, prob = c(pb, pd, 1 - pb - pd))
    n <- n + dn
    out[k+1, 1] <- k * dt
    out[k+1, 2] <- n
  }
  plot(out)
}

# Exercise 6
#' title simulates continuous time model by Doob-Gillespie algorithm
#' @param tend maximum simulation time
#' @param b birth probability rate
#' @param d death probabilty rate
#' @param n0 initial number of individuals
#' @examples simcontinuouseff(n0 = 1000, tend = 10, b = 0.1, d = 0.1, tstart = 0)
#' @export
simcontinuouseff <- function(n0, tend, b, d, tstart) {
  k <- 2
  n <- n0
  out <- data.frame(matrix(ncol = 2))
  colnames(out) <- c("t", "n")
  out[1, 1] <- tstart
  out[1, 2] <- n
  currentt <- tstart
  tnew <- currentt + rexp(n = 1, rate = ((b + d) * n))
  while (tnew < tend) {
    change <- sample(c(1, -1), size = 1, prob = c(b,d))
    n <- n + change
    currentt <- tnew
    out[k, 1] <- currentt
    out[k, 2] <- n
    k <- k + 1
    tnew <- currentt + rexp(n = 1, rate = ((b + d) * n))
  }
  plot(out)
}
