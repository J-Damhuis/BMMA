# Exercise 1
probs <- function(b, d, t) {
  p0 <- (d * (exp((b - d) * t) - 1)) / (b * exp((b - d) * t) - d)
  notp0 <- 1 - p0
  return(cbind(p0, notp0))
}

ut <- function(b, d, t) {
  out <- (b * (exp((b - d) * t) - 1)) / (b * exp((b - d) * t) - d)
  return(out)
}

sample_bd <- function(b, d, t, size) {
  prob1 <- probs(b = b, d = d, t = t)
  ut1 <- ut(b = b, d = d, t =t)
  
  out <- rep(NA, size)
  for (i in 1:size) {
    if (sample(size = 1, x = c("0", "not0"), prob = prob1) == "0") {
      out[i] <- 0
    } else {
      not0 <- 1 + rgeom(n = 1, prob = ut1)
      out[i] <- not0
    }
  }
  return(out)
}

sample_bd(b = 0.5, d = 0.6, t = 10, size = 10)

# Exercise 2
calcprob <- function(n, t, b, d) {
  out <- ((d * (exp((b - d) * t) - 1)) / (b * exp((b - d) * t) - d)) ^ n
  return(out)
}

calcprob(n = 100, t = 25, b = 0.5, d = 0.6)

# Exercise 3
simdiscrete <- function(n, t, b, d) {
  out <- data.frame(matrix(ncol = 2))
  colnames(out) <- c("t", "n")
  out[1, 1] <- 0
  out[1, 2] <- n
  for (k in 1:t) {
    dn <- sample(x = c(1, -1, 0), size = 1, prob = c(b, d, 1 - b - d))
    n <- n + dn
    out[k + 1, 1] <- k + 1
    out[k + 1, 2] <- n
  }
  plot(out)
}

simdiscrete(n = 1000, t = 1000)

# Exercise 4
simdiscreteeff <- function(n, tstart = 0, tend, b, d) {
  k <- 2
  out <- data.frame(matrix(ncol = 2))
  colnames(out) <- c("t", "n")
  out[1, 1] <- tstart
  out[1, 2] <- n
  tcurrent <- tstart + rgeom(n = 1, prob = b + d)
  while (tcurrent < tend) {
    dn <- sample(x = c(1, -1), size = 1, prob = c(b, d))
    n <- n + dn
    out[k, 1] <- currentt
    out[k, 2] <- n
    k <- k + 1
    tcurrent <- tcurrent + rgeom(n = 1, prob = b + d)
  }
  plot(out)
}

simdiscreteeff(n = 1000, t = 1000)

# Exercise 5
simcontinuous <- function(n, t, b, d, dt) {
  out <- data.frame(matrix(ncol = 2))
  colnames(out) <- c("t", "n")
  out[1, 1] <- 0
  out[1, 2] <- n
  for (k in 1:(t / dt)) {
    pb <- b * n * dt
    pd <- d * n * dt
    dn <- sample(x = c(1, -1, 0), size = 1, prob = c(pb, pd, 1 - pb - pd))
    n <- n + dn
    out[k+1, 1] <- k * dt
    out[k+1, 2] <- n
  }
  plot(out)
}

simcontinuous(n = 1000, t = 10, b = 0.1, d = 0.1, dt = 0.001)

# Exercise 6
simcontinuouseff <- function(n, tstart = 0, tend, b, d) {
  k <- 2
  out <- data.frame(matrix(ncol = 2))
  colnames(out) <- c("t", "n")
  out[1, 1] <- tstart
  out[1, 2] <- n
  tcurrent <- tstart + rexp(n = 1, rate = ((b + d) * n))
  while (tcurrent < tend) {
    dn <- sample(c(1, -1), size = 1, prob = c(b, d))
    n <- n + dn
    out[k, 1] <- tcurrent
    out[k, 2] <- n
    k <- k + 1
    tcurrent <- tcurrent + rexp(n = 1, rate = ((b + d) * n))
  }
  plot(out)
}

simcontinuouseff(n = 1000, t = 10, b = 0.1, d = 0.1)

# Exercise 7
simnew <- function(n, tstart, tend, b0, k, d) {
  i <- 2
  out <- data.frame(matrix(ncol = 2))
  colnames(out) <- c("t", "n")
  out[1, 1] <- tstart
  out[1, 2] <- n
  b <- max(0, b0 * (1 - n / k))
  tcurrent <- tstart + rexp(n = 1, rate = ((b + d) * n))
  while (tcurrent < tend) {
    dn <- sample(c(1, -1), size = 1, prob = c(b, d))
    n <- n + dn
    out[i, 1] <- tcurrent
    out[i, 2] <- n
    i <- i + 1
    b <- max(0, b0 * (1 - n / k))
    tcurrent <- tcurrent + rexp(n = 1, rate = ((b + d) * n))
  }
  plot(out)
}

simnew(n = 30, t = 10, b0 = 3, d = 0.1, k = 40)