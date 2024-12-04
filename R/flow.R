library(mvtnorm)

rpoisar1 <- function(N, p, lambda.0, params) {
  d <- params$linear$d
  A <- params$linear$A
  B <- params$linear$B
  tau  <- params$seasonal$T
  ampl <- params$seasonal$A
  R <- params$corrmat
  K <- params$K
  # Initialize vectors to store results
  y <- lambda <- matrix(NA, nrow = N, ncol = p)
  # Set initial values
  lambda.t <- lambda.0
  for (t in 1L:N) {
    u   <- apply(rmvnorm(K, sigma = R), 2L, pnorm)
    x   <- t(t(-log(u)) / lambda.t)
    y.t <- apply(x, 2L, cumsum) |>
      apply(2L, function(x.j) sum(x.j <= 1.0))
    y[t,] <- y.t

    lambda[t,] <- lambda.t
    lambda.t <- d + c(A %*% lambda.t) + c(B %*% y.t) + ampl * cos(2.0 * pi * ((t + 1L) / tau))
  }
  # Return simulated values
  list('y' = y, 'lambda' = lambda)
}

params = list(
  "linear" = list(
    'd' = c(0.5, 0.4, 0.5, 0.8),
    'A' = diag(c(0.7, 0.8, 0.6, 0.5)),
    'B' = diag(c(0.2, 0.1, 0.3, 0.3))
  ),
  "seasonal" = list(
    'T' = 365.0,
    'A' = 0.2
  ),
  "corrmat" = matrix(c(
    1.0, 0.3, 0.5, 0.7, 
    0.3, 1.0, 0.4, 0.2, 
    0.5, 0.4, 1.0, 0.6, 
    0.7, 0.2, 0.6, 1.0), nrow = 4L),
  'K' = 20L
)

Y <- rpoisar1(365L, 4L, c(5.0, 3.0, 9.0, 4.0), params)$y

matplot(Y, type = 's', lty = 1L, col = 1:4L, xlab = 'Time', ylab = 'Count', main = 'Simulated Poisson AR(1) Process')

## Single time step simulation
rpoisar1 <- function(t, p, lambda.t, params) {
  d <- params$linear$d
  A <- params$linear$A
  B <- params$linear$B
  tau  <- params$seasonal$T
  ampl <- params$seasonal$A
  R <- params$corrmat
  K <- params$K
  # Set initial values
  u   <- apply(rmvnorm(K, sigma = R), 2L, pnorm)
  x   <- t(t(-log(u)) / lambda.t)
  y.t <- apply(x, 2L, cumsum) |>
    apply(2L, function(x.j) sum(x.j <= 1.0))

  lambda.t <- d + c(A %*% lambda.t) + c(B %*% y.t) + ampl * cos(2.0 * pi * ((t + 1L) / tau))
  # Return simulated values
  list('y.t' = y.t, 'lambda.t' = lambda.t)
}

p <- 4L
lambda.t <- c(5.0, 3.0, 9.0, 4.0)

Y <- matrix(0L, ncol = 4L, nrow = 365L)
for (t in 1L:365L) {
  tmp <- rpoisar1(t, 4L, lambda.t, params)
  Y[t,]    <- tmp$y.t
  lambda.t <- tmp$lambda.t
}
matplot(Y, type = 's', lty = 1L, col = 1L:4L, 
        xlab = 'Time', ylab = 'Count', 
        main = 'Simulated Poisson AR(1) Process')
