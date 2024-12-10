library(mvtnorm)

## Correlated stagewise independent Poisson
# Number of blocks
m <- 2L

# Size of each block
n <- 6L

# Function to create individual covariance blocks
create_block <- function(n) {
  # Example: Covariance matrix with 1 on the diagonal and 0.5 off-diagonal
  block <- matrix(sample(seq(0.6, 0.8, by = 0.1), 1L), nrow = n, ncol = n)
  diag(block) <- 1.0
  return(block)
}

# Create a list of blocks
blocks <- replicate(m, create_block(n), simplify = FALSE)

# Combine blocks into a block diagonal matrix
block_diag_matrix <- Matrix::bdiag(blocks)

# Convert to dense matrix if needed
covmat <- as.matrix(block_diag_matrix)
covmat[covmat == 0.0] <- 0.4

flow <- mvtnorm::rmvnorm(n = 10000L, sigma = covmat) |>
  apply(2L, pnorm) |>
  qpois(2000L)

df <- as.data.frame(flow)
colnames(df) <- c(paste0("Infow_", 1L:n), paste0("Outfow_", 1L:n))

library(GGally)
library(ggplot2)
# Generate pairwise hexbin heatmap plots using geom_hex
custom_hex <- function(data, mapping, ...) {
  ggplot(data, mapping) +
    geom_hex(bins = 30) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    theme_minimal()
}

# Generate pairwise hexbin heatmap plots
ggpairs(
  df,
  lower = list(continuous = wrap(custom_hex)),
  title = "Pairwise Hexbin Heatmap Plots"
) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    title = ggplot2::element_text(size = 16L, face = "bold"),  # Increase plot title font size
    legend.position = "top",                # Move legend to the top
    legend.title = ggplot2::element_blank(), # Increase legend title font size
    legend.text  = ggplot2::element_text(size = 10L),  # Increase legend items font size
    axis.title   = ggplot2::element_text(size = 12L),   # Increase axis titles font size
    axis.text    = ggplot2::element_text(size = 10L),    # Increase axis text font size
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    strip.text   = ggplot2::element_text(size = 12L, face = "bold")    # Increase facet labels font size
  )

################################################################################
## Correlated Poisson AR(1)
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

################################################################################
## Correlated stagewise independent spot rates
# Number of blocks
m <- 4L

# Size of each block
n <- 5L

# Function to create individual covariance blocks
create_block <- function(n) {
  # Example: Covariance matrix with 1 on the diagonal and 0.5 off-diagonal
  block <- matrix(sample(seq(0.3, 0.9, by = 0.1), 1L), nrow = n, ncol = n)
  diag(block) <- 1
  return(block)
}

# Create a list of blocks
blocks <- replicate(m, create_block(n), simplify = FALSE)

# Combine blocks into a block diagonal matrix
block_diag_matrix <- Matrix::bdiag(blocks)

# Convert to dense matrix if needed
covmat <- as.matrix(block_diag_matrix)

# Print the matrix
print(covmat)

# Test with mvtnorm::rmvnorm
spot_rates <- mvtnorm::rmvnorm(n = 10000L, mean = rep(5.0, n * m), sigma = covmat)

# Verify the result
plot(spot_rates[,1L], spot_rates[,5L])
