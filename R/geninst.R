library(TLPR)


## 12 x 6 x 6 x 20 instances
n_instances <- 100L

for (idx in 1L:n_instances) {
  ic <- new.env()

  with(ic, {
    tau           <- 12L
    nOrigins      <- 6L
    nDestinations <- 6L
    nCarriers     <- 20L
    nBids         <- 10L
    Bids          <- replicate(nBids,     sample(nOrigins * nDestinations, size = sample(6L:18L, 1L), replace = F))
    winners       <- replicate(nCarriers, sample(nBids, size = sample(1L:2L, 1L), replace = F))

    ordx <- unlist(winners, use.names = FALSE)
    Ldx  <- unlist(Bids[ordx], use.names = FALSE)
    nLc  <- c(0L, sapply(winners, function(winner) length(unlist(Bids[winner]))))

    entry_stock_0    <- sample(seq(0L,  500L, by = 50L), nOrigins, replace = T) # c(200L, 100L,  50L,   0L,   0L, 100L)
    exit_stock_0     <- sample(seq(0L, 1000L, by = 50L), nDestinations, replace = T) # c(100L,   0L,   0L, 200L, 100L, 100L)
    exit_short_0     <- c(  0L,   0L,   0L,   0L,   0L,   0L)
    entry_capacity   <- c(10000L, 10000L, 10000L, 10000L, 10000L, 10000L)
    exit_capacity    <- c(10000L, 10000L, 10000L, 10000L, 10000L, 10000L)
    flow_support     <- seq(1000L, 3000L, by = 100L)
    entry_store_coef <- c(20.0, 20.0, 20.0, 20.0, 20.0, 20.0)
    exit_store_coef  <- c(10.0, 10.0, 10.0, 10.0, 10.0, 10.0)
    exit_short_coef  <- c(30.0, 30.0, 30.0, 30.0, 30.0, 30.0)
    transport_coef   <- runif(length(Ldx), 6.0, 8.0)
    spot_coef        <- runif(nOrigins * nDestinations * nCarriers * tau, 3.0, 9.0)
    carrier_capacity <- c(sample(seq(400L, 800L, by = 50L), size = nCarriers * tau, replace = T), rep(40L, nCarriers * tau))

    Q <- sample(seq(1000L, 3000L, by = 100L), tau * nOrigins, replace = T)
    D <- sample(seq(1000L, 3000L, by = 100L), tau * nDestinations, replace = T)
  })

  json <- jsonlite::toJSON(sapply(names(ic), get, envir = ic), pretty = TRUE)
  write(json, file = paste0(
    "~/tsproj/instances/instance_", 
    ic$tau, 'x', 
    ic$nOrigins, 'x', 
    ic$nDestinations, 'x', 
    ic$nCarriers, "_", 
    idx %/% 100L, 
    (idx %/% 10L) %% 10L, 
    idx %% 10L, ".json"))
}

## 12 x 2 x 2 x 2 instances
ic <- new.env()

with(ic, {
  tau           <- 12L
  nOrigins      <- 2L
  nDestinations <- 2L
  nCarriers     <- 2L
  nBids         <- 4L
  Bids          <- replicate(nBids,     sample(nOrigins * nDestinations, size = sample(1L:3L, 1L), replace = F))
  winners       <- replicate(nCarriers, sample(nBids, size = sample(1L:2L, 1L), replace = F))

  ordx <- unlist(winners, use.names = FALSE)
  Ldx  <- unlist(Bids[ordx], use.names = FALSE)
  nLc  <- c(0L, sapply(winners, function(winner) length(unlist(Bids[winner]))))

  entry_stock_0    <- sample(seq(0L, 50L, by = 10L), nOrigins, replace = T)
  exit_stock_0     <- sample(seq(0L, 50L, by = 10L), nDestinations, replace = T)
  exit_short_0     <- c(  0L,   0L)
  entry_capacity   <- c(100L, 100L)
  exit_capacity    <- c(100L, 100L)
  flow_support     <- seq(10L, 30L, by = 5L)
  entry_store_coef <- c(20.0, 20.0)
  exit_store_coef  <- c(10.0, 10.0)
  exit_short_coef  <- c(30.0, 30.0)
  transport_coef   <- runif(length(Ldx), 7.0, 9.9)
  spot_coef        <- runif(nOrigins * nDestinations * nCarriers * tau, 3.5, 8.0)
  carrier_capacity <- c(sample(seq(10L, 20L, by = 5L), size = nCarriers * tau, replace = T), rep(5L, nCarriers * tau))

  Q <- sample(seq(10L, 30L, by = 5L), tau * nOrigins, replace = T)
  D <- sample(seq(10L, 30L, by = 5L), tau * nDestinations, replace = T)
})

json <- jsonlite::toJSON(sapply(names(ic), get, envir = ic), pretty = TRUE)
write(json, file = paste0(
  "~/tsproj/instances/instance_", 
  ic$tau, 'x', 
  ic$nOrigins, 'x', 
  ic$nDestinations, 'x', 
  ic$nCarriers, "_", 
  idx %/% 100L, 
  (idx %/% 10L) %% 10L, 
  idx %% 10L, ".json"))


## 12 x 6 x 6 instance (hard-coded)
ic <- new.env()

ic$tau           <- 12L
ic$nOrigins      <- 6L
ic$nDestinations <- 6L
ic$nCarriers     <- 20L
ic$nBids         <- 10L
ic$Bids          <- list(
  c( 6L, 23L, 35L, 21L,  4L,  1L,  7L, 17L, 33L, 25L, 29L, 32L, 13L, 30L, 31L, 14L),
  c(20L, 28L, 16L, 27L, 31L, 25L, 35L,  2L,  1L, 14L, 18L,  9L, 34L, 10L),
  c(28L,  5L, 24L, 21L, 26L, 19L, 33L, 20L,  8L,  2L, 16L, 29L),
  c(16L, 20L, 32L, 35L, 30L, 19L),
  c( 8L, 21L,  3L, 23L, 24L, 28L, 27L, 30L, 31L,  1L, 33L,  9L, 29L),
  c(35L, 23L, 18L, 17L, 31L, 10L,  2L, 16L,  9L,  6L, 11L,  1L), 
  c(33L, 31L, 28L,  4L, 29L, 30L,  5L, 16L,  9L, 20L,  1L, 36L, 32L), 
  c(18L, 24L, 32L, 31L, 26L, 25L, 23L, 19L, 36L, 20L, 27L, 29L,  1L, 34L, 11L), 
  c(13L, 20L, 14L, 29L, 27L,  8L, 26L, 19L, 11L, 12L,  6L, 24L,  5L, 34L, 17L),
  c( 9L, 12L,  6L,  8L, 29L,  1L, 36L,  4L, 33L, 30L, 17L)
)
ic$winners       <- list(
  c(10L, 6L), 
  c(8L), 
  c(3L, 6L), 
  c(3L), 
  c(7L), 
  c(8L), 
  c(7L), 
  c(9L), 
  c(6L), 
  c(8L, 6L), 
  c(2L), 
  c(6L, 1L), 
  c(4L, 3L), 
  c(1L, 3L), 
  c(5L), 
  c(1L, 4L), 
  c(3L, 8L), 
  c(5L), 
  c(1L, 4L), 
  c(1L) 
)
ic$entry_stock_0    <- c(200L, 100L,  50L,   0L,   0L, 100L)
ic$exit_stock_0     <- c(100L,   0L,   0L, 200L, 100L, 100L)
ic$exit_short_0     <- c(  0L,   0L,   0L,   0L,   0L,   0L)
ic$entry_capacity   <- c(5000L, 5000L, 5000L, 5000L, 5000L, 5000L)
ic$exit_capacity    <- c(5000L, 5000L, 5000L, 5000L, 5000L, 5000L)
ic$flow_support     <- seq(1000L, 3000L, by = 100L)
ic$entry_store_coef <- c(20.0, 20.0, 20.0, 20.0, 20.0, 20.0)
ic$exit_store_coef  <- c(10.0, 10.0, 10.0, 10.0, 10.0, 10.0)
ic$exit_short_coef  <- c(30.0, 30.0, 30.0, 30.0, 30.0, 30.0)
ic$transport_coef   <- c(
  6.21, 6.67, 5.08, 8.67, 5.09, 5.63, 7.65, 4.90, 5.70, 7.38, 6.78, 6.22, 5.69, 5.62, 7.25, 
  6.31, 5.47, 5.21, 4.70, 4.34, 7.07, 5.94, 6.42, 5.87, 4.16, 4.64, 5.90, 6.20, 6.81, 7.47, 
  5.85, 5.66, 5.05, 5.10, 6.37, 5.84, 6.89, 5.13, 4.79, 4.45, 6.31, 7.21, 6.75, 6.96, 5.99, 
  5.61, 5.46, 4.99, 6.07, 4.95, 5.78, 7.35, 3.58, 7.44, 5.50, 5.90, 7.30, 7.22, 5.65, 6.83, 
  6.59, 6.83, 7.48, 7.06, 6.27, 7.93, 6.19, 5.52, 5.17, 5.72, 5.51, 8.71, 6.02, 5.26, 7.42, 
  5.38, 7.84, 6.06, 5.43, 5.30, 6.14, 6.94, 3.57, 6.47, 5.88, 4.52, 8.06, 6.03, 4.86, 5.00, 
  6.45, 2.15, 6.22, 5.32, 5.84, 6.22, 5.56, 4.78, 5.94, 5.90, 5.78, 5.63, 6.49, 7.23, 4.94, 
  6.24, 6.60, 4.64, 5.80, 6.78, 6.28, 5.19, 6.76, 7.21, 5.86, 5.98, 7.63, 5.39, 5.44, 4.61, 
  5.22, 4.97, 5.32, 6.36, 6.02, 6.13, 5.58, 6.10, 5.85, 6.69, 4.86, 5.69, 6.92, 6.56, 7.03, 
  4.71, 5.96, 6.04, 4.23, 7.58, 5.20, 6.90, 5.21, 7.00, 7.62, 6.28, 7.47, 6.00, 5.89, 7.86, 
  4.91, 5.72, 6.91, 6.75, 5.49, 5.79, 6.33, 7.00, 6.26, 6.13, 7.24, 8.42, 8.72, 4.80, 6.20, 
  6.49, 7.42, 5.84, 5.80, 5.90, 5.97, 4.87, 6.35, 4.98, 7.63, 5.61, 6.74, 6.41, 6.77, 7.06, 
  4.70, 4.29, 5.74, 6.56, 6.21, 6.13, 6.01, 6.25, 7.97, 5.24, 4.66, 7.52, 5.19, 4.95, 6.19, 
  6.49, 7.39, 6.05, 4.35, 7.27, 5.73, 5.58, 4.81, 4.01, 5.72, 5.03, 5.91, 6.02, 8.06, 7.10, 
  5.57, 6.41, 5.44, 5.46, 6.48, 5.14, 6.26, 7.47, 4.90, 8.29, 6.17, 7.41, 5.74, 6.65, 6.75, 
  6.30, 4.82, 5.66, 5.54, 5.47, 7.27, 4.56, 6.86, 4.24, 6.95, 6.73, 4.62, 6.34, 6.70, 4.48, 
  7.89, 6.46, 4.94, 5.69, 4.87, 6.20, 4.90, 5.24, 8.35, 5.30, 6.03, 7.68, 5.21, 5.48, 6.13, 
  4.24, 5.46, 6.08, 5.53, 4.41, 5.21, 5.98, 5.95, 4.76, 8.25, 6.01, 6.14, 6.94, 4.80, 6.74, 
  6.79, 5.72, 7.94, 6.06, 6.32, 7.41, 8.44, 6.27, 4.90, 7.82, 5.30, 6.92, 4.92, 5.81, 5.26, 
  4.59, 7.24, 5.63, 6.56, 5.43, 5.95, 6.15, 5.53, 5.06, 6.54, 4.97, 7.40, 5.77, 6.28, 6.78, 
  4.60, 5.54, 4.83, 6.61, 5.32, 4.89, 2.63, 5.90, 6.57, 3.56, 6.98, 8.08, 6.26, 5.67, 6.50, 
  5.22, 3.43, 7.05, 5.20, 5.22, 6.49, 6.40, 7.20, 5.56, 6.21, 5.85, 3.13, 4.25, 7.08, 7.50, 
  6.60, 2.99, 4.88, 6.00, 6.31, 5.61, 4.77, 6.67, 6.76, 5.01, 5.06, 5.25, 4.40, 4.88, 6.50, 
  5.97, 5.80, 5.31, 6.16, 5.99, 6.85, 5.78, 7.32, 6.84, 5.62, 7.18, 4.34, 5.44, 5.07, 5.74, 
  6.31, 5.87, 5.28, 6.77, 5.47, 6.39, 5.83, 7.07, 4.30, 5.28)
ic$carrier_capacity <- c(
  800L, 700L, 700L, 500L, 700L, 500L, 700L, 800L, 400L, 400L, 500L, 700L, 
  400L, 500L, 500L, 800L, 500L, 700L, 500L, 400L, 400L, 700L, 800L, 500L, 
  700L, 800L, 800L, 500L, 500L, 800L, 400L, 800L, 800L, 700L, 500L, 500L, 
  600L, 400L, 500L, 800L, 800L, 700L, 700L, 400L, 400L, 700L, 700L, 400L, 
  400L, 400L, 500L, 700L, 600L, 600L, 500L, 700L, 600L, 600L, 500L, 700L, 
  800L, 400L, 500L, 600L, 700L, 400L, 800L, 400L, 600L, 800L, 400L, 700L, 
  800L, 500L, 600L, 700L, 800L, 400L, 800L, 800L, 700L, 400L, 600L, 400L, 
  700L, 600L, 700L, 500L, 400L, 500L, 600L, 400L, 600L, 700L, 700L, 800L, 
  400L, 600L, 500L, 800L, 600L, 500L, 500L, 400L, 800L, 700L, 800L, 500L, 
  400L, 500L, 700L, 700L, 500L, 400L, 400L, 400L, 700L, 800L, 500L, 700L, 
  400L, 800L, 400L, 500L, 400L, 500L, 500L, 700L, 800L, 600L, 400L, 700L, 
  800L, 600L, 800L, 400L, 800L, 500L, 400L, 700L, 400L, 500L, 600L, 700L, 
  500L, 800L, 700L, 800L, 500L, 700L, 400L, 400L, 800L, 400L, 600L, 600L, 
  800L, 400L, 500L, 800L, 600L, 800L, 500L, 400L, 500L, 600L, 800L, 800L, 
  500L, 800L, 500L, 700L, 700L, 600L, 500L, 400L, 800L, 800L, 600L, 500L, 
  600L, 800L, 800L, 600L, 800L, 800L, 400L, 600L, 500L, 500L, 500L, 800L, 
  500L, 700L, 800L, 400L, 500L, 600L, 500L, 800L, 600L, 500L, 500L, 800L, 
  600L, 500L, 700L, 800L, 700L, 700L, 800L, 800L, 500L, 500L, 400L, 700L, 
  700L, 700L, 800L, 800L, 500L, 600L, 600L, 500L, 700L, 700L, 500L, 800L, 
  700L, 600L, 700L, 700L, 700L, 800L, 600L, 400L, 600L, 500L, 700L, 600L
)

json <- jsonlite::toJSON(sapply(names(ic), get, envir = ic), pretty = TRUE)
write(json, file = paste0("~/tsproj/instances/instance_", ic$tau, 'x', ic$nOrigins, 'x', ic$nDestinations, 'x', ic$nCarriers, "_011.json"))

## 12 x 1 x 1 x 2 instances
ic <- new.env()

ic$tau           <- 12L
ic$nOrigins      <- 1L
ic$nDestinations <- 1L
ic$nCarriers     <- 2L
ic$Bids <- list(
  c(1L),
  c(1L)
)
ic$winners <- list(
  c(1L),
  c(2L)
)

ordx <- unlist(ic$winners, use.names = FALSE)
Ldx  <- unlist(ic$Bids[ordx], use.names = FALSE)

ic$entry_stock_0    <- c(200L)
ic$exit_stock_0     <- c(0L)
ic$exit_short_0     <- c(0L)
ic$entry_capacity   <- c(1000L)
ic$exit_capacity    <- c(1000L)
ic$flow_support     <- seq(100L, 500L, by = 50L)
ic$entry_store_coef <- c(20.0)
ic$exit_store_coef  <- c(10.0)
ic$exit_short_coef  <- c(30.0)
ic$transport_coef   <- rnorm(length(Ldx), 7.0, 1.0)
ic$carrier_capacity <- sample(seq(100L, 300L, by = 50L), size = ic$nCarriers * ic$tau, replace = T)

json <- jsonlite::toJSON(sapply(names(ic), get, envir = ic), pretty = TRUE)
write(json, file = paste0("~/tsproj/instances/instance_", ic$tau, 'x', ic$nOrigins, 'x', ic$nDestinations, 'x', ic$nCarriers, "_001.json"))
