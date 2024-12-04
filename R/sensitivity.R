library(TLPR)

num_instances <- 100L

sims <- replicate(num_instances, new.env(), simplify = FALSE)
for (idx in 1L:num_instances) {
  json_path <- paste0(
    "~/tsproj/instances/instance_", 
    12L, 'x', 
     6L, 'x', 
     6L, 'x', 
    20L, "_", 
     idx %/% 100L, 
    (idx %/% 10L) %% 10L, 
     idx %% 10L, ".json")
  jsonlite::fromJSON(json_path) |>
    list2env(envir = sims[[idx]])
}

# Create the environment for the model object
genenv <- function(sim) {
  # Auxiliary data
  fromx <- outer(1L:sim$nOrigins, (1L:sim$nDestinations - 1) * sim$nOrigins, '+')
  tox   <- outer((1L:sim$nDestinations - 1L) * sim$nOrigins, 1L:sim$nOrigins, '+')

  env <- new.env()

  env$tau <- sim$tau - 1L
  env$nI  <- sim$nOrigins
  env$nJ  <- sim$nDestinations
  env$I_  <- 1L:env$nI
  env$J_  <- 1L:env$nJ
  env$R   <- max(sim$exit_capacity) # exit_capacity
  env$nL  <- env$nI * env$nJ
  env$nCS <- sim$nCarriers
  env$nCO <- sim$nCarriers
  # env$Cb  <- matrix(sim$carrier_capacity[-((sim$tau-1)*sim$nCarriers+1L:sim$nCarriers)], nrow = env$tau)
  # env$Co  <- matrix(rep(0L, env$nCO * env$tau), nrow = env$tau)
  env$Cb  <- matrix(sim$carrier_capacity[1L:(sim$nCarriers * sim$tau)], nrow = sim$tau)
  env$Co  <- matrix(sim$carrier_capacity[(sim$nCarriers * sim$tau + 1L):(2L * sim$nCarriers * sim$tau)], nrow = sim$tau)
  env$nLc <- sim$nLc
  env$L_  <- sim$Ldx
  env$nL_ <- length(sim$Ldx)
  env$nvars  <- length(sim$Ldx) + env$nCO * (sim$nOrigins * sim$nDestinations)
  env$CS     <- 1L:env$nCS
  env$from_i <- apply(fromx, 1L, function(ldx) which(c(sim$Ldx, rep(seq(env$nL), env$nCO)) %in% ldx), simplify = F)
  env$to_j   <- apply(tox,   1L, function(ldx) which(c(sim$Ldx, rep(seq(env$nL), env$nCO)) %in% ldx), simplify = F)

  env$CTb   <- sim$transport_coef
  # env$CTo   <- matrix(rnorm(env$tau * env$nCO * env$nL, 4.0, 1.0), nrow = env$tau)
  # env$CTo   <- matrix(sim$spot_coef[1L:((sim$tau - 1L) * sim$nCarriers * sim$nOrigins * sim$nDestinations)], nrow = env$tau)
  env$CTo   <- matrix(sim$spot_coef, nrow = sim$tau)
  env$alpha <- c(sim$entry_store_coef, c(rbind(sim$exit_store_coef, sim$exit_short_coef)))

  env
}

generate_model <- function(env, init.state, Q, D) {
  ccx <- carrier_capacity_padded(env)
  tlx <- transition_logic(env, q = Q[seq(env$nI)], d = D[seq(env$nJ)])
  slx <- storage_limits(env, q = Q[seq(env$nI)])

  obj_ <- c(env$alpha, env$CTb, env$CTo[1L,], env$alpha)

  A   <- rbind(ccx$A, tlx$A, slx$A)
  rhs <- c(ccx$rhs, tlx$rhs, slx$rhs)
  sns <- c(ccx$sense, tlx$sense, slx$sense)

  model <- multiperiod_expansion(env, Q, D, A, obj_, rhs, sns)

  model$A <- rbind(
    Matrix::spMatrix(
      ncol = ncol(model$A), 
      nrow =  env$nI + 2L * env$nJ, 
      i = 1L:(env$nI + 2L * env$nJ), 
      j = 1L:(env$nI + 2L * env$nJ), 
      x = rep(1L, env$nI + 2L * env$nJ)
    ), model$A)
  model$sense <- c(rep("=", env$nI + 2L * env$nJ), model$sense)
  model$rhs   <- c(init.state, model$rhs)

  model$modelsense <- 'min'
  model$vtype <- rep('I', ncol(model$A))

  model
}

N <- 1000L
Q <- sample(seq(1000L, 3000L, by = 100L), 12L * 6L * N, replace = TRUE) |> matrix(nrow = N)

times <- numeric(num_instances)
optx  <- numeric(num_instances * N)
for (idx in 1L:num_instances) {
  print(idx)
  env <- genenv(sims[[idx]])

  # Indexing the model object for the Q variables
  pdx    <- (env$nI + 2L * env$nJ + env$nCS + env$nCO)
  offset <- (2L * (env$nI + env$nJ) + env$nCS + env$nCO)
  pdx    <- pdx + offset * 0L:(env$tau - 1L)

  Qdx.1 <- c(outer(env$I_, pdx, '+'))
  Qdx.2 <- c(env$nI + env$nJ + Qdx.1)

  init.state <- c(sims[[idx]]$entry_stock_0, c(rbind(sims[[idx]]$exit_stock_0, sims[[idx]]$exit_short_0)))
  model <- generate_model(env, init.state, sims[[idx]]$Q[-(11L * env$nI + env$I_)], sims[[idx]]$D[-(11L * env$nJ + env$J_)])

  job <- function(k) {
    model$rhs[Qdx.1] <- model$rhs[Qdx.2] <- Q[k,]
    opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
    opt$objval
  }

  timex <- Sys.time()
  cl <- parallel::makeCluster(parallel::detectCores() - 2L)
  parallel::clusterExport(cl, c("model", "env", "Qdx.1", "Qdx.2", "Q"))
  optx[(idx - 1L) * N + seq(N)] <- parallel::parLapply(cl, seq(N), job) |> as.numeric()
  parallel::stopCluster(cl)

  times[idx] <- as.numeric(Sys.time() - timex)
}

# Empty plot
densities <- apply(matrix(optx, nrow = N), 2L, density)

ylim <- range(sapply(densities, function(d) d$y))
# Initialize an empty plot with appropriate limits
plot(NA, xlim = range(sapply(densities, function(d) d$x)),
     ylim = ylim,
     xlab = "Total Cost", ylab = "Density", main = "Objective Densities: 100 Instances, 1000 Inflow Samples", xaxt = "n", yaxt = "n")

# Add density lines in a loop
for (i in seq_along(densities)) {
  lines(densities[[i]], col = rgb(0.0, 0.0, 0.0, alpha = 0.2), lwd = 2L)
}
rngx <- seq(min(optx), max(optx), length.out = 20L)
axis(1L, at = rngx, labels = round(rngx / 1e6, 1L))
rngy <- seq(0.0, ylim[2L], length.out = 4L)
axis(2L, at = rngy, labels = round(rngy / 1e-06, 1L))
mtext(expression("x"~10^-6), side = 3L, line = 1L, at = par("usr")[1L])
mtext(expression("x"~10^6), side = 1L, line = 1L, at = par("usr")[2L])

# ## Operations optimization
# opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
# opt$objbound

# # Test indices
# model$rhs[Qdx.1] == sims[[idx]]$Q[-(11L * env$nI + env$I_)]
# model$rhs[Qdx.2] == sims[[idx]]$Q[-(11L * env$nJ + env$J_)]


# N <- 1000L
# Q <- sample(seq(1000L, 3000L, by = 100L), env$tau * env$nI * N, replace = TRUE) |> matrix(nrow = N)
# 
# timex <- Sys.time()
# 
# cl <- parallel::makeCluster(parallel::detectCores() - 2L)
# parallel::clusterExport(cl, c("model", "env", "Qdx.1", "Qdx.2", "Q"))
# 
# optx <- parallel::parLapply(cl, seq(N), job) |> as.numeric()
# 
# parallel::stopCluster(cl)
# 
# timex <- Sys.time() - timex
# 
# hist(optx, main = 'Objective values', xlab = 'Objective value (millions)', ylab = 'Density', col = 'lightblue', border = 'white', breaks = 20L, freq = F, xaxt = "n")
# rngx <- seq(min(optx), max(optx), length.out = 20L)
# axis(1L, at = rngx, labels = round(rngx / 1e6, 1L))
# density(optx) |> lines(main = 'Density of objective values', xlab = 'Objective value (millions)', ylab = 'Density', col = 'red')

################################################################################################
# N <- 100L
# obj.vals <- numeric(N)
# for (i in 1L:N) {
#   Q <- sample(seq(1000L, 3000L, by = 100L), env$tau * env$nI, replace = T)
#   model$rhs[Qdx.1] <- model$rhs[Qdx.2] <- Q
#   opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
#   obj.vals[i] <- opt$objval
# }
# hist(obj.vals, main = 'Objective values', xlab = 'Objective value', ylab = 'Frequency', col = 'lightblue', border = 'white', breaks = 10L)
################################################################################################