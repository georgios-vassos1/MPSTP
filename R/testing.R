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
  env$L   <- TLPR::CartesianProductX(env$I_, env$J_)
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

# # Read instance from JSON
# json_path <- "~/tsproj/instances/instance_12x6x6x20_001.json"
# jsonlite::fromJSON(json_path) |>
#   list2env(envir = .GlobalEnv)
# 
# env <- genenv(.GlobalEnv)

# idx <- 1L
# env <- genenv(sims[[idx]])

# init.state <- c(sims[[idx]]$entry_stock_0, c(rbind(sims[[idx]]$exit_stock_0, sims[[idx]]$exit_short_0)))
# model <- generate_model(
#   env, 
#   init.state, 
#   sims[[idx]]$Q[-(env$tau * env$nI + env$I_)], 
#   sims[[idx]]$D[-(env$tau * env$nJ + env$J_)]
# )

# ## Operations optimization
# opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
# opt$objbound

# Initialize the cluster outside the loop
cl <- parallel::makeCluster(parallel::detectCores() - 2L)

regrets <- matrix(NA, nrow = num_instances, ncol = 1000L)
for (idx in 1L:num_instances) {
  if (idx %% 10L == 0L) {
    cat(idx, "\n")
  }
  env <- genenv(sims[[idx]])

  init.state <- c(sims[[idx]]$entry_stock_0, c(rbind(sims[[idx]]$exit_stock_0, sims[[idx]]$exit_short_0)))
  model <- generate_model(
    env, 
    init.state, 
    sims[[idx]]$Q[-(env$tau * env$nI + env$I_)], 
    sims[[idx]]$D[-(env$tau * env$nJ + env$J_)]
  )

  taux <- env$tau + 1L

  # Indexing the model object for the Q and D variables
  pdx    <- (env$nI + 2L * env$nJ + env$nCS + env$nCO)
  offset <- (2L * (env$nI + env$nJ) + env$nCS + env$nCO)
  pdx    <- pdx + offset * 0L:(env$tau - 1L)

  Qdx.1 <- c(outer(env$I_, pdx, '+'))
  Qdx.2 <- c(env$nI + env$nJ + Qdx.1)
  Ddx   <- Qdx.1 + env$nI

  # Load SDDP data
  sddp_obj <- rhdf5::h5read(
    "~/tsproj/output/12x6x6x20_10x1500_sims.h5", 
    paste0("obj_oob10e3_", paste0(idx %/% 100L, (idx %/% 10L) %% 10L, idx %% 10L))
  )
  sddp_ksi <- rhdf5::h5read(
    "~/tsproj/output/12x6x6x20_10x1500_sims.h5", 
    paste0("ksi_oob10e3_", paste0(idx %/% 100L, (idx %/% 10L) %% 10L, idx %% 10L))
  )

  N <- length(sddp_obj)

  # Define the job function
  job <- function(i) {
    Q <- c(t(sddp_ksi[(i - 1L) * taux + 1L:taux, env$I_]))
    D <- c(t(sddp_ksi[(i - 1L) * taux + 1L:taux, env$nI + env$J_]))

    model$rhs[Qdx.1] <- model$rhs[Qdx.2] <- Q[-((env$tau) * env$nI + env$I_)]
    model$rhs[Ddx]   <- D[-((env$tau) * env$nJ + env$J_)]

    opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))

    (sddp_obj[i] - opt$objbound) / opt$objbound
  }

  # Update environment-specific variables on the cluster
  parallel::clusterExport(cl, c(
      "model", "env", "taux", "N", 
      "Qdx.1", "Qdx.2", "Ddx", 
      "sddp_obj", "sddp_ksi"
    )
  )

  # Parallel computation
  regrets[idx,] <- parallel::parLapply(cl, seq(N), job) |> as.numeric()
}

# Stop the cluster after the loop
parallel::stopCluster(cl)

## Visualize stats
hist(regrets[1L,], breaks = seq(0, max(regrets), by = 0.00001), main = "Out-of-bag regret distribution (10,000 trials)", xlab = "Regret", freq = F)


par(mfrow = c(1L, 3L))

regrets <- readRDS("~/tsproj/output/regrets100x1000.RDS")
for (key in names(regrets)) {
  iters <- as.numeric(gsub("SDDPx", "", key))
  # Regret densities
  densities <- apply(regrets[[key]], 1L, density)

  ylim <- range(sapply(densities, function(d) d$y))
  # Initialize an empty plot with appropriate limits
  plot(NA, xlim = pmin(range(sapply(densities, function(d) d$x)), 8.9 * 10e-6),
       ylim = pmin(ylim, 500000.0),
       xlab = "Regret", ylab = "Density", main = paste0(iters, " SDDP Iterations"), 
       xaxt = "n", yaxt = "n", cex.lab = 1.5, cex.main = 1.5)
  # Add density lines in a loop
  for (i in seq_along(densities)) {
    lines(densities[[i]], col = rgb(0.0, 0.0, 0.0, alpha = 0.1), lwd = 2L)
  }
  rngx <- seq(min(regrets[[key]]), max(regrets[[key]]), length.out = 20L)
  axis(1L, at = rngx[1L:9L], labels = round(rngx * 1e6, 1L)[1L:9L], cex.axis = 1.5)
  mtext(expression("x"~10^-6), side = 1L, line = 1L, at = par("usr")[2L])

  rngy <- seq(0.0, min(ylim[2L], 500000.0), length.out = 6L)
  axis(2L, at = rngy, labels = rngy / 1e6, cex.axis = 1.5)
  mtext(expression("x"~10^6), side = 3L, line = 1L, at = par("usr")[1L])
}

par(mfrow = c(1L, 1L))

################################################################################
regret <- numeric(N)
for (i in 1L:N) {
  if ( i %% 100L == 0 ) {
    cat(i, "\n")
  }
  Q <- c(t(sddp_ksi[(i - 1L) * taux + 1L:taux, 1L:6L]))
  D <- c(t(sddp_ksi[(i - 1L) * taux + 1L:taux, 7L:12L]))

  model$rhs[Qdx.1] <- model$rhs[Qdx.2] <- Q[-((env$tau) * env$nI + env$I_)]
  model$rhs[Ddx]   <- D[-((env$tau) * env$nJ + env$J_)]

  opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
  regret[i] <- (sddp_obj[i] - opt$objbound) / opt$objbound
}
################################################################################

## Logs analysis
# Create a regular expression to handle key-value pairs and special cases

logs_analyzer <- function(base, iterations = 500L, num_instances = 100L) {
  results <- matrix(NA, nrow = 100L, ncol = 6L)
  for (idx in 1L:num_instances) {
    log_file <- paste0(
      file.path(base, "SDDPx"), iterations, "_", 
      idx %/% 100L, 
      (idx %/% 10L) %% 10L, 
      idx %% 10L, ".log")

    log_text <- readLines(log_file)

    # Extract the section of interest
    log_lines <- strsplit(log_text, "\n")
    summary_start <- grep("^status", log_lines) + 1L
    summary_lines <- log_lines[summary_start:(length(log_lines) - 2L)]

    kv_pattern <- "^([^:]+):\\s*(.+)$"
    ci_pattern <- "(.+) ± (.+)"

    ldx <- 1L
    # Process lines
    for (line in summary_lines) {
      if (grepl(kv_pattern, line)) {
        # Extract key and value
        matches <- regmatches(line, regexec(kv_pattern, line))[[1L]]
        value   <- trimws(matches[3L])
        
        if (grepl(ci_pattern, value)) {
          # Split value and uncertainty for simulation ci
          ci_matches <- regmatches(value, regexec(ci_pattern, value))[[1L]]
          results[idx,c(ldx, ldx + 1L)] <- c(as.numeric(trimws(ci_matches[2L])), as.numeric(trimws(ci_matches[3L])))
          ldx <- ldx + 2L
        } else {
          key   <- trimws(matches[2L])
          value <- as.numeric(value)
          results[idx,ldx] <- value
          ldx <- ldx + 1L
        }
      }
    }
  }
  results
}

results <- logs_analyzer("~/tsproj/logs", 1000L)

mean(results[,1L])
1.96 * sd(results[,1L]) / sqrt(num_instances)

unique(results[,2L])

bias <- (results[,4L] - results[,3L]) / results[,4L]
mean(bias)
1.96 * sd(bias) / sqrt(num_instances)

ci_ratio <- results[,5L] / results[,4L]
mean(ci_ratio)
1.96 * sd(ci_ratio) / sqrt(num_instances)
