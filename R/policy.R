library(TLPR)

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
  env$nCO <- sim$nSpotCarriers
  # env$Cb  <- matrix(sim$carrier_capacity[-((sim$tau-1)*sim$nCarriers+1L:sim$nCarriers)], nrow = env$tau)
  # env$Co  <- matrix(rep(0L, env$nCO * env$tau), nrow = env$tau)
  env$Cb  <- matrix(sim$carrier_capacity[1L:(sim$nCarriers * sim$tau)], nrow = sim$tau)
  env$Co  <- matrix(sim$carrier_capacity[(sim$nCarriers * sim$tau + 1L):((sim$nCarriers + sim$nSpotCarriers) * sim$tau)], nrow = sim$tau)
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

# Read instance from JSON
json_path <- "~/tsproj/instances/instance_12x2x2x2_001.json"
jsonlite::fromJSON(json_path) |>
  list2env(envir = .GlobalEnv)

env <- genenv(.GlobalEnv)

ccx <- carrier_capacity_padded(env)
tlx <- transition_logic(env, q = Q[seq(env$nI)], d = D[seq(env$nJ)])
slx <- storage_limits(env, q = Q[seq(env$nI)])

obj_ <- c(env$alpha, env$CTb, env$CTo[1L,], env$alpha)

A   <- rbind(ccx$A, tlx$A, slx$A)
rhs <- c(ccx$rhs, tlx$rhs, slx$rhs)
sns <- c(ccx$sense, tlx$sense, slx$sense)

model <- multiperiod_expansion(env, Q, D, A, obj_, rhs, sns)

init.state <- c(entry_stock_0, c(rbind(exit_stock_0, exit_short_0)))
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

## Operations optimization
time <- system.time({
  opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
})

post_hoc_simulation <- function(env, x) {
  S.I <- matrix(NA, nrow = (env$tau + 1L) * env$nI, ncol = 1L)
  S.J <- matrix(NA, nrow = (env$tau + 1L) * env$nJ, ncol = 1L)
  allocation <- matrix(NA, env$tau * env$nvars, ncol = 1L)

  offset <- env$nI + 2L * env$nJ
  blk    <- offset + env$nvars

  jdx <- seq(1L, 2L * env$nI, by = 2L)
  for (t in seq(env$tau + 1L)) {
    i <- (t - 1L) * env$nI + env$I_
    j <- (t - 1L) * env$nJ + env$J_
    idx <- (t - 1L) * blk + seq(blk)
    
    S.I[i, 1L] <- x[idx][env$I_]
    S.J[j, 1L] <- x[idx][env$nI + jdx] - x[idx][env$nI + jdx + 1L]
    
    if (t > env$tau) 
      break
    
    allocation[(t - 1L) * env$nvars + seq(env$nvars), 1L] <- x[idx][offset + seq(env$nvars)]
  }

  list(S.I = S.I, S.J = S.J, allocation = allocation)
}

TLPR::post_hoc_simulation(env, opt$x) |>
  list2env(envir = .GlobalEnv)

carriers <- unlist(mapply(rep, 1L:env$nCS, env$nLc[-1L]))
carriers <- c(carriers, rep(env$nCS + (1L:env$nCO), each = env$nL))
lanes    <- c(env$L_, rep(1L:env$nL, env$nCO))
dt <- data.table::as.data.table(cbind(
  t       = rep(seq(env$tau), each = env$nvars), 
  lane    = lanes, 
  carrier = carriers,
  origin  = env$L[lanes, 1L], 
  destination = env$L[lanes, 2L] + env$nI, 
  assignment  = allocation[, 1L]))
dtx <- rbind(
  dt[, .(assignment = sum(assignment, na.rm = TRUE)), by = .(t, origin, destination)],
  data.table::data.table(
    t      = rep(env$tau + 1L, env$nL), 
    origin = rep(env$I_, each = env$nJ), 
    destination = env$nI + rep(env$J_, env$nI), 
    assignment  = rep(NA, env$nL)))

env$tau <- 12L
plot2x2instance(env, dtx, S.I, S.J, Q[1L:((env$tau - 1L) * env$nI)], D[1L:((env$tau - 1L) * env$nJ)])
env$tau <- 11L

# Plot the volume allocation
cxlxrs <- c(
  "#2F454E", "#6F98A8", "#BFBFBF",
  "#FCA91C", "#2B8EAD", "#72C3DC" 
)

market <- c("contract", "contract", "spot", "spot")

dt[, .(lane = paste0(origin, " \u2192 ", destination), segment = paste0("Carrier ", ((carrier - 1L) %% env$nCS) + 1L, " (", market[carrier], ")")), by = .(t, carrier, assignment)] |>
  ggplot2::ggplot(ggplot2::aes(x = t, y = assignment, fill = lane)) +
  ggplot2::geom_bar(stat = "identity", position = "stack") +
  ggplot2::facet_wrap(~segment, scales = 'free_y') +
  # ggplot2::scale_fill_viridis_d(option = "C") + 
  ggplot2::scale_fill_manual(values = cxlxrs) + 
  ggplot2::theme_minimal() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  ggplot2::labs(title = 'Carrier Utilization', x = 'Time Stage', y = 'Assigned TEUs') +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    title = ggplot2::element_text(size = 16L, face = "bold"),  # Increase plot title font size
    legend.position = "top",                # Move legend to the top
    legend.title = ggplot2::element_blank(), # Increase legend title font size
    legend.text  = ggplot2::element_text(size = 12L),  # Increase legend items font size
    axis.title   = ggplot2::element_text(size = 14L),   # Increase axis titles font size
    axis.text    = ggplot2::element_text(size = 12L),    # Increase axis text font size
    strip.text   = ggplot2::element_text(size = 14L, face = "bold")    # Increase facet labels font size
  )
