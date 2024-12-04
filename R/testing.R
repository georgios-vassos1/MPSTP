library(TLPR)

# Read instance from JSON
json_path <- "~/tsproj/instances/instance_12x6x6x20_001.json"
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
opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
opt$objbound

# Compute SDDP regret
sddp_obj <- rhdf5::h5read("~/tsproj/output/12x6x6x20_10x1000_sims.h5", "12x6x6x20_Unif10x1500_obj_oob10e3")
sddp_ksi <- rhdf5::h5read("~/tsproj/output/12x6x6x20_10x1000_sims.h5", "12x6x6x20_Unif10x1500_ksi_oob10e3")

taux <- env$tau + 1L

n <- length(sddp_obj)
regret <- numeric(n)
for (i in 1L:n) {
  if ( i %% 100L == 0 ) {
    cat(i, "\n")
  }
  Q <- c(t(sddp_ksi[(i - 1L) * taux + 1L:taux, 1L:6L]))
  D <- c(t(sddp_ksi[(i - 1L) * taux + 1L:taux, 7L:12L]))

  tlx <- transition_logic(env, q = Q[seq(env$nI)], d = D[seq(env$nJ)])
  slx <- storage_limits(env, q = Q[seq(env$nI)])

  A   <- rbind(ccx$A, tlx$A, slx$A)
  rhs <- c(ccx$rhs, tlx$rhs, slx$rhs)

  model <- multiperiod_expansion(env, Q, D, A, obj_, rhs, sns)
  # Initial state constraint
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

  opt <- gurobi::gurobi(model, params = list(OutputFlag = 0L))
  regret[i] <- (sddp_obj[i] - opt$objbound) / opt$objbound
}
hist(regret, breaks = 50L, main = "Out-of-sample regret distribution (1,000 trials)", xlab = "Regret", freq = T)
