module tsproj

# Import necessary packages
using Distributions
using LinearAlgebra
using Random
using SDDP
using JuMP
using HiGHS
using Gurobi

# Include separate function files
include("greet.jl")
include("rmvnorm.jl")
include("PoissonAR1.jl")

# Exported functions
export greet
export rmvnorm
export rpoisar1, qpoisar1, convert_poisar1_to_unif, run_poisar1_simulation

end # module MyPackage
