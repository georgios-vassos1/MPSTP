module tsproj

# Import necessary packages
using Distributions
using LinearAlgebra
using Random

# Include separate function files
include("greet.jl")
include("rmvnorm.jl")
include("PoissonAR1.jl")

# Exported functions
export greet
export rmvnorm
export simulate_poisson_ar1, calculate_probabilities, run_poisson_ar1_simulation

end # module MyPackage
