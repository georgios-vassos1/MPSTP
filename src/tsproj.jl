module tsproj

# Import necessary packages
using Distributions
using LinearAlgebra
using Random

# Include separate function files
include("greet.jl")
include("rmvnorm.jl")

# Exported functions
export greet, rmvnorm

end # module MyPackage
