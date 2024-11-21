module tsproj

# Import necessary packages
using Distributions
using LinearAlgebra
using DataStructures
using Random
using SDDP
using JuMP
using HiGHS
using Gurobi

# Include separate function files
include("rmvnorm.jl")

# Exported functions
export rmvnorm

end # module MyPackage
