using LinearAlgebra
using Random
using Base.Threads  # Import the Threads module for multithreading

"""
    multivariateNormalSample(mean::Vector{Float64}, covar::Matrix{Float64}, gen::Random.AbstractRNG) -> Vector{Float64}

Generates a single sample from a multivariate normal distribution with mean `mean` and covariance matrix `covar`.
"""
function multivariateNormalSample(mean::Vector{Float64}, covar::Matrix{Float64}, gen::Random.AbstractRNG)
    p = length(mean)
    # Generate a vector of independent standard normal random variables
    randomVec = randn(gen, p)

    # Compute the Cholesky decomposition of the covariance matrix
    cholSolver = cholesky(covar)

    # Ensure the decomposition was successful
    if cholSolver.info != 0
        throw(ArgumentError("Cholesky decomposition failed"))
    end

    # Compute the sample as mean + L * randomVec
    return mean + cholSolver.L * randomVec
end

"""
    rmvnorm(n::Int, mean::Vector{Float64}, covar::Matrix{Float64}) -> Matrix{Float64}

Generates `n` samples from a multivariate normal distribution with mean `mean` and covariance matrix `covar`.
"""
function rmvnorm(n::Int, mean::Vector{Float64}, covar::Matrix{Float64})
    p = length(mean)
    outputMatrix = Matrix{Float64}(undef, n, p)

    # Generate the samples in parallel using threads
    @threads for idx in 1:n
        outputMatrix[idx, :] = multivariateNormalSample(mean, covar, Random.default_rng())
    end

    return outputMatrix
end
