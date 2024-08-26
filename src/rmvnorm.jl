using Base.Threads  # Import the Threads module for multithreading
using Distributions

"""
    rmvnorm(n::Int, mean::Vector{Float64}, covar::Matrix{Float64}) -> Matrix{Float64}

Generates `n` samples from a multivariate normal distribution with mean `mean` and covariance matrix `covar`.
"""
function rmvnorm(n::Int, mean::Vector{Float64}, covar::Matrix{Float64})
    p = length(mean)
    outputMatrix = Matrix{Float64}(undef, n, p)

    # Create a MultivariateNormal distribution
    mvn = MvNormal(mean, covar)

    # Generate the samples in parallel using threads
    @threads for idx in 1:n
        outputMatrix[idx, :] = rand(mvn)'
    end

    return outputMatrix
end
