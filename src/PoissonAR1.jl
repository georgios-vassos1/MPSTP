using Distributions
using DataFrames

"""
    rpoisar1(alpha::Float64, beta::Float64, n::Int) -> Vector{Int}

Simulate a Poisson AR(1) process.

# Arguments
- `alpha::Float64`: The intercept or base rate of the process.
- `beta::Float64`: The AR(1) coefficient.
- `n::Int`: The number of time points to simulate.

# Returns
- `Vector{Int}`: A vector representing the simulated Poisson AR(1) time series.
"""
function rpoisar1(alpha::Float64, beta::Float64, n::Int) :: Vector{Int}
    # Initialize the time series
    Y = Vector{Int}(undef, n)
    Y[1] = rand(Poisson(alpha))  # Initial value

    # Simulate the Poisson AR(1) process
    for t in 2:n
        lambda_t = alpha + beta * Y[t - 1]
        Y[t] = rand(Poisson(lambda_t))
    end

    return Y
end

"""
    qpoisar1(U::Vector{Float64}, alpha::Float64, beta::Float64) -> Vector{Int}

Convert uniform data to Poisson AR(1) data using the given AR(1) parameters.

# Arguments
- `U::Vector{Float64}`: The uniform random numbers (generated from a Gaussian copula or similar).
- `alpha::Float64`: The intercept or base rate of the AR(1) process.
- `beta::Float64`: The AR(1) coefficient.

# Returns
- `Vector{Int}`: A vector representing the Poisson AR(1) time series.
"""
function qpoisar1(U::Vector{Float64}, alpha::Float64, beta::Float64) :: Vector{Int}
    n = length(U)

    # Initialize the AR(1) lambda process
    lambda = Vector{Float64}(undef, n)
    lambda[1] = alpha / (1 - beta)  # Stationary initial value

    # Generate the AR(1) process
    for t in 2:n
        lambda[t] = alpha + beta * lambda[t - 1]
    end

    # Convert uniform data to Poisson AR(1) data
    poisson_ar1_data = Vector{Int}(undef, n)
    for t in 1:n
        poisson_ar1_data[t] = quantile(Poisson(lambda[t]), U[t])
    end

    return poisson_ar1_data
end

"""
    convert_poisar1_to_unif(Y::Vector{Int}, alpha::Float64, beta::Float64) -> Vector{Float64}

Calculate the probabilities of the observed Poisson AR(1) time series.

# Arguments
- `Y::Vector{Int}`: The observed Poisson AR(1) time series.
- `alpha::Float64`: The intercept or base rate of the process.
- `beta::Float64`: The AR(1) coefficient.

# Returns
- `Vector{Float64}`: A vector representing the probability of each observed value given the previous value.
"""
function convert_poisar1_to_unif(Y::Vector{Int}, alpha::Float64, beta::Float64) :: Vector{Float64}
    n = length(Y)
    U = Vector{Float64}(undef, n)
    U[1] = cdf(Poisson(alpha), Y[1])  # Initial probability for Y_1

    for t in 2:n
        lambda_t = alpha + beta * Y[t - 1]
        U[t] = cdf(Poisson(lambda_t), Y[t])
    end

    return U 
end

"""
    run_poisar1_simulation(alpha::Float64, beta::Float64, n::Int) -> DataFrame

Run the full Poisson AR(1) simulation and return the results in a DataFrame.

# Arguments
- `alpha::Float64`: The intercept or base rate of the process.
- `beta::Float64`: The AR(1) coefficient.
- `n::Int`: The number of time points to simulate.

# Returns
- `DataFrame`: A DataFrame containing the time points, simulated values, and their corresponding probabilities.
"""
function run_poisar1_simulation(alpha::Float64, beta::Float64, n::Int)
    Y  = rpoisar1(alpha, beta, n)
    U  = convert_poisar1_to_unif(Y, alpha, beta)
    df = DataFrame(Time = 1:n, Y = Y, U = U)

    return df 
end
