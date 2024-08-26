using Distributions
using DataFrames

"""
    simulate_poisson_ar1(alpha::Float64, beta::Float64, n::Int) -> Vector{Int}

Simulate a Poisson AR(1) process.

# Arguments
- `alpha::Float64`: The intercept or base rate of the process.
- `beta::Float64`: The AR(1) coefficient.
- `n::Int`: The number of time points to simulate.

# Returns
- `Vector{Int}`: A vector representing the simulated Poisson AR(1) time series.
"""
function simulate_poisson_ar1(alpha::Float64, beta::Float64, n::Int) :: Vector{Int}
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
    calculate_probabilities(Y::Vector{Int}, alpha::Float64, beta::Float64) -> Vector{Float64}

Calculate the probabilities of the observed Poisson AR(1) time series.

# Arguments
- `Y::Vector{Int}`: The observed Poisson AR(1) time series.
- `alpha::Float64`: The intercept or base rate of the process.
- `beta::Float64`: The AR(1) coefficient.

# Returns
- `Vector{Float64}`: A vector representing the probability of each observed value given the previous value.
"""
function calculate_probabilities(Y::Vector{Int}, alpha::Float64, beta::Float64) :: Vector{Float64}
    n = length(Y)
    probs = Vector{Float64}(undef, n)
    probs[1] = cdf(Poisson(alpha), Y[1])  # Initial probability for Y_1
    
    for t in 2:n
        lambda_t = alpha + beta * Y[t - 1]
        probs[t] = cdf(Poisson(lambda_t), Y[t])
    end
    
    return probs
end

"""
    run_poisson_ar1_simulation(alpha::Float64, beta::Float64, n::Int) -> DataFrame

Run the full Poisson AR(1) simulation and return the results in a DataFrame.

# Arguments
- `alpha::Float64`: The intercept or base rate of the process.
- `beta::Float64`: The AR(1) coefficient.
- `n::Int`: The number of time points to simulate.

# Returns
- `DataFrame`: A DataFrame containing the time points, simulated values, and their corresponding probabilities.
"""
function run_poisson_ar1_simulation(alpha::Float64, beta::Float64, n::Int)
    Y = simulate_poisson_ar1(alpha, beta, n)
    probs = calculate_probabilities(Y, alpha, beta)
    result = DataFrame(Time = 1:n, Y = Y, Probability = probs)
    
    return result
end
