using tsproj
using Distributions

# Define the parameters for the multivariate normal distribution
n  = 100
mu = [0.0, 0.0]
covar = [1.0 0.5; 0.5 1.0]

x = rmvnorm(n, mu, covar)

# Define parameters for the AR(1) Poisson process
alpha = 1.0
beta  = 0.5

# Convert the multivariate normal samples to uniform and apply qpoisar1
flow_data = mapslices(u -> qpoisar1(u, alpha, beta), cdf.(Normal(), x), dims=1)

# Print or plot the results
println("Generated Poisson AR(1) Data:")
println(flow_data)
