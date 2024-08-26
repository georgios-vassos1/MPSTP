using Test
using tsproj
using Statistics  # Import Statistics for mean and var functions

@testset "rmvnorm function" begin
    mean_vec = [0.0, 0.0]  # Renamed to avoid conflict with the `mean` function
    covar = [1.0 0.5; 0.5 1.0]
    samples = rmvnorm(1000, mean_vec, covar)

    @test size(samples) == (1000, 2)
    
    # Correct usage of `mean` and `var`
    sample_mean = mean(samples[:, 1])  # Use [] for indexing
    sample_var = var(samples[:, 1])    # Use [] for indexing
    
    @test isapprox(sample_mean, 0.0, atol=0.1)
    @test isapprox(sample_var, 1.0, atol=0.1)
end
