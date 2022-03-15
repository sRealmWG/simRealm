# Parameters
THETA <- c(10L, 90L) # Fundamental Biodiversity Number. May et al. 2015 PRSB fit thetas of 25 to 90 to BCI data, depending on which pattern was fit. Tittensor & Worm 2016 GEB use 10, citing Hubbell. These values span a range.
M <- c(0.01, 0.1, 0.9) # migration rate (probability of new individual coming from regional pool)
N <- c(20L, 200L) # number of individuals in local community
NSAMPS <- 600L # Number of sampled timesteps
STEPL <- 0.5 # Sampling frequency multiplier (multiplies by N). Scaled so that, on average, half the individuals die between every sample
SEED <- 42L # Seed
NBURN = 100L # Burnin length multiplier (multiplies by N)



parameter_table <- expand.grid(THETA = THETA, M = M, N = N, NSAMPS = NSAMPS, STEPL=STEPL, SEED=SEED, NBURN=NBURN)
parameter_table$parameter_id <- seq_len(nrow(parameter_table))
