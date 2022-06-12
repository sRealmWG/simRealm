# Parameters
THETA <- c(10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L) # Fundamental Biodiversity Number. May et al. 2015 PRSB fit thetas of 25 to 90 to BCI data, depending on which pattern was fit. Tittensor & Worm 2016 GEB use 10, citing Hubbell. These values span a range.
M <- c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9) # migration rate (probability of new individual coming from regional pool)
N <- c(100L, 200L, 300L, 500L, 1000L, 2000L, 3000L, 5000L) # number of individuals in local community
NSAMPS <- 600L # Number of sampled timesteps
STEPL <- 0.01 # Sampling frequency multiplier (multiplies by N). Scaled so that, on average, 1% of the individuals die between every sample. Make sure this times N is an integer.
SEED <- 41L # Seed (must be different than previous parameter sets)
NBURN = 100L # Burnin length multiplier (multiplies by N)



parameter_table <- rbind(
   data.table(THETA = THETA, M = 0.2, N = 1000, NSAMPS = NSAMPS, STEPL=STEPL, SEED=SEED, NBURN=NBURN),
   data.table(THETA = 40, M = M, N = 1000, NSAMPS = NSAMPS, STEPL=STEPL, SEED=SEED, NBURN=NBURN),
   data.table(THETA = 40, M = 0.2, N = N, NSAMPS = NSAMPS, STEPL=STEPL, SEED=SEED, NBURN=NBURN))
parameter_table$parameter_id <- seq_len(nrow(parameter_table))
