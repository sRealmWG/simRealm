# Parameters
## SAD
S_POOL <- c(20L, 200L)
N_SIM <- c(2000L, 20000L)
SAD_COEF = c(list(cv_abund = 0.1), list(cv_abund = 0.5), list(cv_abund = 1))

## Dispersion
SIGMA <- c(0.05, 0.2, 0.8)

## sampling
QUADRAT_WIDTH <- sqrt(c(0.01, 0.1))

parameter_table <- expand.grid(S_POOL = S_POOL, N_SIM = N_SIM, SAD_COEF = SAD_COEF, SIGMA = SIGMA, QUADRAT_WIDTH = QUADRAT_WIDTH)
