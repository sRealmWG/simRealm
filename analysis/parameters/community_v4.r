# Parameters
## SAD
S_POOL   <- seq(from = 10L, to = 90L, by = 10L)
N_SIM    <- c(100L, 200L, 300L, 500L, 1000L, 2000L, 3000L, 5000L) * 100L
SAD_COEF <- c(list(cv_abund = 0.5))

## initial dispersion
SIGMA    <- 10

## movement dispersion
MOVEMENT_SD <- 0
DRIFT    <- seq(from = 0.001, to = 0.01, length = 5) # at max speed, the map comes back to where it was at the beginning in 100 timesteps

## sampling
QUADRAT_AREA <- 0.01 # in a grid of 100
N_QUADRATS <- 100L

## subsampling
# SUBSAMPLING_RATIO <- c(.1, .5, 1)

parameter_table <- expand.grid(
   S_POOL = S_POOL, N_SIM = N_SIM, SAD_COEF = SAD_COEF,
   SIGMA = SIGMA,
   MOVEMENT_SD = MOVEMENT_SD, DRIFT = DRIFT,
   QUADRAT_AREA = QUADRAT_AREA, N_QUADRATS = N_QUADRATS
)
