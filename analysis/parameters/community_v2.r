# Parameters
## SAD
S_POOL   <- c(20L, 200L)
N_SIM    <- c(2000, 20000L)
SAD_COEF <- c(list(cv_abund = 0.1), list(cv_abund = 0.5), list(cv_abund = 1))

## initial dispersion
SIGMA <- c(0.05, 10)

## movement dispersion
MOVEMENT_SD <- c(0.01, 0.1)

## sampling
QUADRAT_AREA <- 0.01 # in a grid of 100
N_QUADRATS <- 100L

## subsampling
# SUBSAMPLING_RATIO <- c(.1, .5, 1)

parameter_table <- expand.grid(
   S_POOL = S_POOL, N_SIM = N_SIM, SAD_COEF = SAD_COEF,
   SIGMA = SIGMA,
   MOVEMENT_SD = MOVEMENT_SD,
   QUADRAT_AREA = QUADRAT_AREA, N_QUADRATS = N_QUADRATS
)
