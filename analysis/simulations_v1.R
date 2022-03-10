# Initial models

## Parameter values ----
### SAD
S_POOL <- c(20L, 200L)
N_SIM <- c(2000L, 20000L)
SAD_COEF = c(list(cv_abund = 0.1), list(cv_abund = 0.5), list(cv_abund = 1))

### Dispersion
SIGMA <- c(0.05, 0.2, 0.8)

### sampling
QUADRAT_WIDTH <- sqrt(c(0.01, 0.1))

### Iterations
nrep <- 300L

### seed
seed <- 42L

parameter_table <- expand.grid(S_POOL = S_POOL, N_SIM = N_SIM, SAD_COEF = SAD_COEF, SIGMA = SIGMA, QUADRAT_WIDTH = QUADRAT_WIDTH)
# parameter_table <- parameter_table[1:2,]

## Simulation function ----
running_first_sim <- function(S_POOL, N_SIM, SAD_COEF,
                              SIGMA,
                              QUADRAT_WIDTH,
                              xrange = c(0, 1), yrange = c(0, 1)
) {

   sad <- mobsim::sim_sad(s_pool = S_POOL, n_sim = N_SIM, sad_type = "lnorm", sad_coef = SAD_COEF, fix_s_sim = TRUE, drop_zeros = TRUE)
   comm <- mobsim::sim_thomas_coords(abund_vec = sad, mother_points = 1L, sigma = SIGMA)
   comm <- sRealm::jitter_species(comm = comm, sd = 0.01)
   samp <- mobsim::abund_rect(comm = comm, x0 = mean(xrange) - QUADRAT_WIDTH / 2, y0 = mean(yrange) - QUADRAT_WIDTH / 2, xsize = QUADRAT_WIDTH, ysize = QUADRAT_WIDTH)

   return(samp)
}

# running_first_sim(
#    S_POOL = parameter_table$S_POOL[72], N_SIM = parameter_table$N_SIM[72], SAD_COEF = parameter_table$SAD_COEF[72],
#    SIGMA = parameter_table$SIGMA[72],
#    QUADRAT_WIDTH = parameter_table$QUADRAT_WIDTH[72]
# )

# res <- sapply(
#    seq_len(nrep), function(iteration) running_first_sim(
#       S_POOL = parameter_table$S_POOL[72], N_SIM = parameter_table$N_SIM[72], SAD_COEF = parameter_table$SAD_COEF[72],
#       SIGMA = parameter_table$SIGMA[72],
#       QUADRAT_WIDTH = parameter_table$QUADRAT_WIDTH[72]
#    )
# )

# Running the simulations
set.seed(seed)
res <- lapply(seq_len(nrow(parameter_table)),
              function(i) sapply(
                 seq_len(nrep),function(iteration) running_first_sim(
                    S_POOL = parameter_table$S_POOL[i], N_SIM = parameter_table$N_SIM[i], SAD_COEF = parameter_table$SAD_COEF[i],
                    SIGMA = parameter_table$SIGMA[i],
                    QUADRAT_WIDTH = parameter_table$QUADRAT_WIDTH[i]
                 )
              )
)

# Wrangling the results
dt <- lapply(res, data.table::as.data.table, keep.rownames = TRUE)
dt <- data.table::rbindlist(dt, idcol = TRUE)
dt[, rn := as.factor(rn)]
column_vector <- grep("V", colnames(dt), value = TRUE)
dt[, (column_vector) := lapply(.SD, function(column) replace(column, column == 0L, NA_integer_)), .SDcols = column_vector] # replace all 0

dt <- data.table::melt(dt, id.vars = c(".id", "rn"), na.rm = TRUE, variable.name = "iteration")
dt[, iteration := as.integer(gsub("V", "", iteration))]
data.table::setnames(dt, "rn", "species")
data.table::setnames(dt, ".id", "parameter_id")
data.table::setorder(x = dt, parameter_id, iteration, species)

# Simulation metadata
simulation_ID <- sRealm::create_random_ID(1L, seed = seed) #
metadata <- data.table::as.data.table(parameter_table)
metadata[, .id := seq_len(nrow(metadata))]
metadata[, unique_id := simulation_ID]
metadata[, date := Sys.time()]
metadata[, seed := seed]
data.table::setnames(metadata, ".id", "parameter_id")
data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "date", "seed"))

# Saving results and metadata
data.table::fwrite(dt, file = paste0("./data/simulations/", simulation_ID, "_sim.csv"))
data.table::fwrite(metadata, file = paste0("./data/simulations/", simulation_ID, "_metadata.csv"))

# empty samples?
if (any(dt[, length(unique(iteration)) != nrep, by = parameter_id])) warning("Some samples were empty and do not appear in the results.")

