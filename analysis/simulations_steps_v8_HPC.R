# Initial models

## Parameter values ----
### Community
source("./analysis/parameters/community_v4.r")

### Iterations
nyears <- 600L

### seed
seed <- 42L

# Running the simulations ----
# simulation_ID <- sRealmTools::create_random_ID()
simulation_ID <- Sys.Date()
source("./functions/simulations_v8_steps_future.R", local = FALSE)

beginning <- Sys.time()
res <- simulation_v8_steps_future(nyears = nyears, strategy = "batchtools_slurm", seed = seed, simulation_ID = simulation_ID)
Sys.time() - beginning

# Reading the simulations ----
# res <- readRDS(file = paste0("./data/simulations/mobsim/temporary_files/", simulation_ID, "_sim_jitter_7.rds"))

# Wrangling the results ----
beginning <- Sys.time()
dt <- lapply(res, function(param_i) data.table::rbindlist(
   lapply(param_i, data.table::as.data.table, keep.rownames = TRUE),
   idcol = TRUE
)
)

dt <- lapply(dt, function(param_i) data.table::melt(param_i,
                                                    id.vars = c(".id", "rn"),
                                                    variable.name = "species",
                                                    value.name = "N"
)
)
dt <- lapply(dt, function(dt_element) dt_element[N > 0L])
dt <- data.table::rbindlist(dt, idcol = TRUE)

dt[, rn := as.factor(rn)]
data.table::setnames(dt, 1L:3L, c("parameter_id", "timestep", "quadrat_id"))
data.table::setcolorder(dt, neworder = c("parameter_id", "quadrat_id", "timestep"))
data.table::setorder(dt, parameter_id, timestep, species)

# Simulation metadata ----
metadata <- data.table::as.data.table(parameter_table)
metadata[, parameter_id := seq_len(nrow(metadata))]
metadata[, unique_id := simulation_ID]
metadata[, date := Sys.time()]
metadata[, seed := seed]
metadata[, simulation_function := "simulations_v8_steps_future"]
metadata[, parameter_set := "community_v4"]
metadata[, sRealmTools_version := as.character(utils::packageVersion("sRealmTools"))]
metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

data.table::setcolorder(metadata, neworder = c("unique_id", "parameter_id", "simulation_function", "parameter_set", "sRealmTools_version", "mobsim_version", "date", "seed"))

# Saving results and metadata ----
data.table::fwrite(dt, file = paste0("./data/simulations/mobsim/", simulation_ID, "_steps_sim.csv"))
data.table::fwrite(metadata, file = paste0("./data/simulations/mobsim/", simulation_ID, "_steps_metadata.csv"))

Sys.time() - beginning

# empty samples?
# if (any(dt[, length(unique(timestep)) != nyears, by = parameter_id])) warning("Some samples were empty and do not appear in the results.")
