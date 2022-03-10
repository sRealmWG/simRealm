# Initial models

## Parameter values ----
### Community
source("./analysis/parameters/community_v1.r")

### Iterations
nrep <- 600L

### seed
seed <- 42L


# Running the simulations ----
## setting the parallel cluster
cl <- parallel::makeCluster(7L)
parallel::clusterExport(cl = cl, varlist = c("parameter_table", "seed", "nrep"))
beginning <- Sys.time()
## sending the simulations
res <- parallel::parLapply(cl = cl,
                           X = seq_len(nrow(parameter_table)),
                           fun = function(i) {
                              ## Initialising
                              set.seed(seed)
                              sad <- mobsim::sim_sad(s_pool = parameter_table$S_POOL[i], n_sim = parameter_table$N_SIM[i], sad_type = "lnorm", sad_coef = parameter_table$SAD_COEF[i], fix_s_sim = TRUE, drop_zeros = TRUE)
                              comm <- mobsim::sim_thomas_coords(abund_vec = sad, mother_points = 1L, sigma = parameter_table$SIGMA[i])
                              ## Iterating
                              sapply(
                                 seq_len(nrep),
                                 function(iteration) {
                                    comm <- sRealm::jitter_species(comm = comm, sd = 0.01)
                                    comm <- sRealm::torusify(comm)
                                    samp <- mobsim::abund_rect(comm = comm, x0 = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH[i] / 2, y0 = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH[i] / 2, xsize = parameter_table$QUADRAT_WIDTH[i], ysize = parameter_table$QUADRAT_WIDTH[i])
                                    return(samp)
                                 }
                              )
                           }
)
Sys.time() - beginning
alarm()
parallel::stopCluster(cl = cl)

# Wrangling the results ----
dt <- lapply(res, data.table::as.data.table, keep.rownames = TRUE)
# rm(res)
dt <- data.table::rbindlist(dt, idcol = TRUE)
dt[, rn := as.factor(rn)]
column_vector <- grep("V", colnames(dt), value = TRUE)
dt[, (column_vector) := lapply(.SD, function(column) replace(column, column == 0L, NA_integer_)), .SDcols = column_vector] # replace all 0

dt <- data.table::melt(dt, id.vars = c(".id", "rn"), na.rm = TRUE, variable.name = "iteration")
dt[, iteration := as.integer(gsub("V", "", iteration))]
data.table::setnames(dt, "rn", "species")
data.table::setnames(dt, ".id", "parameter_id")
data.table::setorder(x = dt, parameter_id, iteration, species)
data.table::setcolorder(dt, neworder = c("parameter_id","iteration"))

# Simulation metadata ----
simulation_ID <- sRealm::create_random_ID(1L, seed = seed) #
metadata <- data.table::as.data.table(parameter_table)
metadata[, parameter_id := seq_len(nrow(metadata))]
metadata[, unique_id := simulation_ID]
metadata[, date := Sys.time()]
metadata[, seed := seed]
metadata[, simulation_function := "simulations_v3"]
metadata[, sRealm_version := as.character(utils::packageVersion("sRealm"))]
metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "simulation_function", "sRealm_version", "mobsim_version", "date", "seed"))

# Saving results and metadata ----
data.table::fwrite(dt, file = paste0("./data/simulations/", simulation_ID, "_sim.csv"))
data.table::fwrite(metadata, file = paste0("./data/simulations/", simulation_ID, "_metadata.csv"))

# empty samples?
if (any(dt[, length(unique(iteration)) != nrep, by = parameter_id])) warning("Some samples were empty and do not appear in the results.")

