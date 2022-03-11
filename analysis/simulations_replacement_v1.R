# reproduction model

## Parameter values ----
### Community
source("./analysis/parameters/community_v1.r")
# parameter_table <- parameter_table[1:2,]
### Iterations
nrep <- 200L

### seed
seed <- 42L


# Running the simulations ----

## setting the parallel cluster
cl <- parallel::makeCluster(7L)
parallel::clusterExport(cl = cl, varlist = c("parameter_table", "seed", "nrep"))

## sending the simulations
beginning <- Sys.time()
res <- parallel::parLapply(
   cl = cl,
   X = seq_len(nrow(parameter_table)),
   fun = function(i) {

      ## Initialising
      set.seed(seed)
      sad <- mobsim::sim_sad(
         s_pool = parameter_table$S_POOL[i],
         n_sim = parameter_table$N_SIM[i],
         sad_type = "lnorm", sad_coef = parameter_table$SAD_COEF[i],
         fix_s_sim = TRUE, drop_zeros = TRUE
      )
      comm <- mobsim::sim_thomas_coords(
         abund_vec = sad, mother_points = 1L,
         sigma = parameter_table$SIGMA[i]
      )
      x0 <- y0 <- mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH[i] / 2
      xsize <- ysize <-  parameter_table$QUADRAT_WIDTH[i]

      ## Iterating
      lapply(
         seq_len(nrep),
         function(iteration) {
            comm <<- sRealm::replace_individuals(comm, R = 0.1)
            samp <- mobsim::abund_rect(comm = comm, x0 = x0, y0 = y0, xsize = xsize, ysize = ysize)
            return(samp)
         }
      )
   }
)
Sys.time() - beginning
alarm()
parallel::stopCluster(cl = cl)

# Wrangling the results ----
dt <- data.table::rbindlist(
   idcol = TRUE,
   l = lapply(
      res,
      function(table_parameter_id) data.table::rbindlist(
         idcol = TRUE,
         l = lapply(table_parameter_id, data.table::as.data.table)
      )[N != 0L]
   )
)

# rm(res)
data.table::setnames(dt, 1:3, c("parameter_id", "iteration", "species"))
dt[, species := as.factor(species)]
# data.table::setorder(x = dt, parameter_id, iteration, species)
data.table::setcolorder(dt, neworder = c("parameter_id", "iteration"))

# Simulation metadata ----
simulation_ID <- sRealm::create_random_ID()
metadata <- data.table::as.data.table(parameter_table)
metadata[, parameter_id := seq_len(nrow(metadata))]
metadata[, unique_id := simulation_ID]
metadata[, date := Sys.time()]
metadata[, seed := seed]
metadata[, simulation_function := "simulations_replace_individuals_v1"]
metadata[, sRealm_version := as.character(utils::packageVersion("sRealm"))]
metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

data.table::setcolorder(metadata, neworder = c("unique_id", "parameter_id", "simulation_function", "sRealm_version", "mobsim_version", "date", "seed"))

# Saving results and metadata ----
data.table::fwrite(dt, file = paste0("./data/simulations/", simulation_ID, "_sim.csv"))
data.table::fwrite(metadata, file = paste0("./data/simulations/", simulation_ID, "_metadata.csv"))

# empty samples?
if (any(dt[, length(unique(iteration)) != nrep, by = parameter_id])) warning("Some samples were empty and do not appear in the results.")

