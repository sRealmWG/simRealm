library("future")
future::plan(future.batchtools::batchtools_slurm,
             label     = "sim_jitter_6",
             resources = list(
                job.name = "sim_jitter_6",
                walltime = 180,
                memory = "6G",
                ncpus  = 36,
                output = "/work/%u/%j-%x.log",
                email  = "sagouis@pm.me"
             )
)

### Community
source("./analysis/parameters/community_v2.r")

### Iterations
nrep <- 600L

### seed
seed <- 42L

res <- listenv::listenv()
for (param_i in 1:nrow(parameter_table))  res[[param_i]] %<-% {
   ## Initialising
   sad <- mobsim::sim_sad(
      s_pool = parameter_table$S_POOL[param_i],
      n_sim = parameter_table$N_SIM[param_i],
      sad_type = "lnorm", sad_coef = parameter_table$SAD_COEF[param_i],
      fix_s_sim = TRUE, drop_zeros = TRUE
   )
   comm <- mobsim::sim_thomas_coords(
      abund_vec = sad, mother_points = 1L,
      sigma = parameter_table$SIGMA[param_i]
   )
   x0 <- y0 <- mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH[param_i] / 2
   xsize <- ysize <-  parameter_table$QUADRAT_WIDTH[param_i]

   ## Iterating
   res_param_i <- vector(mode = 'list', length = nrep)
   for (timestep in 1:nrep) {
      comm <- sRealmTools::jitter_species(comm = comm, sd = 0.01)
      comm <- sRealmTools::torusify(comm)
      res_param_i[[timestep]] <- mobsim::abund_rect(comm = comm, x0 = x0, y0 = y0, xsize = xsize, ysize = ysize)
   }
   res[[param_i]] <- res_param_i
} %seed% seed
res <- as.list(res)
saveRDS(object = res, file = "~/simRealm/data/simulations/sim_jitter_6.rds")
