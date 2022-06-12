simulation_v7_jitter_future <- function(strategy = "multisession", nyears = 5L, seed, simulation_ID) {
   library("future")
   if (strategy == "multisession") {
      future::plan("multisession")
   } else {
      future::plan(strategy = future.batchtools::batchtools_slurm,
                   template = ".batchtools.slurm.tmpl",
                   label     = "sim_jitter_6",
                   resources = list(
                      job.name = "sim_jitter_6",
                      walltime = 14,
                      memory = "5G",
                      ncpus  = 24,
                      output = "/work/%u/%j-%x.log",
                      email  = "alban.sagouis@idiv.de"
                   )
      )
   }


   ### Community
   source("./analysis/parameters/community_v3.r")
   # parameter_table <- parameter_table[1:2,]

   beginning <- Sys.time()
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

      ## Iterating
      res_param_i <- vector(mode = 'list', length = nyears)
      for (timestep in 1:nyears) {
         comm <- sRealmTools::jitter_species(comm = comm, sd = parameter_table$MOVEMENT_SD[param_i], drift = parameter_table$DRIFT[param_i])
         comm <- sRealmTools::torusify(comm)
         res_param_i[[timestep]] <- mobsim::sample_quadrats(comm = comm, n_quadrats = parameter_table$N_QUADRATS[param_i], quadrat_area = parameter_table$QUADRAT_AREA[param_i], method = "grid", plot = FALSE)$spec_dat
      }
      res[[param_i]] <- res_param_i
   } %seed% seed
   Sys.time() - beginning
   res <- as.list(res)
   Sys.time() - beginning
   saveRDS(object = res, file = paste0("./data/simulations/mobsim/temporary_files/", simulation_ID, "_sim_jitter_7.rds"))
   return(res)
}
