jittering_v1_lapply <- function(parameter_set, nrep, seed = NULL) {
   ## Initialising
   set.seed(seed)
   sad <- mobsim::sim_sad(
      s_pool = parameter_set$S_POOL,
      n_sim = parameter_set$N_SIM,
      sad_type = "lnorm", sad_coef = parameter_set$SAD_COEF,
      fix_s_sim = TRUE, drop_zeros = TRUE
   )
   comm <- mobsim::sim_thomas_coords(
      abund_vec = sad, mother_points = 1L,
      sigma = parameter_set$SIGMA
   )
   x0 <- y0 <- mean(c(0, 1)) - parameter_set$QUADRAT_WIDTH / 2
   xsize <- ysize <-  parameter_set$QUADRAT_WIDTH

   ## Iterating
   lapply(
      seq_len(nrep),
      function(iteration) {
         comm <- sRealmTools::jitter_species(comm = comm, sd = 0.01)
         comm <<- sRealmTools::torusify(comm)
         samp <- sRealmTools::abund_rect(comm = comm, x0 = x0, y0 = y0, xsize = xsize, ysize = ysize)
         return(samp)
      }
   )
}


jittering_v1_for <- function(parameter_set, nrep, seed = NULL) {
   ## Initialising
   set.seed(seed)
   sad <- mobsim::sim_sad(
      s_pool = parameter_set$S_POOL,
      n_sim = parameter_set$N_SIM,
      sad_type = "lnorm", sad_coef = parameter_set$SAD_COEF,
      fix_s_sim = TRUE, drop_zeros = TRUE
   )
   comm <- mobsim::sim_thomas_coords(
      abund_vec = sad, mother_points = 1L,
      sigma = parameter_set$SIGMA
   )
   x0 <- y0 <- mean(c(0, 1)) - parameter_set$QUADRAT_WIDTH / 2
   xsize <- ysize <-  parameter_set$QUADRAT_WIDTH
   res_param_i <- vector(mode = 'list', length = nrep)

   ## Iterating
   for (iteration in 1:nrep) {
      comm <- sRealmTools::jitter_species(comm = comm, sd = 0.01)
      comm <- sRealmTools::torusify(comm)
      res_param_i[[iteration]] <- mobsim::abund_rect(comm = comm, x0 = x0, y0 = y0, xsize = xsize, ysize = ysize)
   }
   return(res_param_i)
}



microbenchmark::microbenchmark(
   setup = {
      ### Community
      source("./analysis/parameters/community_v1.r")
      parameter_table[1:10,]

      ### Iterations
      nrep <- 5L

      ### seed
      seed <- 42L
   },
   times = 20L,
   "lapply" = res <- lapply(seq_len(nrow(parameter_table)), function(param_i) jittering_v1_lapply(parameter_set = parameter_table[param_i,], nrep = nrep, seed = seed)),

   "for" = {
      res <- vector(mode = 'list', length = nrow(parameter_table))
      for (param_i in 1:nrow(parameter_table))  res[[param_i]] <- jittering_v1_for(parameter_set = parameter_table[param_i,], nrep = nrep, seed = seed)
   },
   "future" = {
      library("future")
      plan(strategy = "sequential")
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
         for (iteration in 1:nrep) {

            comm <- sRealmTools::jitter_species(comm = comm, sd = 0.01)
            comm <- sRealmTools::torusify(comm)
            res_param_i[[iteration]] <-    mobsim::abund_rect(comm = comm, x0 = x0, y0 = y0, xsize = xsize, ysize = ysize)

         }
         res[[param_i]] <- res_param_i
      } %seed% seed
      res <- as.list(res)
   }
)

