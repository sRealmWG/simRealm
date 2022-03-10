replacement_v2 <- function(i, parameter_table, seed) {

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
