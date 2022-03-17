# Plotting communities

## Parameter values ----
### Community
source("./analysis/parameters/community_v2.r")

### Iterations
nrep <- 30L

### seed
seed <- 42L

# Running many steps for 1 scenario ----
for (param_i in c(1, 25)) {

   simulation_ID <- sRealmTools::create_random_ID()

   png_path <- file.path(tempdir(), paste0(simulation_ID,"_frame%03d.png"))
   png(png_path)
   par(ask = FALSE)

   ## Preparing the sampling quadrat
   quadrat55cornerCoordinates <- mobsim::sampling_grids(
      n_quadrats = parameter_table$N_QUADRATS[param_i], quadrat_size = sqrt(parameter_table$QUADRAT_AREA[param_i]),
      xmin = 0, xmax = 1, ymin = 0, ymax = 1,
      x0 = 0, y0 = 0,
      delta_x = sqrt(parameter_table$QUADRAT_AREA[param_i]), delta_y = sqrt(parameter_table$QUADRAT_AREA[param_i])
   )[55L, ]


   ## Initialising ----
   set.seed(seed)
   comm <- mobsim::sim_thomas_community(
      s_pool = parameter_table$S_POOL[param_i],
      n_sim = parameter_table$N_SIM[param_i],
      sad_type = "lnorm", sad_coef = parameter_table$SAD_COEF[param_i],
      fix_s_sim = TRUE,
      mother_points = 1L, sigma = parameter_table$SIGMA[param_i]
   )
   title_string <- paste0("p_id=", param_i, ", s=", parameter_table$S_POOL[param_i],", n=", parameter_table$N_SIM[param_i], ", SAD_COEF=", parameter_table$SAD_COEF[param_i], ", sigma=", parameter_table$SIGMA[param_i], ", movementSD=", parameter_table$MOVEMENT_SD[param_i])
   plot(comm, main = "1", sub = title_string)
   rect(
      xleft = quadrat55cornerCoordinates$x, xright = quadrat55cornerCoordinates$x + sqrt(parameter_table$QUADRAT_AREA[param_i]),
      ybottom = quadrat55cornerCoordinates$y, ytop = quadrat55cornerCoordinates$y + sqrt(parameter_table$QUADRAT_AREA[param_i]),
      lwd = 2
   )


   ## Iterating ----
   for (i in 2L:nrep) {
      comm <- sRealmTools::jitter_species(comm = comm, sd = parameter_table$MOVEMENT_SD[param_i])
      comm <- sRealmTools::torusify(comm)
      title_string <- paste0("p_id=", param_i, ", s=", parameter_table$S_POOL[param_i],", n=", parameter_table$N_SIM[param_i], ", SAD_COEF=", parameter_table$SAD_COEF[param_i], ", sigma=", parameter_table$SIGMA[param_i], ", movementSD=", parameter_table$MOVEMENT_SD[param_i])
      plot(comm, main = i, sub = title_string)
      rect(
         xleft = quadrat55cornerCoordinates$x, xright = quadrat55cornerCoordinates$x + sqrt(parameter_table$QUADRAT_AREA[param_i]),
         ybottom = quadrat55cornerCoordinates$y, ytop = quadrat55cornerCoordinates$y + sqrt(parameter_table$QUADRAT_AREA[param_i]),
         lwd = 2
      )
   }

   dev.off()
   png_files <- sprintf(png_path, 1:nrep)
   gif_file <- paste0("./figures/communities/", simulation_ID, "_jittering.gif")
   gifski::gifski(png_files, gif_file, delay = 0.4, loop = TRUE, width = 2000L, height = 2000L)
   unlink(png_files)
   utils::browseURL(gif_file)


   ## Simulation metadata ----
   metadata <- data.table::as.data.table(parameter_table[1, ])
   metadata[, parameter_id := seq_len(nrow(metadata))]
   metadata[, unique_id := simulation_ID]
   metadata[, simulation_function := "plotting_simulations_jitter_v5"]
   metadata[, date := Sys.time()]
   metadata[, seed := seed]
   metadata[, sRealmTools_version := as.character(utils::packageVersion("sRealmTools"))]
   metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

   data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "sRealmTools_version", "mobsim_version", "date", "seed"))

   ## Saving metadata ----
   data.table::fwrite(metadata, file = paste0("./figures/communities/", simulation_ID, "_jittering_metadata.csv"))

}
