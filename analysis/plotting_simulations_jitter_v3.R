# Plotting communities

## Parameter values ----
### Community
source("./analysis/parameters/community_v1.r")

### Iterations
nrep <- 30L

### seed
seed <- 42L

# Running many steps for 1 scenario ----
simulation_ID <- sRealm::create_random_ID(1L)

png_path <- file.path(tempdir(), "frame%03d.png")
png(png_path)
par(ask = FALSE)


## Initialising ----
set.seed(seed)
param_i <- 1
comm <- mobsim::sim_thomas_community(
   s_pool = parameter_table$S_POOL[param_i],
   n_sim = parameter_table$N_SIM[param_i],
   sad_type = "lnorm", sad_coef = parameter_table$SAD_COEF[param_i],
   fix_s_sim = TRUE,
   mother_points = 1L, sigma = parameter_table$SIGMA[param_i]
)
title_string <- paste0("p_id=", param_i, ", s=", parameter_table$S_POOL[param_i],", n=", parameter_table$N_SIM[param_i], ", SAD_COEF=", parameter_table$SAD_COEF[param_i], ", sigma=", parameter_table$SIGMA[param_i])
plot(comm, main = "1", sub = title_string)
graphics::rect(
   xleft = mean(c(0, 1)) - sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
   ybottom = mean(c(0, 1)) - sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
   xright = mean(c(0, 1)) + sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
   ytop = mean(c(0, 1)) + sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
   lwd = 2
)

## Iterating ----
for (i in 2L:nrep) {
   comm <- sRealm::jitter_species(comm = comm, sd = 0.01)
   comm <- sRealm::torusify(comm)
   title_string <- paste0("p_id=", param_i, ", s=", parameter_table$S_POOL[param_i],", n=", parameter_table$N_SIM[param_i], ", SAD_COEF=", parameter_table$SAD_COEF[param_i], ", sigma=", parameter_table$SIGMA[param_i])
   plot(comm, main = i, sub = title_string)
   graphics::rect(
      xleft = mean(c(0, 1)) - sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
      ybottom = mean(c(0, 1)) - sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
      xright = mean(c(0, 1)) + sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
      ytop = mean(c(0, 1)) + sqrt(parameter_table$QUADRAT_WIDTH[param_i]) / 2,
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
metadata[, simulation_function := "plotting_simulations_jitter_v3"]
metadata[, date := Sys.time()]
metadata[, seed := seed]
metadata[, sRealm_version := as.character(utils::packageVersion("sRealm"))]
metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "sRealm_version", "mobsim_version", "date", "seed"))

## Saving metadata ----
data.table::fwrite(metadata, file = paste0("./figures/communities/", simulation_ID, "_jittering_metadata.csv"))


