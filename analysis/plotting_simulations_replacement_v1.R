# Plotting replacement process overtime

## Parameter values ----
### Community
source("./analysis/parameters/community_v1.r")

### Iterations
nrep <- 100L

### seed
seed <- 42L

# Running many steps for 1 scenario ----
simulation_ID <- sRealm::create_random_ID()
grDevices::pdf(onefile = TRUE, file =  paste0("./figures/communities/", simulation_ID, "_replacement.pdf"))

## Initialising ----
set.seed(seed)
i <- 1
comm <- mobsim::sim_thomas_community(
   s_pool = parameter_table$S_POOL[i],
   n_sim = parameter_table$N_SIM[i],
   sad_type = "lnorm", sad_coef = parameter_table$SAD_COEF[i],
   fix_s_sim = TRUE,
   mother_points = 1L, sigma = parameter_table$SIGMA[i]
)
plot(comm, main = paste0("p_id=", i, ", s=", parameter_table$S_POOL[i],", n=", parameter_table$N_SIM[i], ", SAD_COEF=", parameter_table$SAD_COEF[i], ", sigma=", parameter_table$SIGMA[i]))
graphics::rect(
   xleft = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH / 2,
   ybottom = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH / 2,
   xright = mean(c(0, 1)) + parameter_table$QUADRAT_WIDTH / 2,
   ytop = mean(c(0, 1)) + parameter_table$QUADRAT_WIDTH / 2,
   lwd = 2
)

## Iterating ----
for (j in 1L:nrep) {
   comm <- sRealm::replace_individuals(comm, R = 0.1)
   plot(comm, main = paste0("param_id=", i, ", s=", parameter_table$S_POOL[i],", s=", length(unique(comm$census$species)), ", n=", parameter_table$N_SIM[i], ", SAD_COEF=", parameter_table$SAD_COEF[i], ", sigma=", parameter_table$SIGMA[i]))
   graphics::rect(
      xleft = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH / 2,
      ybottom = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH / 2,
      xright = mean(c(0, 1)) + parameter_table$QUADRAT_WIDTH / 2,
      ytop = mean(c(0, 1)) + parameter_table$QUADRAT_WIDTH / 2,
      lwd = 2
   )
   plotrix::draw.circle(x = comm$census$x[1L], y = comm$census$y[1L], radius = 0.1, lwd = 2)
}
dev.off()

## Simulation metadata ----
metadata <- data.table::as.data.table(parameter_table)
metadata[, parameter_id := seq_len(nrow(metadata))]
metadata[, unique_id := simulation_ID]
metadata[, date := Sys.time()]
metadata[, seed := seed]
metadata[, simulation_function := "replace_individuals()"]
metadata[, sRealm_version := as.character(utils::packageVersion("sRealm"))]
metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "sRealm_version", "mobsim_version", "date", "seed"))

## Saving metadata ----
data.table::fwrite(metadata, file = paste0("./figures/communities/", simulation_ID, "_replacement_metadata.csv"))


