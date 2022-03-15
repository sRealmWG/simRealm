# Plotting communities

## Parameter values ----
### Community
source("./analysis/parameters/community_v1.r")

### Iterations
nrep <- 100L

### seed
seed <- 42L

# Running many steps for 1 scenario ----
simulation_ID <- sRealmTools::create_random_ID(1L)
grDevices::pdf(onefile = TRUE, file =  paste0("./figures/communities/", simulation_ID, "_jittering.pdf"))

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
plot(comm)

## Iterating ----
for (i in 1L:nrep) {
   comm <- sRealmTools::jitter_species(comm = comm, sd = 0.01)
   comm <- sRealmTools::torusify(comm)
   plot(comm)
}
dev.off()

## Simulation metadata ----
metadata <- data.table::as.data.table(parameter_table)
metadata[, parameter_id := seq_len(nrow(metadata))]
metadata[, unique_id := simulation_ID]
metadata[, date := Sys.time()]
metadata[, seed := seed]
metadata[, sRealmTools_version := as.character(utils::packageVersion("sRealmTools"))]
metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "sRealmTools_version", "mobsim_version", "date", "seed"))

## Saving metadata ----
data.table::fwrite(metadata, file = paste0("./figures/communities/", simulation_ID, "jittering_metadata.csv"))




# Running the first step for all scenarios ----
simulation_ID <- sRealmTools::create_random_ID(1L)
set.seed(seed)
grDevices::pdf(onefile = TRUE, file = paste0("./figures/communities/", simulation_ID, "_initials.pdf"))

for (i in seq_len(nrow(parameter_table))) {
   sad <- mobsim::sim_sad(s_pool = parameter_table$S_POOL[i], n_sim = parameter_table$N_SIM[i], sad_type = "lnorm", sad_coef = parameter_table$SAD_COEF[i], fix_s_sim = TRUE, drop_zeros = TRUE)
   comm <- mobsim::sim_thomas_coords(abund_vec = sad, mother_points = 1L, sigma = parameter_table$SIGMA[i])
   plot(comm, main = paste0("param_id=", i, ", s=", parameter_table$S_POOL[i],", n=", parameter_table$N_SIM[i], ", SAD_COEF=", parameter_table$SAD_COEF[i], ", sigma=", parameter_table$SIGMA[i]))
   graphics::rect(
      xleft = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH / 2,
      ybottom = mean(c(0, 1)) - parameter_table$QUADRAT_WIDTH / 2,
      xright = mean(c(0, 1)) + parameter_table$QUADRAT_WIDTH / 2,
      ytop = mean(c(0, 1)) + parameter_table$QUADRAT_WIDTH / 2,
      lwd = 2
   )
}
dev.off()

## Simulation metadata ----
metadata <- data.table::as.data.table(parameter_table)
metadata[, parameter_id := seq_len(nrow(metadata))]
metadata[, unique_id := simulation_ID]
metadata[, date := Sys.time()]
metadata[, seed := seed]
metadata[, sRealmTools_version := as.character(utils::packageVersion("sRealmTools"))]
metadata[, mobsim_version := as.character(utils::packageVersion("mobsim"))]

data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "sRealm_version", "mobsim_version", "date", "seed"))

# Saving metadata ----
data.table::fwrite(metadata, file = paste0("./figures/communities/", simulation_ID, "_initials_metadata.csv"))

