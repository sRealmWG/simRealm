library("future")
future::plan(future.batchtools::batchtools_slurm,
             template = ".batchtools.slurm.tmpl",
             label     = "sim_test_1",
             resources = list(
                job.name = "sim_test_1",
                walltime = 1,
                memory = "5G",
                ncpus  = 1,
                output   = "/work/%u/%j-%x.log",
                email  = "sagouis@pm.me"
             )
)

### Community
source("./analysis/parameters/community_v1.r")

comm <- mobsim::sim_thomas_community(100, 100000, fix_s_sim = TRUE)
comm <- sRealm::jitter_species(comm = comm, sd = 0.01)
comm <- sRealm::torusify(comm)

x0 <- y0 <- 0.45
xsize <- ysize <-  0.1

res <- mobsim::abund_rect(comm = comm, x0 = x0, y0 = y0, xsize = xsize, ysize = ysize)
saveRDS(object = res, file = "~/simRealm/data/simulations/sim_test_1.rds")
