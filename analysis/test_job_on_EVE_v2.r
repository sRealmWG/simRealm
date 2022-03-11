library("future")
future::plan(future.batchtools::batchtools_slurm,
             template = ".batchtools.slurm.tmpl",
             label     = "sim_test_2",
             resources = list(
                job.name = "sim_test_2",
                walltime = 1,
                memory = "2G",
                ncpus  = 4,
                output   = "/work/%u/%j-%x.log",
                email  = "alban.sagouis@idiv.de"
             )
)

### Community
source("./analysis/parameters/community_v1.r")
res <- listenv::listenv()
for (param_i in 1:4)  res[[param_i]] %<-% {

   comm <- mobsim::sim_thomas_community(100, 100000, fix_s_sim = TRUE)
   comm <- sRealm::jitter_species(comm = comm, sd = 0.01)
   comm <- sRealm::torusify(comm)

   x0 <- y0 <- 0.45
   xsize <- ysize <-  0.1

   mobsim::abund_rect(comm = comm, x0 = x0, y0 = y0, xsize = xsize, ysize = ysize)
}
res <- as.list(res)
saveRDS(object = res, file = "~/simRealm/data/simulations/sim_test_2.rds")
