# Run line below to install island
# install.packages("analysis/vicente/island_0.2.7.tar.gz", repos = NULL, type="source")


# Loading packages --------------------------------------------------------

library(tidyverse)
library(island)
library(future)


# Loading data ------------------------------------------------------------

load("data/time_series/timeSeries_site55_pid-1-24.Rdata")

metadata <- read.csv("data/simulations/mobsim/IFYAH3130E_jitter_metadata.csv")

# Running analyses --------------------------------------------------------
# It takes 4-5 hours in my cranky old laptop.
plan("multisession")

outputfuture <- listenv::listenv()
fila <- 1
for(n in 1:length(unique(site55$parameter_id))){
   for(m in 1:length(unique(site55$timeSeriesID))){
      # for(n in 1:2){
      #    for(m in 1:2){

      temp <- site55 %>% filter(parameter_id == unique(site55$parameter_id)[n]) %>%
         filter(timeSeriesID == unique(site55$timeSeriesID)[m])
      ss100 <- data.frame()
      # ss50 <- data.frame()
      # ss10 <- data.frame()
      for (i in 1:length(temp$timestep)){
         ss100 <- rbind(ss100, temp[i,] %>% select(timestep, timeSeriesID, ss100)  %>% unnest(cols = ss100))
      }
      temp <- ss100 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
         pivot_wider(names_from = timestep, values_from = N, values_fill = 0)
      temp <- (temp > 0)/1.0

      columns <- ncol(temp)

      raw_ce <- regular_sampling_scheme(temp, 2:(columns))

      raw_T_c <- 1/(raw_ce[1] + raw_ce[4])

      raw_J_t_c <- 1 - (raw_ce[1] + raw_ce[4] * exp(-1))/(raw_ce[1] + 2*raw_ce[4] - raw_ce[4] * exp(-1))

      raw_h_s <- raw_T_c * log((2 * raw_ce[1] + 3 * raw_ce[4])/(raw_ce[1] + 2 * raw_ce[4]))

      raw_a_j <- 1 - (raw_ce[1] / (raw_ce[1] + 2 * raw_ce[4]))

      raw_affinity <- raw_a_j / raw_h_s

      rm(ss100)

      species <- metadata$S_POOL[which(metadata$parameter_id == n)]


      if(nrow(temp) < 1) next
      rows <- species - nrow(temp)
      temp <- rbind(temp, matrix(0, rows, columns))
      ce <- regular_sampling_scheme(temp, 2:(columns))

      T_c <- 1/(ce[1] + ce[4])

      h_s <- T_c * log((2 * ce[1] + 3 * ce[4])/(ce[1] + 2 * ce[4]))

      a_j <- 1 - (ce[1] / (ce[1] + 2 * ce[4]))

      affinity <- a_j / h_s
      J_t_c <- 1 - (ce[1] + ce[4] * exp(-1))/(ce[1] + 2*ce[4] - ce[4] * exp(-1))

      outputfuture[[fila]] %<-% c(parameter_id = unique(site55$parameter_id)[n],
                          timeSeriesID = unique(site55$timeSeriesID)[m],
                          T_c = unname(T_c),
                          h_t = unname(h_s),
                          a_j = unname(a_j),
                          Affinity = unname(affinity),
                          J_t_c = unname(J_t_c),
                          raw_J_t_c = unname(raw_J_t_c),
                          raw_T_c = unname(raw_T_c),
                          raw_h_t = unname(raw_h_s),
                          raw_a_j = unname(raw_a_j),
                          raw_Affinity = unname(raw_affinity),
                          Duration = columns - 1)
      rm(temp)
      fila <- fila + 1
   }
}
outputfuture <- as.list(outputfuture)
tst <- data.table::rbindlist(outputfuture)


# Save output -------------------------------------------------------------

save(tst, file = "analysis/vicente/future_good_solutions.RData")

head(tst)


# I explain the output here.

# T_c --- Characteristic time --- a measure on how fast communities change.
# h_t --- halving time --- the time it takes the community to reach half the asymptotic turnover.
# a_j --- Asymptotic turnover --- measures how big should be the turnover when samples are too far apart in time that they are independent.
# Affinity --- asymptotic turnover/halving time --- akin to Helmut's approach.
# J_t_c --- Jaccard at the characteristic time --- measures change after 1 T_c

# raw_* --- When raw is indicated before the measure, it means that I did not
# take into account the number of species in the pool to make the estimations.
# That is, when raw_ is not present, I completed the species by time matrix to
# have as many species as the simulation had, adding 0 for the absent species.
# When raw is present, the observed species pool increases with each timestep
# and I only consider observed species.

# Some plots --------------------------------------------------------

ggplot(tst, aes(x = Duration, y = raw_T_c)) + geom_point() +
   facet_wrap(~parameter_id, scales = "free_y") +
   ggtitle("Characteristic time") +
   geom_smooth()

ggplot(tst, aes(x = Duration, y = raw_h_t)) + geom_point() +
   facet_wrap(~parameter_id, scales = "free_y") +
   ggtitle("Half-saturation time") +
   geom_smooth()

ggplot(tst, aes(x = Duration, y = J_t_c)) + geom_point() +
   facet_wrap(~parameter_id, scales = "free_y") +
   ggtitle("Jaccard at the T_c") +
   geom_smooth()

ggplot(tst, aes(x = Duration, y = raw_a_j)) + geom_point() +
   facet_wrap(~parameter_id, scales = "free_y") +
   ggtitle("Assymptotic Jaccard") +
   geom_smooth()

ggplot(tst, aes(x = Duration, y = a_j)) + geom_point() +
   facet_wrap(~parameter_id, scales = "free_y") +
   ggtitle("Assymptotic Jaccard") +
   geom_smooth()

ggplot(tst, aes(x = Duration, y = raw_Affinity)) + geom_point() +
   facet_wrap(~parameter_id, scales = "free_y") + ggtitle("ETIB affinity")


tst_w_metadata <- inner_join(metadata, tst)

# tst_w_metadata <- tst_w_metadata %>% mutate(facet = paste0("SP: ", S_POOL, " N: ", N_SIM,
#                                          " SAD: ", SAD_COEF, " SIGMA : ", SIGMA))

tst_w_metadata <- tst_w_metadata %>% mutate(facet = paste0(S_POOL, "-", N_SIM,
                                                           "-", SAD_COEF, "-", SIGMA))


ggplot(tst_w_metadata, aes(x = Duration, y = raw_T_c)) + geom_point() +
   facet_wrap(~facet, scales = "free_y") +
   ggtitle("Characteristic time - raw") +
   geom_smooth()

ggplot(tst_w_metadata, aes(x = Duration, y = T_c)) + geom_point() +
   facet_wrap(~facet, scales = "free_y") +
   ggtitle("Characteristic time - complete species pool") +
   geom_smooth()

ggplot(tst_w_metadata, aes(x = Duration, y = raw_a_j)) + geom_point() +
   facet_wrap(~facet, scales = "free_y") +
   ggtitle("Asymptotic Jaccard - raw") +
   geom_smooth()

ggplot(tst_w_metadata, aes(x = Duration, y = a_j)) + geom_point() +
   facet_wrap(~facet, scales = "free_y") +
   ggtitle("Asymptotic Jaccard - complete species pool") +
   geom_smooth()

ggplot(tst_w_metadata, aes(x = Duration, y = a_j - raw_a_j)) + geom_point() +
   facet_wrap(~facet, scales = "free_y") +
   ggtitle("Asymptotic Jaccard - complete minus raw") +
   geom_smooth()

ggplot(tst_w_metadata, aes(x = Duration, y = T_c - raw_T_c)) + geom_point() +
   facet_wrap(~facet, scales = "free_y") +
   ggtitle("Characteristic time - complete minus raw") +
   geom_smooth()
