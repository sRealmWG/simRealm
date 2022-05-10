# Run line below to install island
# install.packages("analysis/vicente/island_0.2.7.tar.gz", repos = NULL, type="source")


# Loading packages --------------------------------------------------------

library(tidyverse)
library(island)
library(future)


# Loading data ------------------------------------------------------------

load("data/time_series/mobsim_v2_timeSeries_site55_pid-1-24.Rdata")

metadata <- read.csv("data/simulations/mobsim/CXYAB2252T_steps_metadata.csv")

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
      if(nrow(ss100) == 0) next
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

save(tst, file = "analysis/vicente/estimates_mobsim_v2.RData")

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


# Sanity check ------------------------------------------------------------

# I can't make sense of the results. Maybe there's something wrong with my data
# processing. In any case, I have implemented this sanity check to see what
# happens to specific parameter_id and timeseries.

par_id <- 20
timeseries <- "ts208"

temp <- site55 %>% filter(parameter_id == par_id) %>%
   filter(timeSeriesID == timeseries)
ss100 <- data.frame()
for (i in 1:length(temp$timestep)){
   ss100 <- rbind(ss100, temp[i,] %>% select(timestep, timeSeriesID, ss100)  %>% unnest(cols = ss100))
}
temp <- ss100 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
   pivot_wider(names_from = timestep, values_from = N, values_fill = 0)

# We find very few colonizations and extinctions, and almost no recolonizations.
# Let's see if c-e estimates are saved correctly.

temp <- (temp > 0)/1.0
columns <- ncol(temp)
raw_ce <- regular_sampling_scheme(temp, 2:(columns))
raw_T_c <- 1/(raw_ce[1] + raw_ce[4]) # Yes, it works.


# Neutral simulations -----------------------------------------------------

load("data/time_series/neutral_time_series_v2.Rdata")

n_metadata <- read.csv("data/simulations/neutral_metadata_v2.csv")

plan("multisession")

outputfuture <- listenv::listenv()
fila <- 1
rm(n)
for(par in 1:length(unique(neutral_local_ts$parameter_id))){
   for(m in 1:length(unique(neutral_local_ts$timeSeriesID))){
      # for(par in 1:2){
      #    for(m in 1:2){

      temp <- neutral_local_ts %>% filter(parameter_id == unique(neutral_local_ts$parameter_id)[par]) %>%
         filter(timeSeriesID == unique(neutral_local_ts$timeSeriesID)[m])
      ss100 <- data.frame()
      # ss50 <- data.frame()
      # ss10 <- data.frame()
      for (i in 1:length(temp$timestep)){
         ss100 <- rbind(ss100, temp[i,] %>% select(timestep, timeSeriesID, ss100)  %>% unnest(cols = ss100))
      }
      if(nrow(ss100) == 0) next
      temp <- ss100 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
         pivot_wider(names_from = timestep, values_from = n, values_fill = 0)
      temp <- (temp > 0)/1.0

      columns <- ncol(temp)

      raw_ce <- regular_sampling_scheme(temp, 2:(columns))

      raw_T_c <- 1/(raw_ce[1] + raw_ce[4])

      raw_J_t_c <- 1 - (raw_ce[1] + raw_ce[4] * exp(-1))/(raw_ce[1] + 2*raw_ce[4] - raw_ce[4] * exp(-1))

      raw_h_s <- raw_T_c * log((2 * raw_ce[1] + 3 * raw_ce[4])/(raw_ce[1] + 2 * raw_ce[4]))

      raw_a_j <- 1 - (raw_ce[1] / (raw_ce[1] + 2 * raw_ce[4]))

      raw_affinity <- raw_a_j / raw_h_s

      rm(ss100)

      # species <- n_metadata$THETA[which(n_metadata$parameter_id == par)]
      #
      #
      # if(nrow(temp) < 1) next
      # rows <- species - nrow(temp)
      # temp <- rbind(temp, matrix(0, rows, columns))
      # ce <- regular_sampling_scheme(temp, 2:(columns))
      #
      # T_c <- 1/(ce[1] + ce[4])
      #
      # h_s <- T_c * log((2 * ce[1] + 3 * ce[4])/(ce[1] + 2 * ce[4]))
      #
      # a_j <- 1 - (ce[1] / (ce[1] + 2 * ce[4]))
      #
      # affinity <- a_j / h_s
      # J_t_c <- 1 - (ce[1] + ce[4] * exp(-1))/(ce[1] + 2*ce[4] - ce[4] * exp(-1))

      outputfuture[[fila]] %<-% c(parameter_id = unique(neutral_local_ts$parameter_id)[par],
                                  timeSeriesID = unique(neutral_local_ts$timeSeriesID)[m],
                                  # T_c = unname(T_c),
                                  # h_t = unname(h_s),
                                  # a_j = unname(a_j),
                                  # Affinity = unname(affinity),
                                  # J_t_c = unname(J_t_c),
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
neutral <- data.table::rbindlist(outputfuture)

# save(neutral, file = "analysis/vicente/estimates_neutral_v2.RData")


# Sanity check ------------------------------------------------------------

par_id <- 21
timeseries <- "ts208"

temp <- neutral_local_ts %>% filter(parameter_id == par_id) %>%
   filter(timeSeriesID == timeseries)
ss100 <- data.frame()
# ss50 <- data.frame()
# ss10 <- data.frame()
for (i in 1:length(temp$timestep)){
   ss100 <- rbind(ss100, temp[i,] %>% select(timestep, timeSeriesID, ss100)  %>% unnest(cols = ss100))
}
if(nrow(ss100) == 0) next
temp <- ss100 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
   pivot_wider(names_from = timestep, values_from = n, values_fill = 0)

# Again, we find very few colonizations and extinctions, and almost no recolonizations.
# Let's see if c-e estimates are saved correctly.


temp <- (temp > 0)/1.0
columns <- ncol(temp)
raw_ce <- regular_sampling_scheme(temp, 2:(columns))
raw_T_c <- 1/(raw_ce[1] + raw_ce[4]) # It works.

