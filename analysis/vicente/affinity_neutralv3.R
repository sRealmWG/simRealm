# Run line below to install island
# install.packages("island_0.2.7.tar.gz", repos = NULL, type="source")


# Loading packages --------------------------------------------------------

library(tidyverse)
library(island)
library(parallel) # Cargando el paquete que hace la paralelización


# Loading data ------------------------------------------------------------


# Neutral simulations -----------------------------------------------------

# Change paths as needed
# neutral_v3 <- read.csv("data/simulations/neutral_sim_v3.csv")
neutral_v3 <- read.csv("neutral_sim_v3.csv") #hpc


# get time series of duration d from the complete neutral_data set
neutral_dat_nest <- neutral_v3 %>%
   group_by(parameter_id, timestep) %>%
   nest(neutral_data = c(species, n)) %>%
   ungroup()

load("duration.Rdata") #hpc

# get ~200 time series with duration d from each parameter_id
neutral_dat_ts = tibble()
for(i in 1:nrow(duration)){
   print(paste(i, ' of ', nrow(duration), ' time series'))
   # random starting point
   start_point <- floor(runif(1, min = 1, max = 500))

   ts_id = neutral_dat_nest %>%
      group_by(parameter_id) %>%
      filter(timestep %in% start_point:(start_point+duration$d[i])) %>%
      ungroup() %>%
      mutate(timeSeriesID = paste0('ts', i))


   neutral_dat_ts = bind_rows(neutral_dat_ts, ts_id)
}

# n_metadata <- read.csv("data/simulations/neutral_metadatav3.csv")



index.list <-
   expand.grid(1:length(unique(neutral_dat_ts$parameter_id)),
               1:length(unique(neutral_dat_ts$timeSeriesID)))

Cores <- detectCores()
Cores <- 20 #hpc


# No subsampling ----------------------------------------------------------

parall <- mclapply(1:nrow(index.list), mc.cores=Cores, mc.preschedule = F,function(fila){

   par <- index.list[fila, 1]
   m <- index.list[fila, 2]

   temp <- neutral_local_ts %>% filter(parameter_id == unique(neutral_dat_ts$parameter_id)[par]) %>%
      filter(timeSeriesID == unique(neutral_dat_ts$timeSeriesID)[m])
   ss100 <- data.frame()

   for (i in 1:length(temp$timestep)){
      ss100 <- rbind(ss100, temp[i,] %>% select(timestep, timeSeriesID, ss100)  %>% unnest(cols = ss100))
   }
   # if(nrow(ss100) == 0) next
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

   rm(temp)

   c(parameter_id = unique(neutral_local_ts$parameter_id)[par],
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

})

neutral_estimates_v3 <- data.table::rbindlist(parall)
save(neutral_estimates_v3, file = "analysis/vicente/ss100_v3.RData")


