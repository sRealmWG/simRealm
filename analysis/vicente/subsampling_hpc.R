# Run line below to install island
# install.packages("island_0.2.7.tar.gz", repos = NULL, type="source")


# Loading packages --------------------------------------------------------

library(tidyverse)
library(island)
library(parallel) # Cargando el paquete que hace la paralelización


# Loading data ------------------------------------------------------------


# Neutral simulations -----------------------------------------------------

# Change paths as needed
load("neutral_time_series_v2_extraSubsamples.Rdata")

n_metadata <- read.csv("neutral_metadata_v2.csv")



index.list <-
expand.grid(1:length(unique(neutral_local_ts$parameter_id)),
            1:length(unique(neutral_local_ts$timeSeriesID)))

Cores <- 20


# No subsampling ----------------------------------------------------------

parall <- mclapply(1:nrow(index.list), mc.cores=Cores, mc.preschedule = F,function(fila){

   par <- index.list[fila, 1]
   m <- index.list[fila, 2]

   temp <- neutral_local_ts %>% filter(parameter_id == unique(neutral_local_ts$parameter_id)[par]) %>%
      filter(timeSeriesID == unique(neutral_local_ts$timeSeriesID)[m])
   ss100 <- data.frame()

   for (i in 1:length(temp$timestep)){
      ss100 <- rbind(ss100, temp[i,] %>% select(timestep, timeSeriesID, ss100)  %>% unnest(cols = ss100))
   }
   # if(nrow(ss100) == 0) next
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

save(parall, file = "ss100.RData")


# ss 75 -------------------------------------------------------------------


parall <- mclapply(1:nrow(index.list), mc.cores=Cores, mc.preschedule = F,function(fila){

   par <- index.list[fila, 1]
   m <- index.list[fila, 2]

      temp <- neutral_local_ts %>% filter(parameter_id == unique(neutral_local_ts$parameter_id)[par]) %>%
         filter(timeSeriesID == unique(neutral_local_ts$timeSeriesID)[m])
      ss75 <- data.frame()

      for (i in 1:length(temp$timestep)){
         ss75 <- rbind(ss75, temp[i,] %>% select(timestep, timeSeriesID, ss75)  %>% unnest(cols = ss75))
      }
      # if(nrow(ss100) == 0) next
      temp <- ss75 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
         pivot_wider(names_from = timestep, values_from = N, values_fill = 0)
      temp <- (temp > 0)/1.0

      columns <- ncol(temp)

      raw_ce <- regular_sampling_scheme(temp, 2:(columns))

      raw_T_c <- 1/(raw_ce[1] + raw_ce[4])

      raw_J_t_c <- 1 - (raw_ce[1] + raw_ce[4] * exp(-1))/(raw_ce[1] + 2*raw_ce[4] - raw_ce[4] * exp(-1))

      raw_h_s <- raw_T_c * log((2 * raw_ce[1] + 3 * raw_ce[4])/(raw_ce[1] + 2 * raw_ce[4]))

      raw_a_j <- 1 - (raw_ce[1] / (raw_ce[1] + 2 * raw_ce[4]))

      raw_affinity <- raw_a_j / raw_h_s

      rm(ss75)

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

save(parall, file = "ss75.RData")


# ss50 --------------------------------------------------------------------

parall <- mclapply(1:nrow(index.list), mc.cores=Cores, mc.preschedule = F,function(fila){

   par <- index.list[fila, 1]
   m <- index.list[fila, 2]

   temp <- neutral_local_ts %>% filter(parameter_id == unique(neutral_local_ts$parameter_id)[par]) %>%
      filter(timeSeriesID == unique(neutral_local_ts$timeSeriesID)[m])
   ss50 <- data.frame()

   for (i in 1:length(temp$timestep)){
      ss50 <- rbind(ss50, temp[i,] %>% select(timestep, timeSeriesID, ss50)  %>% unnest(cols = ss50))
   }
   # if(nrow(ss100) == 0) next
   temp <- ss50 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
      pivot_wider(names_from = timestep, values_from = N, values_fill = 0)
   temp <- (temp > 0)/1.0

   columns <- ncol(temp)

   raw_ce <- regular_sampling_scheme(temp, 2:(columns))

   raw_T_c <- 1/(raw_ce[1] + raw_ce[4])

   raw_J_t_c <- 1 - (raw_ce[1] + raw_ce[4] * exp(-1))/(raw_ce[1] + 2*raw_ce[4] - raw_ce[4] * exp(-1))

   raw_h_s <- raw_T_c * log((2 * raw_ce[1] + 3 * raw_ce[4])/(raw_ce[1] + 2 * raw_ce[4]))

   raw_a_j <- 1 - (raw_ce[1] / (raw_ce[1] + 2 * raw_ce[4]))

   raw_affinity <- raw_a_j / raw_h_s

   rm(ss50)

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

save(parall, file = "ss50.RData")


# ss25 --------------------------------------------------------------------


parall <- mclapply(1:nrow(index.list), mc.cores=Cores, mc.preschedule = F,function(fila){

   par <- index.list[fila, 1]
   m <- index.list[fila, 2]

   temp <- neutral_local_ts %>% filter(parameter_id == unique(neutral_local_ts$parameter_id)[par]) %>%
      filter(timeSeriesID == unique(neutral_local_ts$timeSeriesID)[m])
   # ss100 <- data.frame()
   ss25 <- data.frame()
   # ss10 <- data.frame()
   for (i in 1:length(temp$timestep)){
      ss25 <- rbind(ss25, temp[i,] %>% select(timestep, timeSeriesID, ss25)  %>% unnest(cols = ss25))
   }
   # if(nrow(ss100) == 0) next
   temp <- ss25 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
      pivot_wider(names_from = timestep, values_from = N, values_fill = 0)
   temp <- (temp > 0)/1.0

   columns <- ncol(temp)

   raw_ce <- regular_sampling_scheme(temp, 2:(columns))

   raw_T_c <- 1/(raw_ce[1] + raw_ce[4])

   raw_J_t_c <- 1 - (raw_ce[1] + raw_ce[4] * exp(-1))/(raw_ce[1] + 2*raw_ce[4] - raw_ce[4] * exp(-1))

   raw_h_s <- raw_T_c * log((2 * raw_ce[1] + 3 * raw_ce[4])/(raw_ce[1] + 2 * raw_ce[4]))

   raw_a_j <- 1 - (raw_ce[1] / (raw_ce[1] + 2 * raw_ce[4]))

   raw_affinity <- raw_a_j / raw_h_s

   rm(ss25)

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

save(parall, file = "ss25.RData")


# ss10 --------------------------------------------------------------------


parall <- mclapply(1:nrow(index.list), mc.cores=Cores, mc.preschedule = F,function(fila){

   par <- index.list[fila, 1]
   m <- index.list[fila, 2]

   temp <- neutral_local_ts %>% filter(parameter_id == unique(neutral_local_ts$parameter_id)[par]) %>%
      filter(timeSeriesID == unique(neutral_local_ts$timeSeriesID)[m])
   # ss100 <- data.frame()
   ss10 <- data.frame()
   # ss10 <- data.frame()
   for (i in 1:length(temp$timestep)){
      ss10 <- rbind(ss10, temp[i,] %>% select(timestep, timeSeriesID, ss10)  %>% unnest(cols = ss10))
   }
   # if(nrow(ss100) == 0) next
   temp <- ss10 %>% select(-timeSeriesID) %>% arrange(timestep) %>%
      pivot_wider(names_from = timestep, values_from = N, values_fill = 0)
   temp <- (temp > 0)/1.0

   columns <- ncol(temp)

   raw_ce <- regular_sampling_scheme(temp, 2:(columns))

   raw_T_c <- 1/(raw_ce[1] + raw_ce[4])

   raw_J_t_c <- 1 - (raw_ce[1] + raw_ce[4] * exp(-1))/(raw_ce[1] + 2*raw_ce[4] - raw_ce[4] * exp(-1))

   raw_h_s <- raw_T_c * log((2 * raw_ce[1] + 3 * raw_ce[4])/(raw_ce[1] + 2 * raw_ce[4]))

   raw_a_j <- 1 - (raw_ce[1] / (raw_ce[1] + 2 * raw_ce[4]))

   raw_affinity <- raw_a_j / raw_h_s

   rm(ss10)

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

save(parall, file = "ss10.RData")
