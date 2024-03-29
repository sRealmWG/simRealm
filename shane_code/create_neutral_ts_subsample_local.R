# create time series from neutral simulations

library(tidyverse)

# v1 
# neutral_dat <- read_csv('~/Dropbox/1current/sRealm/simRealm/data/simulations/QEAYJ1252R_neutral_sim.csv')
# v2
# neutral_dat <- read_csv('~/Dropbox/1current/sRealm/simRealm/simRealm/data/simulations/neutral_sim_v2.csv')
# v3
neutral_dat <- read_csv('~/Dropbox/1current/sRealm/simRealm/simRealm/data/simulations/neutral_sim_v2.csv')

# want to subsample time series with duration between 3-100 years
# duration d (tibble)
load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/duration.Rdata')
# hist(duration$d)

# get time series of duration d from the complete neutral_data set
neutral_dat_nest <- neutral_dat %>% 
  group_by(parameter_id, timestep) %>% 
  nest(neutral_data = c(species, n)) %>% 
  ungroup()

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

# also want to subsample from the local samples (incomplete samples of local assemblages)
expand_subsample <- function(neutral_data, fraction){
  # x is a two column matrix: species, value (id, abundance)
  # fraction describes proportion of sample retained (0.1, 0.5, 1)
  x = rep(neutral_data$species, times = neutral_data$n)
  ss = sample(x, size = fraction*length(x), replace = FALSE)
  subsample = as_tibble(table(ss))
  names(subsample) = c('species', 'N')
  return(subsample)
}

# subsample one quadrat only
neutral_local_ts = neutral_dat_ts %>% 
  mutate(#ss100 = map2(.x = neutral_data, .y = 1, .f = ~expand_subsample(.x, .y)),
    ss75 = map2(.x = neutral_data, .y = 0.75, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss50 = map2(.x = neutral_data, .y = 0.5, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss25 = map2(.x = neutral_data, .y = 0.25, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss10 = map2(.x = neutral_data, .y = 0.1, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL))) %>% 
  rename(ss100 = neutral_data)

# save 
save(duration, 
     neutral_local_ts,
     file = '~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/neutral_time_series_v3.Rdata')

