# inspection of simulated data for sRealm workshop 1

library(tidyverse)

# # v1: random walk movement
# dat <- read_csv('~/Dropbox/1current/sRealm/local_data/IFYAH3130E_jitter_sim.csv')
# meta1 <- read_csv('~/Dropbox/1current/sRealm/local_data/IFYAH3130E_jitter_metadata.csv')

# directed movement (non-random)
# dat <- read_csv('~/Dropbox/simRealm/data/simulations/CXYAB2252T_steps_sim.csv')
dat <- read_csv('/data/idiv_chase/simRealm/2022-05-04_steps_sim.csv')

# meta1 <- read_csv('~/Dropbox/1current/sRealm/simRealm/simRealm/data/simulations/mobsim/CXYAB2252T_steps_metadata.csv')
meta1 <- read_csv('/data/idiv_chase/simRealm/2022-05-04_steps_metadata.csv')

# duration d (tibble)
# load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/duration.Rdata')
load('/data/idiv_chase/simRealm/duration.Rdata')
# hist(duration$d)


# get time series of duration d from the complete data set
dat_nest <- dat %>% 
  group_by(parameter_id, quadrat_id, timestep) %>% 
  nest(data = c(species, N)) %>% 
  ungroup()

# dat_nest <- dat_nest %>% 
#   rename(timestep1 = quadrat_id,
#          quadrat_id = timestep) %>% 
#   rename(timestep = timestep1)

# get ~200 time series with duration d from each parameter_id
dat_ts = tibble()
for(i in 1:nrow(duration)){
  
  print(paste(i, ' of ', nrow(duration), ' time series'))
  
  start_point <- floor(runif(1, min = 1, max = 450))
  
  ts_id = dat_nest %>% 
    group_by(parameter_id, quadrat_id) %>% 
    filter(timestep %in% start_point:(start_point+duration$d[i])) %>% 
    ungroup() %>% 
    mutate(timeSeriesID = paste0('ts', i))
  
  dat_ts = bind_rows(dat_ts, ts_id)
}

# also want to subsample from the local samples (incomplete samples of local assemblages)
expand_subsample <- function(data, fraction){
  # x is a two column matrix: species, value (id, abundance)
  # fraction describes proportion of sample retained (0.1, 0.5, 1)
  x = rep(data$species, times = data$N)
  ss = sample(x, size = fraction*length(x), replace = FALSE)
  subsample = as_tibble(table(ss))
  names(subsample) = c('species', 'N')
  return(subsample)
}

# subsample one quadrat only
site55 = dat_ts %>% 
   filter(quadrat_id=='site55') %>%
  mutate(#ss100 = map2(.x = data, .y = 1, .f = ~expand_subsample(.x, .y)),
    ss75 = map2(.x = data, .y = 0.75, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss50 = map2(.x = data, .y = 0.5, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss25 = map2(.x = data, .y = 0.25, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss10 = map2(.x = data, .y = 0.1, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL))) %>% 
  rename(ss100 = data)

# save site55
save(site55,
     file = '~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/mobsim_v2_timeSeries_site55_pid-1-24.Rdata')

save(site55,
     file = '/data/idiv_chase/simRealm/mobsim_v2_timeSeries_site55.Rdata')

# save all the time series (and the subsamples of site55 only)
save(dat_ts, 
     file = '~/Dropbox/1current/sRealm/local_data/all_timeSeries_subsample_pid-1-24.Rdata')
