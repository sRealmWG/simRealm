# inspection of simulated data for sRealm workshop 1

library(tidyverse)

dat <- read_csv('~/Dropbox/1current/sRealm/local_data/IFYAH3130E_jitter_sim.csv')
meta1 <- read_csv('~/Dropbox/1current/sRealm/local_data/IFYAH3130E_jitter_metadata.csv')

load('~/Dropbox/1current/sRealm/simRealm/data/time_series/timeSeries_site55_pid-1-24.Rdata')
rm(site55)
# want to subsample time series with duration between 3-100 years
# duration = tibble(d = floor(rlnorm(n = 250, meanlog = 2.3, sdlog = 1))) %>% 
#   filter(d > 2 & d < 101)
# 
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
    ss50 = map2(.x = data, .y = 0.5, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss10 = map2(.x = data, .y = 0.1, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL))) %>% 
  rename(ss100 = data)

# save site55
save(duration, 
     site55,
     file = '~/Dropbox/1current/sRealm/local_data/timeSeries_site55_pid-1-24.Rdata')

# save all the time series (and the subsamples of site55 only)
save(dat_ts, 
     file = '~/Dropbox/1current/sRealm/local_data/all_timeSeries_subsample_pid-1-24.Rdata')
