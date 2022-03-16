# create time series from neutral simulations

library(tidyverse)

neutral_neutral_dat <- read_csv('~/Dropbox/1current/sRealm/simRealm/neutral_data/simulations/QEAYJ1252R_neutral_sim.csv')

# want to subsample time series with duration between 3-100 years
# use same distribution of durations created for mobsim calculations
load('~/Dropbox/1current/sRealm/local_data/timeSeries_site55_pid1-24.Rdata')
rm(site55)
# duration = tibble(d = floor(rlnorm(n = 250, meanlog = 2.3, sdlog = 1))) %>% 
#   filter(d > 2 & d < 101)

hist(duration$d)


# get time series of duration d from the complete neutral_data set
neutral_dat_nest <- neutral_dat %>% 
  group_by(parameter_id, timestep) %>% 
  nest(neutral_data = c(species, N)) %>% 
  ungroup()

# get ~200 time series with duration d from each parameter_id
neutral_dat_ts = tibble()
for(i in 1:nrow(duration)){
  print(paste(i, ' of ', nrow(duration), ' time series'))
  ts_id = neutral_dat_nest %>% 
    group_by(parameter_id, quadrat_id) %>% 
    slice_min(order_by = timestep,
              n = duration$d[i]) %>% 
    ungroup() %>% 
    mutate(timeSeriesID = paste0('ts', i)) 
  
  
  neutral_dat_ts = bind_rows(neutral_dat_ts, ts_id)
}

# also want to subsample from the local samples (incomplete samples of local assemblages)
expand_subsample <- function(neutral_data, fraction){
  # x is a two column matrix: species, value (id, abundance)
  # fraction describes proportion of sample retained (0.1, 0.5, 1)
  x = rep(neutral_data$species, times = neutral_data$N)
  ss = sample(x, size = fraction*length(x), replace = FALSE)
  subsample = as_tibble(table(ss))
  names(subsample) = c('species', 'N')
  return(subsample)
}

# subsample one quadrat only
site55 = neutral_dat_ts %>% 
   filter(quadrat_id=='site55') %>%
  mutate(#ss100 = map2(.x = neutral_data, .y = 1, .f = ~expand_subsample(.x, .y)),
    ss50 = map2(.x = neutral_data, .y = 0.5, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL)),
    ss10 = map2(.x = neutral_data, .y = 0.1, .f = possibly(~expand_subsample(.x, .y), otherwise = NULL))) %>% 
  rename(ss100 = neutral_data)

# save site55
save(duration, 
     site55,
     file = '~/Dropbox/1current/sRealm/local_neutral_data/timeSeries_site55_pid-25-48.Rneutral_data')

# save all the time series (and the subsamples of site55 only)
save(neutral_dat_ts, 
     file = '~/Dropbox/1current/sRealm/local_neutral_data/all_timeSeries_subsample_pid-25-48.Rneutral_data')
