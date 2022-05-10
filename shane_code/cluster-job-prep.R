# separate (unnest) time series for more efficient cluster computing

library(tidyverse)


load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/neutral_time_series.Rdata')
load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/neutral_time_series_v2.Rdata')

# separate each of the subsamples and nest all time steps for a given timeSeriesID
neutral_ss100 <- neutral_local_ts %>% 
  unnest(ss100) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, n) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, n)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)

neutral_ss50 <- neutral_local_ts %>% 
  unnest(ss50) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)

for(i in 1:nrow(neutral_ss50)){
  temp = neutral_ss50 %>% 
    slice(i) 
  
  file_name = paste0('parameter_id-', temp$parameter_id, '-', temp$timeSeriesID, '.csv')
  dir = '~/Dropbox/1current/sRealm/local_data/neutral-ts-v2-50/'
  
  temp %>% 
    unnest(cols = data) %>% 
    write_csv(., file = paste0(dir,file_name))
  
}

# prepare mobsim data for calculating whittaker on cluster
load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/mobsim_timeSeries_site55_pid-1-24.Rdata')

# separate each of the subsamples and nest all time steps for a given timeSeriesID
ss100_s55 <- site55 %>% 
  unnest(ss100) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)


for(i in 1:nrow(ss100_s55)){
  temp = ss100_s55 %>% 
    slice(i) 
  
  file_name = paste0('parameter_id-', temp$parameter_id, '-', temp$timeSeriesID, '.csv')
  dir = '~/Dropbox/1current/sRealm/local_data/mobsim-ts/'
  
  temp %>% 
    unnest(cols = data) %>% 
    write_csv(., file = paste0(dir,file_name))
  
}