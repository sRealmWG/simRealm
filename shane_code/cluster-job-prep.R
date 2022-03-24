# separate (unnest) time series for more efficient cluster computing

library(tidyverse)

load('/data/idiv_chase/sablowes/simRealm/data/neutral_time_series.Rdata')
# load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/neutral_time_series.Rdata')
# load('~/Dropbox/1current/sRealm/local_data/timeSeries_site55_pid-25-48.Rdata')

# separate each of the subsamples and nest all time steps for a given timeSeriesID
neutral_ss100 <- neutral_local_ts %>% 
  unnest(ss100) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, n) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, n)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)


for(i in 1:nrow(neutral_ss100)){
  temp = neutral_ss100 %>% 
    slice(i) 
  
  file_name = paste0('parameter_id-', temp$parameter_id, '-', temp$timeSeriesID, '.csv')
  dir = '~/Dropbox/1current/sRealm/local_data/neutral-ts/'
  
  temp %>% 
    unnest(cols = data) %>% 
    write_csv(., file = paste0(dir,file_name))
  
}