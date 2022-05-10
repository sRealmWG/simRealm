# want to break up the jobs: computational reality bites

# parameter_id as grouping variable: 360
library(tidyverse)

load('mobsim_v2_timeSeries_site55.Rdata')

ss100_s55 <- site55 %>% 
  unnest(ss100) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)

pid = unique(ss100_s55$parameter_id)

for(i in pid){
  print(pid[i])
  # write a csv file for each parameter combination
  dat = ss100_s55 %>% 
    filter(parameter_id==i) %>% 
    unnest(data) 
  
  write_csv(dat, file = paste0('/data/idiv_chase/simRealm/data/mobsim_v2_100_PID-', pid[i], '.csv'))
}

ss75_s55 <- site55 %>% 
  unnest(ss75) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)

pid = unique(ss75_s55$parameter_id)

for(i in pid){
  print(pid[i])
  # write a csv file for each parameter combination
  dat = ss75_s55 %>% 
    filter(parameter_id==i) %>% 
    unnest(data) 
  
    write_csv(dat, file = paste0('/data/idiv_chase/simRealm/data/mobsim_v2_75_PID-', pid[i], '.csv'))
}

ss50_s55 <- site55 %>% 
  unnest(ss50) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)

pid = unique(ss50_s55$parameter_id)

for(i in pid){
  print(pid[i])
  # write a csv file for each parameter combination
  dat = ss50_s55 %>% 
    filter(parameter_id==i) %>% 
    unnest(data) 
  
  write_csv(dat, file = paste0('/data/idiv_chase/simRealm/data/mobsim_v2_50_PID-', pid[i], '.csv'))
}

ss25_s55 <- site55 %>% 
  unnest(ss25) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)

pid = unique(ss25_s55$parameter_id)

for(i in pid){
  print(pid[i])
  # write a csv file for each parameter combination
  dat = ss25_s55 %>% 
    filter(parameter_id==i) %>% 
    unnest(data) 
  
  write_csv(dat, file = paste0('/data/idiv_chase/simRealm/data/mobsim_v2_25_PID-', pid[i], '.csv'))
}