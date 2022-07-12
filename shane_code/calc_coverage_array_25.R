# calculate whittaker measures of turnover for the neutral simulations
options(tidyverse.quiet = TRUE)
library(tidyverse)
# get array job task id (use as parameter_id [PID])
PID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

setwd('/data/idiv_chase/simRealm/data/neutral-ts-v3-25/')
filelist = dir(pattern=paste0("parameter_id-", PID, "-"))

dat = tibble()
for(i in 1:length(filelist)){
  # first version of directed movement
  temp <- read_csv(paste0(filelist[i]), show_col_types = FALSE)
  
  dat = bind_rows(dat, temp)
}

dat <- dat %>% 
  group_by(parameter_id, timeSeriesID, timestep) %>% 
  mutate(m = sum(N)) %>% 
  nest(data = c(species, N, m)) %>% 
  ungroup()

# calculate coverage
coverage_per_yr <- dat %>% 
  group_by(parameter_id, timeSeriesID, timestep) %>% 
  mutate(coverage = map(data, ~mobr::Chat(.x$N, .x$m))) %>% 
  ungroup()


# rename and save
setwd('/data/idiv_chase/simRealm/results/neutral/coverage')
save(coverage_per_yr,
     file = Sys.getenv('OFILE'))