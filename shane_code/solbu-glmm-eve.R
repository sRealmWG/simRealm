# fit Solbu et al glmm to neutral population dynamics on EVE

library(tidyverse)
library(glmmTMB)

setwd('/data/idiv_chase/simRealm/data/neutral-ts-v3-100/')
# setwd('~/Dropbox/1current/sRealm/local_data/neutral-ts-v3-100/')

# get array job task id (use as parameter_id [PID])
PID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
filelist = dir(pattern=paste0("parameter_id-", PID, "-"))

dat = tibble()
for(i in 1:length(filelist)){
  # first version of directed movement
  temp <- read_csv(paste0(filelist[i]), show_col_types = FALSE)
  
  dat = bind_rows(dat, temp)
}

# long to wide to put zeroes in
dat <- dat %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, n)) %>% 
  mutate(wide_dat = map(data, ~(pivot_wider(data = ., 
                                            names_from = species, 
                                            values_from = n,
                                            values_fill = 0))))

long_dat <- dat %>% 
  mutate(data = map(wide_dat, ~pivot_longer(data = .,
                                            cols = -timestep,
                                            names_to = 'species',
                                            values_to = 'N'))) %>% 
  unnest(cols = c(data)) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(time = timestep - min(timestep) + 1,
         time_y = numFactor(time),
         location = 'site1') %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, species, N, time, time_y, location) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(time, time_y, location, species, N)) %>% 
  ungroup()


long_dat <- long_dat %>% 
  mutate(model = map(data, possibly(~glmmTMB(data = .,
                                             formula = N ~ 1 + time_y + 
                                               ou(time_y + 0 | species) +
                                               (1 | species),
                                             family = poisson(link = 'log')),
                                    otherwise = NULL)))


# extract parameters and tidy up
# a function to do the wrangle
get_pars <- function(model){
  pars <- (model$fit$par)
  tibble(par_name = names(pars)) %>% 
    mutate(par_name = paste0(par_name, '_', 1:n()),
           values = pars) %>% 
    filter(par_name=='beta_1' | str_detect(par_name, "^theta")) %>% 
    mutate(var_name = c("mu", "lse", "lgamma", "lsh")) %>% 
    select(-par_name) %>% 
    pivot_wider(names_from = var_name,
                values_from = values) %>% 
    mutate(
      # Within-species variation
      se2 = exp(lse) ^ 2,
      # Strength of density regulation
      gamma = exp(lgamma),
      # Among-species variation
      sh2 = exp(lsh) ^ 2,
      # Ratio of environmental variation in relative log abundance
      rse2 = se2 / (se2 + sh2),
      # Species-specific response to environmental variation
      ss2 = se2 * gamma * 2,
      # Variation in growth rate among species
      sr2 = sh2 * gamma ^ 2)
}

# function to check convergence ()
convergence_check <- function(model){
  model$sdr$pdHess
}

long_dat <- long_dat %>% 
  select(parameter_id, timeSeriesID, data, model) %>% 
  mutate(parameters = map(model, possibly(~get_pars(.), otherwise = NULL)),
         time = map(data, ~unique(.x$time)),
         convergence = map(model, ~convergence_check(.)))

# calculate correlation temporal decay in relative abundance
temp_cor_relN <- function(data){
  tibble(time = data$time,
         cor = (data$se2 * exp(-data$gamma * time) + data$sh2) / 
           (data$se2 + data$sh2))
}


results <- long_dat %>% 
  select(parameter_id, timeSeriesID, parameters, time) %>% 
  unnest(c(parameters, time)) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(time = seq(0, max(time) - min(time), by = 1)) %>% 
  nest(data = c(mu, lse, lgamma, lsh, se2, gamma, sh2, rse2, ss2, sr2, time)) %>% 
  mutate(temp_cor_rla = map(data, ~temp_cor_relN(.))) %>% 
  left_join(long_dat %>% 
              select(parameter_id, timeSeriesID, convergence))

setwd('/data/idiv_chase/simRealm/results/neutral/glmm-dyn-sad/')
save(results, long_dat,
     file = Sys.getenv('OFILE'))

