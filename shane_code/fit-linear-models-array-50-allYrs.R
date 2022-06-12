# fit linear models to distances measures of turnover (array job on eve)
library(tidyverse)


# get array job task id (use as parameter_id [PID])
PID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# load one parameter combination and calculate metrics
load(paste0('/data/idiv_chase/simRealm/results/mobsim/mobsim-v2-beta-dist-50-pid-', PID, '.Rdata'))

# fit linear models to turnover (dissimilarity)
allYrs_50 <- beta_dist_50 %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, c_temp_dist, temp_dist, Jbeta, MH_dist) %>% 
  nest(data = c(c_temp_dist, temp_dist, Jbeta, MH_dist)) %>% 
  mutate(Jac_lm_allYrs = map(data, ~lm(.x$Jbeta ~ .x$c_temp_dist)),
         mean_Jac = map(data, ~mean(.x$Jbeta)),
         mh_lm_allYrs = map(data, ~lm(.x$MH_dist ~ .x$c_temp_dist)),
         mean_mh = map(data, ~mean(.x$MH_dist)),
         Jac_glm_allYrs = map(data, possibly(~betareg::betareg(.x$Jbeta ~ .x$c_temp_dist), otherwise = NULL)),
         mh_glm_allYrs = map(data, possibly(~betareg::betareg(.x$MH_dist ~ .x$c_temp_dist), otherwise = NULL))) %>% 
  mutate(Jac_lm_allYrs_tidy = map(Jac_lm_allYrs, broom::tidy),
         MH_lm_allYrs_tidy = map(mh_lm_allYrs, broom::tidy),
         Jac_glm_allYrs_tidy = map(Jac_glm_allYrs, possibly(broom::tidy, otherwise = NULL)),
         MH_glm_allYrs_tidy = map(mh_glm_allYrs, possibly(broom::tidy, otherwise = NULL))) %>% 
  ungroup()


save(allYrs_50,
     file = Sys.getenv('OFILE'))

