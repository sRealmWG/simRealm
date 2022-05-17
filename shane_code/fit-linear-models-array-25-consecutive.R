# fit linear models to distances measures of turnover (array job on eve)
library(tidyverse)


# get array job task id (use as parameter_id [PID])
PID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

# load one parameter combination and calculate metrics
load(paste0('/data/idiv_chase/simRealm/results/mobsim/mobsim-v2-beta-dist-25-pid-', PID, '.Rdata'))

# fit linear models to turnover (dissimilarity)
consecutive_25 <- beta_dist_25 %>% 
  filter((YEAR2-YEAR1) == 1) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(cYear = YEAR2 - mean(YEAR2)) %>% 
  select(parameter_id, timeSeriesID, YEAR2, cYear, Jbeta, MH_dist) %>% 
  nest(data = c(cYear, YEAR2, Jbeta, MH_dist)) %>% 
  mutate(Jac_lm = map(data, ~lm(.x$Jbeta ~ .x$cYear)),
         mean_Jac = map(data, ~mean(.x$Jbeta)),
         mh_lm = map(data, ~lm(.x$MH_dist ~ .x$cYear)),
         mean_mh = map(data, ~mean(.x$MH_dist)),
         Jac_glm = map(data, possibly(~betareg::betareg(.x$Jbeta ~ .x$cYear), otherwise = NULL)),
         mh_glm = map(data, possibly(~betareg::betareg(.x$MH_dist ~ .x$cYear), otherwise = NULL))) %>% 
  mutate(Jac_lm_tidy = map(Jac_lm, broom::tidy),
         MH_lm_tidy = map(mh_lm, broom::tidy),
         Jac_glm_tidy = map(Jac_glm, possibly(broom::tidy, otherwise = NULL)),
         MH_glm_tidy = map(mh_glm, possibly(broom::tidy, otherwise = NULL))) %>% 
  ungroup()



save(consecutive_25,
     file = Sys.getenv('OFILE'))
