# fit hierarchical linear models to distances measures of turnover (eve)
library(tidyverse)
rm(list=ls())

setwd('/data/idiv_chase/simRealm/results/neutral/metric-ts')
filelist = dir(pattern=paste0("neutral-v2-beta-100-"))
dat = tibble()
for(i in 1:length(filelist)){
  load(filelist[i])
  dat = bind_rows(dat, neutral_beta_dist_100)
}


# fit linear models to turnover (dissimilarity)
dat <- dat %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  select(parameter_id, timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist) %>% 
  group_by(parameter_id) %>% 
  nest(data = c(timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist))

allYrs_100_mixed <- dat %>% 
  mutate(Jac_hlm = map(data, ~lme4::lmer(Jbeta ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         MH_hlm = map(data, ~lme4::lmer(MH_dist ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         Jac_mean = map(data, ~mean(.x$Jbeta)),
         MH_mean = map(data, ~mean(.x$MH_dist))) %>% 
  mutate(Jac_hlm_tidy = map(Jac_hlm, broom.mixed::tidy),
         MH_hlm_tidy = map(MH_hlm, broom.mixed::tidy)) %>% 
  ungroup()

save(allYrs_100_mixed,
     file = '/data/idiv_chase/simRealm/results/neutral/model_fits/allYrs_100_mixed.Rdata')


# repeat for subsampled data
rm(list=ls())
filelist = dir(pattern=paste0("neutral-v2-beta-75-"))
dat = tibble()
for(i in 1:length(filelist)){
  load(filelist[i])
  dat = bind_rows(dat, neutral_beta_dist_75)
}

# fit linear models to turnover (dissimilarity)
dat <- dat %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  select(parameter_id, timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist) %>% 
  group_by(parameter_id) %>% 
  nest(data = c(timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist))

allYrs_75_mixed <- dat %>% 
  mutate(Jac_hlm = map(data, ~lme4::lmer(Jbeta ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         MH_hlm = map(data, ~lme4::lmer(MH_dist ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         Jac_mean = map(data, ~mean(.x$Jbeta)),
         MH_mean = map(data, ~mean(.x$MH_dist))) %>% 
  mutate(Jac_hlm_tidy = map(Jac_hlm, broom.mixed::tidy),
         MH_hlm_tidy = map(MH_hlm, broom.mixed::tidy)) %>% 
  ungroup()

save(allYrs_75_mixed,
     file = '/data/idiv_chase/simRealm/results/neutral/model_fits/allYrs_75_mixed.Rdata')

# 50% of the individuals
rm(list=ls())
filelist = dir(pattern=paste0("neutral-v2-beta-50-"))
dat = tibble()
for(i in 1:length(filelist)){
  load(filelist[i])
  dat = bind_rows(dat, neutral_beta_dist_50)
}


# fit linear models to turnover (dissimilarity)
dat <- dat %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  select(parameter_id, timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist) %>% 
  group_by(parameter_id) %>% 
  nest(data = c(timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist))

allYrs_50_mixed <- dat %>% 
  mutate(Jac_hlm = map(data, ~lme4::lmer(Jbeta ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         MH_hlm = map(data, ~lme4::lmer(MH_dist ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         Jac_mean = map(data, ~mean(.x$Jbeta)),
         MH_mean = map(data, ~mean(.x$MH_dist))) %>% 
  mutate(Jac_hlm_tidy = map(Jac_hlm, broom.mixed::tidy),
         MH_hlm_tidy = map(MH_hlm, broom.mixed::tidy)) %>% 
  ungroup()

save(allYrs_50_mixed,
     file = '/data/idiv_chase/simRealm/results/neutral/model_fits/allYrs_50_mixed.Rdata')

# 25% of the individuals
rm(list=ls())
filelist = dir(pattern=paste0("neutral-v2-beta-25-"))
dat = tibble()
for(i in 1:length(filelist)){
  load(filelist[i])
  dat = bind_rows(dat, neutral_beta_dist_25)
}

# fit linear models to turnover (dissimilarity)
dat <- dat %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  select(parameter_id, timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist) %>% 
  group_by(parameter_id) %>% 
  nest(data = c(timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist))

allYrs_25_mixed <- dat %>% 
  mutate(Jac_hlm = map(data, ~lme4::lmer(Jbeta ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         MH_hlm = map(data, ~lme4::lmer(MH_dist ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         Jac_mean = map(data, ~mean(.x$Jbeta)),
         MH_mean = map(data, ~mean(.x$MH_dist))) %>% 
  mutate(Jac_hlm_tidy = map(Jac_hlm, broom.mixed::tidy),
         MH_hlm_tidy = map(MH_hlm, broom.mixed::tidy)) %>% 
  ungroup()

save(allYrs_25_mixed,
     file = '/data/idiv_chase/simRealm/results/neutral/model_fits/allYrs_25_mixed.Rdata')

# 10% of the individuals
rm(list=ls())
filelist = dir(pattern=paste0("neutral-v2-beta-10-"))
dat = tibble()
for(i in 1:length(filelist)){
  load(filelist[i])
  dat = bind_rows(dat, neutral_beta_dist_10)
}

# fit linear models to turnover (dissimilarity)
dat <- dat %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  select(parameter_id, timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist) %>% 
  group_by(parameter_id) %>% 
  nest(data = c(timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist))

allYrs_10_mixed <- dat %>% 
  mutate(Jac_hlm = map(data, ~lme4::lmer(Jbeta ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         MH_hlm = map(data, ~lme4::lmer(MH_dist ~ c_temp_dist + (c_temp_dist | timeSeriesID), data = .)),
         Jac_mean = map(data, ~mean(.x$Jbeta)),
         MH_mean = map(data, ~mean(.x$MH_dist))) %>% 
  mutate(Jac_hlm_tidy = map(Jac_hlm, broom.mixed::tidy),
         MH_hlm_tidy = map(MH_hlm, broom.mixed::tidy)) %>% 
  ungroup()

save(allYrs_10_mixed,
     file = '/data/idiv_chase/simRealm/results/neutral/model_fits/allYrs_10_mixed.Rdata')

# for mean ~ f(duration)
rm(list=ls())
filelist = dir(pattern=paste0("neutral-v2-beta-100-"))
dat = tibble()
for(i in 1:length(filelist)){
  load(filelist[i])
  dat = bind_rows(dat, neutral_beta_dist_100)
}

dat_alt <- dat %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  select(parameter_id, timeSeriesID, temp_dist, c_temp_dist, Jbeta, MH_dist) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(temp_dist, c_temp_dist, Jbeta, MH_dist))

allYrs_100_mean_duration <- dat_alt %>% 
  mutate(Jac_mean = map(data, ~mean(.x$Jbeta)),
         MH_mean = map(data, ~mean(.x$MH_dist))) 
  ungroup()

save(allYrs_100_mean_duration,
     file = '/data/idiv_chase/simRealm/results/neutral/model_fits/allYrs_100_mean_duration.Rdata')
