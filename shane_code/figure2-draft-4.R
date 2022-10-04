# preliminary figure 2 showing affinity and linear model results (fit to neutral simulations)

library(tidyverse)
library(broom.mixed)

load('~/Dropbox/1current/sRealm/simRealm/simRealm/analysis/vicente/Affinity_subsamplings.RData')

neutral.res <- as_tibble(neutral.res)

neutral_meta <- read_csv('~/Dropbox/1current/sRealm/simRealm/simRealm/data/simulations/neutral_metadata_v2.csv')
# parameter combinations: clean for plotting
# M: migration rate
# N: local community size (J)
# THETA: fundamental biodiversity parameter - equivalent to species pool size
neutral_meta <- neutral_meta %>% 
  select(parameter_id, THETA, M, N) %>% 
  mutate(Spool = paste0('S=', THETA),
         Mlabel = paste0('M=', M),
         Nlocal = paste0('Nlocal=', N)) %>%
  mutate(label = paste0(Spool, ', ', Nlocal, ', ', Mlabel),
         SN_label = paste0(Spool, ', ', Nlocal))

neutral_meta$Nlocal = factor(neutral_meta$Nlocal, 
                             levels = c('Nlocal=100', 'Nlocal=200', 'Nlocal=300', 'Nlocal=500', 
                                        'Nlocal=1000', 'Nlocal=2000', 'Nlocal=3000', 'Nlocal=5000'))

load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/duration.Rdata')


load('~/Dropbox/1current/sRealm/local_data/neutral_v2_allYrs_100_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/neutral_v2_allYrs_75_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/neutral_v2_allYrs_50_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/neutral_v2_allYrs_25_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/neutral_v2_allYrs_10_mixed.Rdata')

load('~/Dropbox/1current/sRealm/local_data/neutral_v2_allYrs_100_mean_duration.Rdata')

load('~/Dropbox/1current/sRealm/local_data/consecutive_100_mean_duration.Rdata')
load('~/Dropbox/1current/sRealm/local_data/consecutive_100_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/consecutive_75_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/consecutive_50_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/consecutive_25_mixed.Rdata')
load('~/Dropbox/1current/sRealm/local_data/consecutive_10_mixed.Rdata')

# two colour theme 
linear_col = '#bc5090'
affinity_col = '#ffa600'
mean_col = '#003f5c'

# assemblage size (use completely sampled communities only)
pid = neutral_meta %>% 
  filter(THETA==40 & M==0.2) %>% 
  pull(parameter_id)

# want the timeseries-level slope estimates so as we see some 
# uncertainty around the linear model summary
lm_slopes = allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'c_temp_dist') %>% 
  rename(timeSeriesID = level)

lm_slopes_consec = consecutive_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'cYear') %>% 
  rename(timeSeriesID = level,
         consec_slope = estimate)

N_plot <-
neutral.res %>% 
  filter(parameter_id %in% pid & (Subsampling == 100)) %>% 
  select(parameter_id, timeSeriesID, N, raw_Affinity) %>% 
  left_join(lm_slopes %>%
              select(parameter_id, timeSeriesID, estimate)) %>%
  left_join(lm_slopes_consec %>%
              select(parameter_id, timeSeriesID, consec_slope)) %>%
  left_join(allYrs_100_mixed %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              select(parameter_id, Jac_mean)) %>%  
  left_join(consecutive_100_mixed %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              rename(consec_mean = Jac_mean) %>% 
              select(parameter_id, consec_mean)) %>% 
  ggplot() +
  # geom_point(aes(x = N, y = slope)) +
  stat_smooth(aes(x = N, y = estimate, colour = 'linear_col'),
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_point(aes(x = N, y = consec_slope)) + 
  stat_smooth(aes(x = N, y = consec_slope, colour = 'linear_col'),
              method = 'gam', linetype = 2,
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = N, y = raw_Affinity/1, colour = 'affinity_col'), # need to transform here so as y-axis scale is comparable
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = N, y = Jac_mean/10, colour = 'mean_col'),
                method = 'gam',
                formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = N, y = consec_mean/10, colour = 'mean_col'),
              linetype = 2,
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate',
                     sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                         name = 'Mean Jaccard')) +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col,
                                 'mean_col' = mean_col),
                      guide = 'none'
  ) +
  labs(x = 'Local assemblage size (individuals)') +
  theme_classic() +
  theme(axis.title.y = element_text(size=7),
        axis.title.y.right = element_text(size=7),
        axis.title.x = element_text(size = 7),
        axis.line.y.left = element_line(),
        axis.text.y.left = element_text(size = 6),
        axis.line.y.right = element_line(colour = mean_col),
        axis.text.y.right = element_text(colour = mean_col, size = 6),
        axis.text.x = element_text(size = 6)
  )


# Completeness of local sample (subsampled % of individuals)

pid = neutral_meta %>% 
  filter(THETA==40 & M==0.2 & N==1000) %>% 
  pull(parameter_id)


completeness_dat <- bind_rows(
  allYrs_100_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 100),
  allYrs_75_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 75),
  allYrs_50_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 50),
  allYrs_25_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 25),
  allYrs_10_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 10))

completeness_dat_consec <- bind_rows(
  consecutive_100_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 100),
  consecutive_75_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 75),
  consecutive_50_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 50),
  consecutive_25_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 25),
  consecutive_10_mixed %>% 
    filter(parameter_id %in% pid) %>% 
    mutate(completeness = 10))

lm_slopes = completeness_dat %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'c_temp_dist') %>% 
  rename(timeSeriesID = level)

lm_slopes_consec = completeness_dat_consec %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'cYear') %>% 
  rename(timeSeriesID = level,
         consec_slope = estimate)

# scales of the different measures are quite incompatible here (remove numbers?)
completeness_plot <-
  lm_slopes %>% 
  select(parameter_id, timeSeriesID, completeness, estimate) %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  select(parameter_id, timeSeriesID, completeness, slope, Nlocal) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid) %>% 
              select(parameter_id, timeSeriesID, N, raw_Affinity, Subsampling) %>% 
              rename(completeness = Subsampling) 
  ) %>% 
  left_join(lm_slopes_consec %>% 
              select(parameter_id, timeSeriesID, completeness, consec_slope)) %>%   
  left_join(completeness_dat %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              select(parameter_id, Jac_mean, completeness)) %>%  
  left_join(completeness_dat_consec %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              rename(Jac_mean_consec = Jac_mean) %>% 
              select(parameter_id, Jac_mean_consec, completeness)) %>%    
  ggplot() +
  # geom_point(aes(x = completeness, y = slope, colour = Nlocal)) +
  stat_smooth(aes(x = completeness, y = slope, colour = 'linear_col'),
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = completeness, y = consec_slope, colour = 'linear_col'),
              method = 'gam', linetype = 2,
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = completeness, y = raw_Affinity/1000, colour = 'affinity_col'),
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = completeness, y = Jac_mean/1000, colour = 'mean_col'),
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
    stat_smooth(aes(x = completeness, y = Jac_mean_consec/1000, colour = 'mean_col'),
                method = 'gam', linetype = 2,
                formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate',
                     sec.axis = sec_axis(trans = ~.*1000, # back transform so as numbers are correct on label
                                         name = 'Mean Jaccard')) +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col,
                                 'mean_col' = mean_col),
                      guide = 'none') +
  labs(x = 'Percentage of individuals sampled') +
  theme_classic() +
  theme(axis.title.y = element_text(size=7),
        axis.title.y.right = element_text(size=7),
        axis.title.x = element_text(size = 7),
        axis.line.y.left = element_line(),
        axis.text.y.left = element_text(size = 6),
        axis.line.y.right = element_line(colour = mean_col),
        axis.text.y.right = element_text(colour = mean_col, size = 6),
        axis.text.x = element_text(size = 6)
  )

# species pool size
pid = neutral_meta %>% 
  distinct(THETA, .keep_all = TRUE) %>% 
  pull(parameter_id)

lm_slopes = allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'c_temp_dist') %>% 
  rename(timeSeriesID = level)

lm_slopes_consec = consecutive_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'cYear') %>% 
  rename(timeSeriesID = level,
         consec_slope = estimate)

Spool_plot <-
  lm_slopes %>% 
  select(parameter_id, timeSeriesID, estimate) %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  select(parameter_id, timeSeriesID, THETA, slope) %>% 
  left_join(lm_slopes_consec %>% 
              select(parameter_id, timeSeriesID, consec_slope)) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid & Subsampling==100) %>% 
              select(parameter_id, timeSeriesID, N, THETA, raw_Affinity)
  ) %>% 
  left_join(allYrs_100_mixed %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              select(parameter_id, Jac_mean)) %>%  
  left_join(consecutive_100_mixed %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>%
              rename(Jac_mean_consec = Jac_mean) %>% 
              select(parameter_id, Jac_mean_consec)) %>%  
  ggplot() +
  # geom_point(aes(x = THETA, y = slope)) +
  stat_smooth(aes(x = THETA, y = slope, colour = 'linear_col'),
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = THETA, y = consec_slope, colour = 'linear_col'),
              method = 'gam', linetype = 2,
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_point(aes(x = THETA, y = raw_Affinity, colour = 'affinity_col')) +
  stat_smooth(aes(x = THETA, y = raw_Affinity, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = THETA, y = Jac_mean/10, colour = 'mean_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = THETA, y = Jac_mean_consec/10, colour = 'mean_col'),
              method = 'gam', linetype = 2,
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate',
                     sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                         name = 'Mean Jaccard')) +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col,
                                 'mean_col' = mean_col),
                      guide = 'none') +
  labs(x = 'Regional species pool size') +
  theme_classic() +
  theme(axis.title.y = element_text(size=7),
        axis.title.y.right = element_text(size=7),
        axis.title.x = element_text(size = 7),
        axis.line.y.left = element_line(),
        axis.text.y.left = element_text(size = 6),
        axis.line.y.right = element_line(colour = mean_col),
        axis.text.y.right = element_text(colour = mean_col, size = 6),
        axis.text.x = element_text(size = 6)
  )


# movement
pid = neutral_meta %>% 
  distinct(M, .keep_all = TRUE) %>% 
  filter(THETA==40) %>% 
  pull(parameter_id)

lm_slopes = allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'c_temp_dist') %>% 
  rename(timeSeriesID = level)

lm_slopes_consec = consecutive_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'cYear') %>% 
  rename(timeSeriesID = level,
         consec_slope = estimate)

movement_plot <-
  lm_slopes %>% 
  select(parameter_id, timeSeriesID, estimate) %>% 
  left_join(neutral_meta %>% 
              select(parameter_id, M)) %>% 
  rename(slope = estimate) %>% 
  left_join(lm_slopes_consec %>% 
              select(parameter_id, timeSeriesID, consec_slope)) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid & Subsampling==100) %>% 
              select(parameter_id, timeSeriesID, raw_Affinity, M)
  ) %>% 
  left_join(allYrs_100_mixed %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              select(parameter_id, Jac_mean)) %>%  
  left_join(consecutive_100_mixed %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              rename(Jac_mean_consec = Jac_mean) %>% 
              select(parameter_id, Jac_mean_consec)) %>%  
  ggplot() +
  # geom_point(aes(x = M, y = slope)) +
  stat_smooth(aes(x = M, y = slope, colour = 'linear_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = M, y = consec_slope, colour = 'linear_col'),
              method = 'gam', linetype = 2,
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = M, y = raw_Affinity, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = M, y = Jac_mean/10, colour = 'mean_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
    stat_smooth(aes(x = M, y = Jac_mean_consec/10, colour = 'mean_col'),
                method = 'gam', linetype = 2,
                formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate',
                     sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                         name = 'Mean Jaccard')) +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col,
                                 'mean_col' = mean_col),
                      guide = 'none') +
  labs(x = 'Immigration probability') +
  theme_classic() +
  theme(axis.title.y = element_text(size=7),
        axis.title.y.right = element_text(size=7),
        axis.title.x = element_text(size = 7),
        axis.line.y.left = element_line(),
        axis.text.y.left = element_text(size = 6),
        axis.line.y.right = element_line(colour = mean_col),
        axis.text.y.right = element_text(colour = mean_col, size = 6),
        axis.text.x = element_text(size = 6)
  )


# duration
pid = 25

duration <- duration %>% 
  mutate(timeSeriesID = paste0('ts',row_number())) %>% 
  mutate(ts_length = case_when(d <= 5 ~ '< 6',
                               (d >= 6 & d <=10) ~ '6-10',
                               (d >= 11 & d <=20) ~ '11-20',
                               (d >= 21 & d <=50) ~ '21-50',
                               (d >= 50) ~ '> 50'))

# discrete duration bins for visualisation
duration$ts_length <- factor(duration$ts_length,
                             levels = c('< 6', '6-10', '11-20', '21-50', '> 50'))

slopes = allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~coef(.))) 

slopes2 = slopes$coefs[[1]]$timeSeriesID %>% 
  as_tibble() %>% 
  mutate(timeSeriesID = rownames(slopes$coefs[[1]]$timeSeriesID))

slopes_consec = consecutive_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~coef(.))) 

slopes2_consec = slopes_consec$coefs[[1]]$timeSeriesID %>% 
  as_tibble() %>% 
  mutate(timeSeriesID = rownames(slopes_consec$coefs[[1]]$timeSeriesID))

duration_plot <-
  slopes2 %>%  
  left_join(duration) %>% 
  rename(slope = c_temp_dist) %>% 
  select(d, slope, timeSeriesID) %>% 
  left_join(slopes2_consec %>% 
              rename(slope_consec = cYear) %>% 
              select(slope_consec, timeSeriesID)) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid & Subsampling==100) %>% 
              mutate(d = Duration - 1) %>%
              select(d, raw_Affinity, timeSeriesID)) %>% 
  left_join(allYrs_100_mean_duration %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              select(parameter_id, timeSeriesID, Jac_mean)) %>%  
  left_join(consecutive_100_mean_duration %>% 
              filter(parameter_id %in% pid) %>% 
              unnest(Jac_mean) %>% 
              rename(Jac_mean_consec = Jac_mean) %>% 
              select(parameter_id, timeSeriesID, Jac_mean_consec)) %>%  
  ggplot() +
  # geom_point(aes(x = d, y = slope, colour = 'linear_col')) +
  stat_smooth(aes(x = d, y = slope, colour = 'linear_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = d, y = slope_consec, colour = 'linear_col'),
              method = 'gam', linetype = 2,
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_point(aes(x = d, y = raw_Affinity, colour = 'affinity_col')) +
  stat_smooth(aes(x = d, y = raw_Affinity, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = d, y = Jac_mean/10, colour = 'mean_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = d, y = Jac_mean_consec/10, colour = 'mean_col'),
              method = 'gam', linetype = 2,
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate',
                     sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                         name = 'Mean Jaccard')) +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col,
                                 'mean_col' = mean_col),
                      guide = 'none') +
  labs(x = 'Duration of time series') +
  theme_classic() +
  theme(axis.title.y = element_text(size=7),
        axis.title.y.right = element_text(size=7),
        axis.title.x = element_text(size = 7),
        axis.line.y.left = element_line(),
        axis.text.y.left = element_text(size = 6),
        axis.line.y.right = element_line(colour = mean_col),
        axis.text.y.right = element_text(colour = mean_col, size = 6),
        axis.text.x = element_text(size = 6)
  )# geom_hline(yintercept = 0, lty = 2) + 



source('~/Dropbox/1current/R_random/functions/gg_legend.R')
leg_plot <-
  slopes2 %>%  
  left_join(duration) %>% 
  rename(slope = c_temp_dist) %>% 
  select(d, slope, timeSeriesID) %>% 
  left_join(slopes2_consec %>% 
              rename(slope_consec = cYear) %>% 
              select(slope_consec, timeSeriesID)) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid & Subsampling==100) %>% 
              mutate(d = Duration - 1) %>%
              select(d, raw_Affinity, timeSeriesID)) %>% 
  ggplot() +
  # geom_point(aes(x = d, y = slope, colour = 'linear_col')) +
  stat_smooth(aes(x = d, y = slope, 
                  colour = 'linear_col', linetype = 'All year pairs'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = d, y = slope_consec, 
                  colour = 'linear_col', linetype = 'Consecutive year pairs'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_point(aes(x = d, y = raw_Affinity, colour = 'affinity_col')) +
  stat_smooth(aes(x = d, y = raw_Affinity, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(data = allYrs_100_mean_duration %>% 
                filter(parameter_id %in% pid) %>% 
                unnest(Jac_mean) %>% 
                left_join(duration),
              aes(x = d, y = Jac_mean, colour = 'mean_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Duration of time series') +
  scale_y_continuous(name = 'Parameter estimate') +
  scale_colour_manual(values = c('mean_col' = mean_col,
                                 'linear_col' = linear_col,
                                 'affinity_col' = affinity_col
  ),
  name = '',
  labels = c('mean_col' = 'Mean',
             'linear_col' = 'Linear model (slope)',
             'affinity_col' = 'Affinity (non-linear)')) +
  scale_linetype_manual(values = c('All year pairs' = 1,
                                   'Consecutive year pairs' = 2),
                        name = '') +
    theme(legend.background=element_rect(fill = alpha("white", 0)),
          legend.key=element_rect(fill = alpha("white", .5)),
          legend.key.size = unit(10, 'mm'),
          legend.direction = 'horizontal') +
  guides(colour = guide_legend(order = 1, nrow = 2,
                               override.aes = list(fill = NA)),
         linetype = guide_legend(order = 2,
                                 override.aes = list(fill = NA, colour = 'black')))

leg_col <- gg_legend(leg_plot)

cowplot::plot_grid(duration_plot, 
                   completeness_plot,
                   N_plot,
                   Spool_plot,
                   movement_plot,
                   leg_col,
                   labels = c('a. Time series duration',
                              'b. Completeness of local sample',
                              'c. Local assemblage size',
                              'd. Regional species pool',
                              'e. Movement'),
                   label_size = 8,
                   ncol = 2)

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/simResults.pdf',
       width = 200, height = 200, units = 'mm')
