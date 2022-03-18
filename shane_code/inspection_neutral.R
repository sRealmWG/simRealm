# inspection of the neutral model dissimilarity dynamics

library(tidyverse)
neutral_100 <- read_csv('~/Dropbox/1current/sRealm/simRealm/simRealm/prelim_results/neutral_beta_dist_100.csv')
neutral_meta <- read_csv('~/Dropbox/1current/sRealm/simRealm/simRealm/data/simulations/QEAYJ1252R_neutral_metadata.csv')

neutral_meta <- neutral_meta %>% 
  select(parameter_id, THETA, M, N) %>% 
  mutate(M = paste0('M=', M),
         N = paste0('N=', N),
         THETA = paste0('THETA=', THETA)) %>% 
  mutate(label = paste0(THETA, ', ', M, ', ', N))

# calculate durations for each time series
neutral_duration <- neutral_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  summarise(duration = max(YEAR2) - min(YEAR1)) %>% 
  ungroup()

neutral_baseline_100 <- neutral_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  filter(YEAR1 == min(YEAR1)) %>% 
  ungroup() %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(cYear = YEAR2 - mean(YEAR2)) %>% 
  select(parameter_id, timeSeriesID, YEAR2, cYear, Jbeta, MH_dist) %>% 
  nest(data = c(cYear, YEAR2, Jbeta, MH_dist)) %>% 
  mutate(Jac_lm = map(data, ~lm(.x$Jbeta ~ .x$cYear)),
         mh_lm = map(data, ~lm(.x$MH_dist ~ .x$cYear))) %>% 
  mutate(Jac_lm_tidy = map(Jac_lm, broom::tidy),
         MH_lm_tidy = map(mh_lm, broom::tidy))

neutral_consecutive_100 <- neutral_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  filter((YEAR2 - YEAR1) == 1) %>% 
  ungroup() %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(cYear = YEAR2 - mean(YEAR2)) %>% 
  select(parameter_id, timeSeriesID, YEAR2, cYear, Jbeta, MH_dist) %>% 
  nest(data = c(cYear, YEAR2, Jbeta, MH_dist)) %>% 
  mutate(Jac_lm = map(data, ~lm(.x$Jbeta ~ .x$cYear)),
         mh_lm = map(data, ~lm(.x$MH_dist ~ .x$cYear))) %>% 
  mutate(Jac_lm_tidy = map(Jac_lm, broom::tidy),
         MH_lm_tidy = map(mh_lm, broom::tidy))

neutral_allYrs_100 <- neutral_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, c_temp_dist, temp_dist, Jbeta, MH_dist) %>% 
  nest(data = c(c_temp_dist, temp_dist, Jbeta, MH_dist)) %>% 
  mutate(Jac_lm_allYrs = map(data, ~lm(.x$Jbeta ~ .x$c_temp_dist)),
         mh_lm_allYrs = map(data, ~lm(.x$MH_dist ~ .x$c_temp_dist))) %>% 
  mutate(Jac_lm_allYrs_tidy = map(Jac_lm_allYrs, broom::tidy),
         MH_lm_allYrs_tidy = map(mh_lm_allYrs, broom::tidy))

neutral_baseline_100_coefs %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='(Intercept)') %>% 
  left_join(neutral_meta) %>%
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = estimate)) +
  labs(x = 'Jaccard intercept (mean turnover magnitude)')

neutral_baseline_100 %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='.x$cYear') %>% 
  left_join(neutral_meta) %>%
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = duration, y = estimate)) +
  labs(x = 'Jaccard slope (rate of turnover)',
       subtitle = 'Comparisons to initial assemblage')

neutral_baseline_100 %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='.x$cYear') %>% 
  left_join(neutral_meta) %>%
  left_join(neutral_duration) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_point(aes(x = duration, y = estimate), 
             size = 0.5, alpha = 0.5) +
  stat_smooth(aes(x = duration, y = estimate),
              se = F) +
  labs(x = 'duration', 
       y = 'Jaccard slope (rate of turnover)',
       subtitle = 'Comparisons to initial assemblage')

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/neutral_slopes_duration.png',
       width = 290, height = 200, units = 'mm')
  
neutral_consecutive_100 %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='.x$cYear') %>% 
  rename(estimate_consec = estimate) %>% 
  select(parameter_id, timeSeriesID, estimate_consec) %>% 
  left_join(neutral_baseline_100 %>% 
              unnest(Jac_lm_tidy) %>% 
              filter(term=='.x$cYear') %>% 
              select(parameter_id, timeSeriesID, estimate)) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  facet_wrap(~label) +
  geom_point(aes(x = estimate, y = estimate_consec)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  stat_smooth(method = 'lm',
              aes(x = estimate, estimate_consec)) +
  labs(x = 'comparison to initial assemblage (slope)',
       y = 'consecutive comparisons (slope)') +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white'))

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/neutral_consecutive_initial_slope_comparison.png',
       width = 290, height = 200, units = 'mm')
