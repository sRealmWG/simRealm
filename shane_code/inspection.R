# fit linear models to distances measures of turnover

beta_dist_100 <- read_csv('~/Dropbox/1current/sRealm/local_data/beta_dist_100.csv')
meta <- read_csv('~/Dropbox/1current/sRealm/local_data/IFYAH3130E_jitter_metadata.csv')

meta <- meta %>% 
  select(parameter_id, S_POOL, N_SIM, SAD_COEF, SIGMA) %>% 
  mutate(S = paste0('S=', S_POOL),
         N = paste0('N=', N_SIM),
         SAD = paste0('SAD=', SAD_COEF),
         dispersion = paste0('agg=', SIGMA)) %>% 
  mutate(label = paste0(S, ', ', N, ', ', SAD, ', ', dispersion))

# calculate durations for each time series
duration <- beta_dist_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  summarise(duration = max(YEAR2) - min(YEAR1)) %>% 
  ungroup()

baseline_100 <- beta_dist_100 %>% 
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

allYrs_100 <- beta_dist_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, c_temp_dist, temp_dist, Jbeta, MH_dist) %>% 
  nest(data = c(c_temp_dist, temp_dist, Jbeta, MH_dist)) %>% 
  mutate(Jac_lm_allYrs = map(data, ~lm(.x$Jbeta ~ .x$c_temp_dist)),
         mh_lm_allYrs = map(data, ~lm(.x$MH_dist ~ .x$c_temp_dist))) %>% 
  mutate(Jac_lm_allYrs_tidy = map(Jac_lm_allYrs, broom::tidy),
         MH_lm_allYrs_tidy = map(mh_lm_allYrs, broom::tidy))


baseline_100 %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='(Intercept)') %>% 
  left_join(meta) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = estimate)) +
  labs(x = 'Jaccard intercept (mean turnover magnitude)')

allYrs_100 %>% 
  unnest(Jac_lm_allYrs_tidy) %>% 
  filter(term=='(Intercept)') %>% 
  left_join(meta) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = estimate)) +
  labs(x = 'Jaccard intercept (mean turnover magnitude)')

baseline_100 %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='.x$cYear') %>% 
  left_join(meta) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = estimate)) +
  labs(x = 'Jaccard slope (rate of turnover)',
       subtitle = 'Comparisons to initial assemblage')

allYrs_100 %>% 
  unnest(Jac_lm_allYrs_tidy) %>% 
  filter(term=='.x$c_temp_dist') %>% 
  left_join(meta) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = estimate)) +
  labs(x = 'Jaccard slope (rate of turnover)',
       subtitle = 'Comparisons to initial assemblage')

baseline_100 %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='.x$cYear') %>% 
  left_join(meta) %>% 
  left_join(duration) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_point(aes(x = duration, y = estimate),
             size = 0.5, alpha = 0.25) +
  stat_smooth(aes(x = duration, y = estimate),
              se = F) +
  labs(x = 'duration', 
       y = 'Jaccard slope (rate of turnover)',
       subtitle = 'Comparisons to initial assemblage')

allYrs_100 %>% 
  unnest(Jac_lm_allYrs_tidy) %>% 
  filter(term=='.x$c_temp_dist') %>% 
  left_join(meta) %>% 
  left_join(duration) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_point(aes(x = duration, y = estimate),
             size = 0.5, alpha = 0.25) +
  stat_smooth(aes(x = duration, y = estimate),
              se = F, size = 0.5) +
  labs(x = 'duration', 
       y = 'Jaccard slope (rate of turnover)',
       subtitle = 'Comparisons to initial assemblage')

