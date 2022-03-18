neutral_100 <- read_csv('~/Dropbox/1current/sRealm/simRealm/prelim_results/neutral_beta_dist_100.csv')
neutral_meta <- read_csv('~/Dropbox/1current/sRealm/simRealm/data/simulations/QEAYJ1252R_neutral_metadata.csv')

neutral_meta <- neutral_meta %>% 
  select(parameter_id, THETA, M, N) %>% 
  mutate(M = paste0('M=', M),
         N = paste0('N=', N),
         THETA = paste0('THETA=', THETA)) %>% 
  mutate(label = paste0(THETA, ', ', M, ', ', N))

# calculate durations for each time series
duration <- beta_dist_100 %>% 
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
  ungroup()

neutral_baseline_100_coefs <- neutral_baseline_100 %>% 
  mutate(Jac_lm_tidy = map(Jac_lm, broom::tidy),
         MH_lm_tidy = map(mh_lm, broom::tidy))

neutral_baseline_100_coefs %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='(Intercept)') %>% 
  left_join(neutral_meta) %>%
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = estimate)) +
  labs(x = 'Jaccard intercept (mean turnover magnitude)')

neutral_baseline_100_coefs %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='.x$cYear') %>% 
  left_join(neutral_meta) %>%
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_histogram(aes(x = estimate)) +
  labs(x = 'Jaccard slope (rate of turnover)',
       subtitle = 'Comparisons to initial assemblage')


baseline_100_coefs %>% 
  select(parameter_id, timeSeriesID) %>% 
  
  group_by()
unnest(data) %>% 
  # filter(term=='.x$cYear') %>% 
  left_join(meta) %>% 
  ggplot() +
  facet_wrap(~label, ncol = 4) +
  geom_point(aes(x = YEAR2, y = Jbeta, colour = timeSeriesID),
             size = 0.5) +
  # stat_smooth(method = 'lm', se = F,
  #             aes(x = YEAR2, y = Jbeta, colour = timeSeriesID))
  theme(legend.position = 'none')
