# fit linear models to distances measures of turnover
library(tidyverse)
beta_dist_100 <- read_csv('~/Dropbox/1current/sRealm/local_data/beta_dist_100.csv')
meta <- read_csv('~/Dropbox/1current/sRealm/local_data/IFYAH3130E_jitter_metadata.csv')

# parameter combinations: clean for plotting
meta <- meta %>% 
  select(parameter_id, S_POOL, N_SIM, SAD_COEF, SIGMA) %>% 
  mutate(Spool = paste0('S=', S_POOL),
         N = paste0('N=', N_SIM),
         SAD = paste0('SAD=', SAD_COEF),
         dispersion = case_when(SIGMA==0.05 ~ 'aggregated',
                                SIGMA==10 ~ 'random')) %>% 
  mutate(label = paste0(Spool, ', ', N, ', ', SAD, ', ', dispersion),
         Spool_label = paste0(N, ', ', SAD, ', ', dispersion))

# calculate durations for each time series
duration <- beta_dist_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  summarise(duration = max(YEAR2) - min(YEAR1) + 1) %>% 
  ungroup() %>% 
  mutate(ts_length = case_when(duration <= 5 ~ '< 6',
                               (duration >= 6 & duration <=10) ~ '6-10',
                               (duration >= 11 & duration <=20) ~ '11-20',
                               (duration >= 21 & duration <=50) ~ '21-50',
                               (duration >= 50) ~ '> 50'))

duration$ts_length <- factor(duration$ts_length,
                             levels = c('< 6', '6-10', '11-20', '21-50', '> 50'))

ggplot() +
  geom_bar(data = duration,
                 aes(x = ts_length))
# get alpha-scale univariate metrics (richness, total abundance, etc).
# we are interested in biodiversity change, so we want ≥5 species (other criteria possibly desirable too)
load('~/Dropbox/1current/sRealm/simRealm/simRealm/prelim_results/mobsim_alpha_metrics.Rdata')

alpha_scale_100 %>% 
  left_join(meta, by = c('parameter_id')) %>% 
  filter(S > 4) %>% 
  filter(Spool == 'S=200') %>% 
  filter(parameter_id==16) %>% 
  ggplot() +
  facet_grid(~label ) +
  stat_smooth(method = 'lm', se = F,
              aes(x = timestep, y = S, group = timeSeriesID),
              size = 0.5) +
  scale_y_continuous(trans = 'log')
  
# fit linear models to richness, abundance and evenness on linear and proportional scales



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

consecutive_100 <- beta_dist_100 %>% 
  filter((YEAR2-YEAR1) == 1) %>% 
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

allYrs_100 %>%
  left_join(meta) %>% 
  unnest(data) %>% 
  ggplot() +
  facet_wrap(~label) +
  geom_point(aes(x = temp_dist, y = Jbeta, colour = timeSeriesID),
             size = 0.5, alpha = 0.5) +
  stat_smooth(method = 'lm', se = F,
              aes(x = temp_dist, y = Jbeta, colour = timeSeriesID),
              size = 0.75) +
  theme(legend.position = 'none')

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/mobsim_jbeta_tempDist.png',
       width = 300, height = 200, units = 'mm')  

# compare slopes from models fit to comparisons between initial assemblage and all subsequent assemblages with
# assemblages compared between consecutive years
consecutive_100 %>% 
  unnest(Jac_lm_tidy) %>% 
  filter(term=='.x$cYear') %>% 
  rename(estimate_consec = estimate) %>% 
  select(parameter_id, timeSeriesID, estimate_consec) %>% 
  left_join(baseline_100 %>% 
              unnest(Jac_lm_tidy) %>% 
              filter(term=='.x$cYear') %>% 
              select(parameter_id, timeSeriesID, estimate)) %>% 
  left_join(meta) %>% 
  ggplot() +
  facet_wrap(~label) +
  geom_point(aes(x = estimate, estimate_consec)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  stat_smooth(method = 'lm',
              aes(x = estimate, estimate_consec)) +
  labs(x = 'comparison to initial assemblage (slope)',
       y = 'consecutive comparisons (slope)') +
  theme_minimal() +
  theme(plot.background = element_rect(fill = 'white'))

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/mobsim_consecutive_initial_slope_comparison.png',
       width = 290, height = 200, units = 'mm')

# compare response metrics: jaccard and morisita-horn
jac_mh_coefs <- left_join(allYrs_100 %>% 
  unnest(c(Jac_lm_allYrs_tidy)) %>% 
    rename(jac_estimate = estimate) %>% 
    select(parameter_id, timeSeriesID, term, jac_estimate),
  allYrs_100 %>% 
    unnest(c(MH_lm_allYrs_tidy)) %>% 
    rename(mh_estimate = estimate) %>% 
    select(parameter_id, timeSeriesID, term, mh_estimate)) %>% 
  left_join(meta) %>% 
  left_join(duration)
  
ggplot() +
  facet_grid(Spool~Spool_label,# scales = 'free',
             labeller = label_wrap_gen(width = 10)) +
  geom_point(data = jac_mh_coefs %>% 
               filter(term=='(Intercept)'),
             aes(x = jac_estimate, y = mh_estimate, colour = ts_length), 
             alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_viridis_d(name = 'Duration', 
                        direction = -1) + 
  labs(x = 'jaccard intercept (mean)',
       y = 'morisita-horn (mean)',
       subtitle = 'mobsim (complete local sample)') +
  theme_minimal() +
  coord_fixed() + 
  theme(plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position = 'top',
        legend.direction = 'horizontal',
        strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 6))
  
ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/mobsim_compare_jaccard_mh_average.png',
       width = 350, height = 80, units = 'mm')

ggplot() +
  facet_grid(Spool_label~Spool,# scales = 'free',
             labeller = label_wrap_gen(width = 10)) +
  geom_point(data = jac_mh_coefs %>% 
               filter(term=='.x$c_temp_dist'),
             aes(x = jac_estimate, y = mh_estimate, colour = ts_length), 
             alpha = 0.5) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_viridis_d(name = 'Duration', 
                        direction = -1) + 
  labs(x = 'jaccard slope',
       y = 'morisita-horn slope',
       subtitle = 'mobsim (complete local sample)') +
  theme_minimal() +
  coord_fixed() + 
  theme(plot.background = element_rect(fill = 'white', colour = 'white'),
        legend.position = 'top',
        legend.direction = 'horizontal',
        strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 6))

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/test.png',
       width = 100, height = 350, units = 'mm')

# focus on comparing species pool size
left_join(jac_mh_coefs %>% 
    filter(term=='.x$c_temp_dist' & S_POOL == 20) %>% 
    rename(jac_slope20 = jac_estimate,
           mh_slope20 = mh_estimate) %>% 
      dplyr::select(-parameter_id, timeSeriesID, term, jac_slope20, mh_slope20),
  jac_mh_coefs %>% 
    filter(term=='.x$c_temp_dist' & S_POOL == 200) %>% 
    rename(jac_slope200 = jac_estimate,
           mh_slope200 = mh_estimate) %>% 
    select(-parameter_id, timeSeriesID, term, jac_slope200, mh_slope200),
  by = c('timeSeriesID', 'term')) %>% 
  ggplot() +
  geom_point(aes(x = jac_slope200, y = jac_slope20, colour = ts_length.x)) +
  labs(x = 'jaccard slope (species pool = 200)',
       y = 'jaccard slope (species pool = 20)',
       subtitle = 'mobsim: compare turnover rate estimates when species pool differs in size')
geom_point(aes(x = mh_slope200, y = mh_slope20))
  

