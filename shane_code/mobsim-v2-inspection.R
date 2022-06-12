
load('~/Dropbox/1current/sRealm/local_data/mobsim_v2_linear_model_turnover_baseline.Rdata')
load('~/Dropbox/1current/sRealm/local_data/mobsim_v2_linear_model_turnover_allYrPairs.Rdata')

# parameter combinations: clean for plotting
meta <- read_csv('~/Dropbox/1current/sRealm/local_data/IFYAH3130E_jitter_metadata.csv')
# meta <- read_csv('~/Dropbox/1current/sRealm/local_data/ZXSAX1088U_jitter_metadata.csv')
meta <- meta %>% 
  select(parameter_id, S_POOL, N_SIM, SAD_COEF, SIGMA) %>% 
  mutate(Spool = paste0('S=', S_POOL),
         N = paste0('N=', N_SIM),
         SAD = paste0('SAD=', SAD_COEF),
         dispersion = case_when(SIGMA==0.05 ~ 'aggregated',
                                SIGMA==10 ~ 'random')) %>% 
  mutate(label = paste0(Spool, ', ', N, ', ', SAD, ', ', dispersion),
         Spool_label = paste0(N, ', ', SAD, ', ', dispersion),
         N_dispersion_label = paste0(Spool, ', ', SAD))

load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/duration.Rdata')
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



ggplot() +
  facet_wrap(SAD~N) +
  # geom_point(data = baseline_100 %>%
  #              unnest(Jac_lm_tidy) %>%
  #              filter(term == '(Intercept)') %>%
  #              rename(Jac_base = estimate) %>%
  #              select(parameter_id, timeSeriesID, Jac_base) %>%
  #              left_join(meta) %>% 
  #              left_join(duration),
  #            aes(x = d - 0.5, y = Jac_base, colour = Spool, shape = dispersion)) +
  stat_smooth(data = baseline_100 %>%
                unnest(Jac_lm_tidy) %>%
                filter(term == '(Intercept)') %>%
                rename(Jac_base = estimate) %>%
                select(parameter_id, timeSeriesID, Jac_base) %>%
                left_join(meta) %>% 
                left_join(duration),
              aes(x = d - 0.5, y = Jac_base, colour = Spool, linetype = dispersion),
              method = 'lm') +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = 'Time series duration',
       y = 'OLS intercept\n(Jaccard, all years, temporal distance)') +
  theme_bw()


ggplot() +
  facet_wrap(SAD~N) +
  # geom_point(data = baseline_100 %>%
  #              unnest(Jac_lm_tidy) %>%
  #              filter(term == '.x$cYear') %>%
  #              rename(Jac_base = estimate) %>%
  #              select(parameter_id, timeSeriesID, Jac_base) %>%
  #              left_join(meta) %>% 
  #              left_join(duration),
  #            aes(x = d - 0.5, y = Jac_base, colour = Spool, shape = dispersion)) +
  stat_smooth(data = baseline_100 %>%
                unnest(Jac_lm_tidy) %>%
                filter(term == '(Intercept)') %>% #.x$cYear
                rename(Jac_base = estimate) %>%
                select(parameter_id, timeSeriesID, Jac_base) %>%
                left_join(meta) %>% 
                left_join(duration),
              aes(x = d - 0.5, y = Jac_base, colour = Spool, linetype = dispersion),
              method = 'lm') +
  # geom_point(aes(x = d, y = Jac_base_50), colour = 'orange') +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = 'Time series duration',
       y = 'OLS slope\n(Jaccard, baseline comparison)') +
  theme_bw()


ggplot() +
  facet_wrap(SAD~N) +
  geom_point(data = baseline_100 %>%
               unnest(data) %>%
               left_join(meta) %>%
               left_join(duration) %>% 
               # filter(N_SIM==20000 & SAD_COEF %in% c(0.5)) %>% 
               filter(S_POOL==20 & dispersion == 'random'),
             aes(x = cYear, y = Jbeta, colour = timeSeriesID, shape = dispersion)) +
  stat_smooth(data = baseline_100 %>%
                unnest(data) %>%
                left_join(meta) %>%
                left_join(duration) %>% 
                # filter(N_SIM==20000 & SAD_COEF %in% c(0.5)) %>% 
                filter(S_POOL==20 & dispersion == 'random'),
              aes(x = cYear, y = Jbeta, colour = timeSeriesID, linetype = dispersion),
              method = 'lm') +
  theme_bw() +
  theme(legend.position = 'none')
