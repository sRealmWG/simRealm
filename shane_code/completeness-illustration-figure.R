# want a visual illustration of the subsampling (completeness result)

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

completeness_slopes = completeness_dat %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  select(parameter_id, completeness, level, term, estimate) %>% 
  pivot_wider(names_from = term,
              values_from = estimate) %>% 
  rename(Intercept = `(Intercept)`,
         timeSeriesID = level)

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

left_join(completeness_slopes,
          duration)

ggplot() +
  facet_wrap(~completeness) +
  geom_point(data = completeness_dat %>% 
               unnest(data) %>% 
               filter(parameter_id == 13),
             aes(x = temp_dist, y = Jbeta), 
             alpha = 0.1, size = 0.2) +
  stat_smooth(data = completeness_dat %>% 
                unnest(data) %>% 
                filter(parameter_id == 13),
              method = 'lm', se = F,
              aes(x = temp_dist, y = Jbeta, group = timeSeriesID),
              size = 0.25)

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/completeness-illustration.pdf',
       width = 290, height = 200, units = 'mm')


ggplot() +
  facet_wrap(~completeness) +
  geom_point(data = completeness_dat_consec %>% 
               unnest(data) %>% 
               filter(parameter_id == 13),
             aes(x = cYear, y = Jbeta), 
             alpha = 0.1, size = 0.2) +
  stat_smooth(data = completeness_dat_consec %>% 
                unnest(data) %>% 
                filter(parameter_id == 13),
              method = 'lm', se = F,
              aes(x = cYear, y = Jbeta, group = timeSeriesID),
              size = 0.25) +
  stat_smooth(data = completeness_dat_consec %>% 
                unnest(data) %>% 
                filter(parameter_id == 13),
              method = 'lm', se = F,
              aes(x = cYear, y = Jbeta),
              size = 0.5, colour = 'black')
