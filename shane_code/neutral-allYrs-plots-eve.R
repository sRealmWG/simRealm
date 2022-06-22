library(tidyverse)
# neutral results plots on eve: models fit to allYrs
neutral_meta <- read_csv('/data/idiv_chase/simRealm/neutral_metadata_v3.csv')
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

# assemblage size (use completely sampled communities only)
load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_100_mixed.Rdata')

pid = neutral_meta %>% 
  filter(THETA==40 & M==0.2) %>% 
  pull(parameter_id)

pdf('/data/idiv_chase/simRealm/results/neutral/figures/v3/neutral-allYrs-local-assemblage-size.pdf',
    width = 9, height = 9)

allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  geom_point(aes(x = N, y = Jac_mean)) +
  stat_smooth(aes(x = N, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Local assemblage size (individuals)',
       y = 'Mean Jaccard\n(all year pairs)') 

allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_hlm_tidy) %>% 
  filter(term=='c_temp_dist') %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  ggplot() +
  geom_point(aes(x = N, y = slope)) +
  stat_smooth(aes(x = N, y = slope),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Local assemblage size (individuals)',
       y = 'Slope\n(all year pairs)') 

dev.off()


# local sample completeness assemblage size (use completely sampled communities only)
load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_100_mixed.Rdata')
load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_75_mixed.Rdata')
load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_50_mixed.Rdata')
load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_25_mixed.Rdata')
load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_10_mixed.Rdata')

pid = neutral_meta %>% 
  filter(THETA==40 & M==0.2) %>% 
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

pdf('/data/idiv_chase/simRealm/results/neutral/figures/v3/neutral-allYrs-local-completeness.pdf',
    width = 9, height = 9)

completeness_dat %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  # facet_wrap(~label) + 
  geom_point(aes(x = completeness, y = Jac_mean, colour = Nlocal)) +
  stat_smooth(aes(x = completeness, y = Jac_mean, colour = Nlocal),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Percentage of individuals sampled',
       y = 'Mean Jaccard\n(all year pairs)') 

completeness_dat %>% 
  unnest(Jac_hlm_tidy) %>% 
  filter(term=='c_temp_dist') %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  ggplot() +
  geom_point(aes(x = completeness, y = slope, colour = Nlocal)) +
  stat_smooth(aes(x = completeness, y = slope, colour = Nlocal),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Percentage of individuals sampled',
       y = 'Slope\n(all year pairs)') 

dev.off()


# species pool size
pid = neutral_meta %>% 
  distinct(THETA, .keep_all = TRUE) %>% 
  pull(parameter_id)

pdf('/data/idiv_chase/simRealm/results/neutral/figures/v3/neutral-allYrs-theta.pdf',
    width = 9, height = 9)

allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  # facet_wrap(~label) +
  geom_point(aes(x = THETA, y = Jac_mean)) +
  stat_smooth(aes(x = THETA, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Regional species pool size',
       y = 'Mean Jaccard\n(all year pairs)') 

allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_hlm_tidy) %>% 
  filter(term=='c_temp_dist') %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  ggplot() +
  geom_point(aes(x = THETA, y = slope)) +
  stat_smooth(aes(x = THETA, y = slope),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Regional species pool size',
       y = 'Slope\n(all year pairs)') 

dev.off()

# movement
pid = neutral_meta %>% 
  distinct(M, .keep_all = TRUE) %>% 
  filter(THETA==40) %>% 
  pull(parameter_id)

pdf('/data/idiv_chase/simRealm/results/neutral/figures/v3/neutral-allYrs-movement.pdf',
    width = 9, height = 9)

allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  facet_wrap(~SN_label) +
  geom_point(aes(x = M, y = Jac_mean)) +
  stat_smooth(aes(x = M, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Probability individual is replaced by individual from regional pool',
       y = 'Mean Jaccard\n(all year pairs)') 

allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_hlm_tidy) %>% 
  filter(term=='c_temp_dist') %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  ggplot() +
  facet_wrap(~SN_label) +
  geom_point(aes(x = M, y = slope)) +
  stat_smooth(aes(x = M, y = slope),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Probability individual is replaced by individual from regional pool',
       y = 'Slope\n(all year pairs)') 

dev.off()

# duration
pid = 25
load('/data/idiv_chase/simRealm/duration.Rdata')

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

load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_100_mean_duration.Rdata')
load('/data/idiv_chase/simRealm/results/neutral/model_fits/v3/allYrs_100_mixed.Rdata')

slopes = allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~coef(.))) 

slopes2 = slopes$coefs[[1]]$timeSeriesID %>% 
  as_tibble() %>% 
  mutate(timeSeriesID = rownames(slopes$coefs[[1]]$timeSeriesID))

pdf('/data/idiv_chase/simRealm/results/neutral/figures/v3/neutral-allYrs-duration.pdf',
    width = 9, height = 9)

allYrs_100_mean_duration %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(duration) %>% 
  ggplot() +
  geom_point(aes(x = d, y = Jac_mean)) +
  stat_smooth(aes(x = d, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Duration of time series',
       y = 'Mean Jaccard\n(all year pairs)') 

slopes2 %>%  
  left_join(duration) %>% 
  rename(slope = c_temp_dist) %>% 
  ggplot() +
  geom_point(aes(x = d, y = slope)) +
  stat_smooth(aes(x = d, y = slope),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Duration of time series',
       y = 'Slope\n(all year pairs)') 

dev.off()
