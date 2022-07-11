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


# two colour theme 
linear_col = '#bc5090'
affinity_col = '#ffa600'


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


N_mean_jac_plot <- allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  # geom_point(aes(x = N, y = Jac_mean)) +
  stat_smooth(aes(x = N, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Local assemblage size (individuals)',
       y = 'Mean Jaccard\n(all year pairs)') +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))

N_slope_affinity_plot <-
  neutral.res %>% 
              filter(parameter_id %in% pid & (Subsampling == 100)) %>% 
              select(parameter_id, timeSeriesID, N, raw_Affinity) %>% 
  left_join(slopes %>%
              select(parameter_id, timeSeriesID, estimate)) %>%
  ggplot() +
  # geom_point(aes(x = N, y = slope)) +
  stat_smooth(aes(x = N, y = estimate, colour = 'linear_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = N, y = raw_Affinity/1, colour = 'affinity_col'), # need to transform here so as y-axis scale is comparable
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate') +
  scale_colour_manual(name = '', 
                      values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col),
                      labels = c('linear_col' = 'Linear model',
                                 'affinity_col' = 'Affinity (non-linear)')
                      # guide = 'none'
                      ) +
  labs(x = 'Local assemblage size (individuals)') +
  theme_minimal() +
  theme(axis.title = element_text(size=7),
        axis.text = element_text(size = 6),
        legend.position = c(1,1),
        legend.justification = c(1,1)
  )


N_combo <- cowplot::plot_grid(N_mean_jac_plot,
                   N_slope_affinity_plot)


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

lm_slopes = completeness_dat %>% 
  filter(parameter_id %in% pid) %>% 
  mutate(coefs = map(Jac_hlm, ~tidy(., effects = 'ran_coefs'))) %>% 
  unnest(coefs) %>% 
  filter(term == 'c_temp_dist') %>% 
  rename(timeSeriesID = level)

completeness_meanJac_plot <- completeness_dat %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  # facet_wrap(~label) + 
  # geom_point(aes(x = completeness, y = Jac_mean, colour = Nlocal)) +
  stat_smooth(aes(x = completeness, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Percentage of individuals sampled',
       y = 'Mean Jaccard\n(all year pairs)') +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))

completeness_slope_affinity_plot <-
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
  ggplot() +
  # geom_point(aes(x = completeness, y = slope, colour = Nlocal)) +
  stat_smooth(aes(x = completeness, y = slope, colour = 'linear_col'),
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = completeness, y = raw_Affinity/1000, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  scale_y_continuous(name = 'Parameter estimate') +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col),
                      guide = 'none') +
  labs(x = 'Percentage of individuals sampled') +
  theme_minimal() +
  theme(axis.title = element_text(size=7),
        axis.text = element_text(size = 6),
        legend.position = 'none',
        legend.justification = c(1,1)
  )

completeness_combo <- cowplot::plot_grid(completeness_meanJac_plot,
                   completeness_slope_affinity_plot)


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

Spool_meanJac_plot <- allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  # facet_wrap(~label) +
  # geom_point(aes(x = THETA, y = Jac_mean)) +
  stat_smooth(aes(x = THETA, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Regional species pool size',
       y = 'Mean Jaccard\n(all year pairs)') +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))

Spool_slope_affinity_plot <-
lm_slopes %>% 
  select(parameter_id, timeSeriesID, estimate) %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  select(parameter_id, timeSeriesID, THETA, slope) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid & Subsampling==100) %>% 
              select(parameter_id, timeSeriesID, N, THETA, raw_Affinity)
  ) %>% 
  ggplot() +
  geom_point(aes(x = THETA, y = slope)) +
  stat_smooth(aes(x = THETA, y = slope, colour = 'linear_col'),
              method = 'gam',
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  geom_point(aes(x = THETA, y = raw_Affinity, colour = 'affinity_col')) +
  stat_smooth(aes(x = THETA, y = raw_Affinity, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate') +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col),
                      guide = 'none') +
  labs(x = 'Regional species pool size') +
  theme_minimal() +
  theme(axis.title = element_text(size=7),
        axis.text = element_text(size = 6),
        legend.position = 'none',
        legend.justification = c(1,1)
  )

Spool_combo <- cowplot::plot_grid(Spool_meanJac_plot,
                   Spool_slope_affinity_plot)

# movement
pid = neutral_meta %>% 
  distinct(M, .keep_all = TRUE) %>% 
  filter(THETA==40) %>% 
  pull(parameter_id)


movement_meanJac_plot <- allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  # facet_wrap(~SN_label) +
  # geom_point(aes(x = M, y = Jac_mean)) +
  stat_smooth(aes(x = M, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Probability individual is replaced by individual from regional pool',
       y = 'Mean Jaccard\n(all year pairs)') +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))

movement_slope_affinity_plot <- allYrs_100_mixed %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_hlm_tidy) %>% 
  filter(term=='c_temp_dist') %>% 
  left_join(neutral_meta) %>% 
  rename(slope = estimate) %>% 
  select(parameter_id, slope, M) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid & Subsampling==100) %>% 
              select(parameter_id, raw_Affinity, M)
            ) %>% 
  ggplot() +
  # geom_point(aes(x = M, y = slope)) +
  stat_smooth(aes(x = M, y = slope, colour = 'linear_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  stat_smooth(aes(x = M, y = raw_Affinity, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  scale_y_continuous(name = 'Parameter estimate') +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col),
                      guide = 'none') +
  labs(x = 'Probability individual is replaced by individual from regional pool') +
  theme_minimal() +
  theme(axis.title = element_text(size=7),
        axis.text = element_text(size = 6),
        legend.position = 'none',
        legend.justification = c(1,1)
  )

movement_combo <- cowplot::plot_grid(movement_meanJac_plot,
                   movement_slope_affinity_plot)


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


duration_meanJac_plot <- allYrs_100_mean_duration %>% 
  filter(parameter_id %in% pid) %>% 
  unnest(Jac_mean) %>% 
  left_join(duration) %>% 
  ggplot() +
  geom_point(aes(x = d, y = Jac_mean)) +
  stat_smooth(aes(x = d, y = Jac_mean),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Duration of time series',
       y = 'Mean Jaccard\n(all year pairs)') +
  theme_minimal() +
  theme(axis.title = element_text(size = 7),
        axis.text = element_text(size = 6))

duration_slope_affinity_plot <-
slopes2 %>%  
  left_join(duration) %>% 
  rename(slope = c_temp_dist) %>% 
  select(d, slope, timeSeriesID) %>% 
  left_join(neutral.res %>% 
              filter(parameter_id %in% pid & Subsampling==100) %>% 
              mutate(d = Duration - 1) %>%
              select(d, raw_Affinity, timeSeriesID)) %>% 
  ggplot() +
  # geom_point(aes(x = d, y = slope, colour = 'linear_col')) +
  stat_smooth(aes(x = d, y = slope, colour = 'linear_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_point(aes(x = d, y = raw_Affinity, colour = 'affinity_col')) +
  stat_smooth(aes(x = d, y = raw_Affinity, colour = 'affinity_col'),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)) +
  # geom_hline(yintercept = 0, lty = 2) + 
  labs(x = 'Duration of time series') +
  scale_y_continuous(name = 'Parameter estimate') +
  scale_colour_manual(values = c('linear_col' = linear_col,
                                 'affinity_col' = affinity_col),
                      guide = 'none') +
  theme_minimal() +
  theme(axis.title = element_text(size=7),
        axis.text = element_text(size = 6),
        legend.position = 'none',
        legend.justification = c(1,1)
  )

duration_combo <- cowplot::plot_grid(duration_meanJac_plot,
                   duration_slope_affinity_plot)

cowplot::plot_grid(N_combo,
                   completeness_combo,
                   Spool_combo,
                   movement_combo,
                   duration_combo, 
                   labels = c('a. Time series duration',
                              'b. Completeness of local sample',
                              'c. Local assemblage size',
                              'd. Regional species pool',
                              'e. Movement'),
                              label_size = 8,
                   ncol = 2)

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/simResults.pdf',
       width = 290, height = 200, units = 'mm')
