# plot results data from Laura
library(tidyverse)

# load the data
load('~/Dropbox/1current/sRealm/simRealm/RivFishTIME_case_study_v2.RData')


##sps pool
sps_pool <- all_metrics %>%
  select(sps_pool, jac_estimate, raw_Affinity, mean_Jac) %>% 
  pivot_longer(cols = 2:4,
               names_to = 'metric',
               values_to = 'value') 

sps_pool$metric <- factor(sps_pool$metric,
                          levels = c('jac_estimate', 'raw_Affinity', 'mean_Jac'),
                          labels = c('Linear rate of change in Jaccard',
                                     'Non-linear (affinity)',
                                     'Mean Jaccard'))

sps_pool_fig <- ggplot(sps_pool) +
  facet_wrap(~metric, scales = 'free') + 
   geom_point(aes(x = sps_pool, y = value, colour = metric)) +
   stat_smooth(aes(x = sps_pool, y = value, colour = metric),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
  scale_colour_manual(values = c('Linear rate of change in Jaccard' = linear_col,
                                 'Non-linear (affinity)' = affinity_col,
                                 'Mean Jaccard' = mean_col),
                      guide = 'none') +
  labs(x = 'Regional species pool size') +
  theme_classic() +
  theme(strip.background = element_rect(linetype = 0))



##time series duration
duration <- all_metrics %>%
  select(n_years, jac_estimate, raw_Affinity, mean_Jac) %>% 
  pivot_longer(cols = 2:4,
               names_to = 'metric',
               values_to = 'value') 
  
duration$metric <- factor(duration$metric,
                          levels = c('jac_estimate', 'raw_Affinity', 'mean_Jac'),
                          labels = c('Linear rate of change in Jaccard',
                                     'Non-linear (affinity)',
                                     'Mean Jaccard'))
duration_figure <- ggplot(duration) +
  facet_wrap(~metric, scales = 'free') +
   geom_point(aes(x = n_years, y = value, colour = metric)) +
   stat_smooth(aes(x = n_years, y = value, colour = metric),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
  scale_colour_manual(values = c('Linear rate of change in Jaccard' = linear_col,
                                  'Non-linear (affinity)' = affinity_col,
                                  'Mean Jaccard' = mean_col),
                      guide = 'none') +
   labs(x = 'Number of years') +
   theme_classic() +
   theme(strip.background = element_rect(linetype = 0))


cowplot::plot_grid(duration_figure,
                   sps_pool_fig,
                   nrow = 2)
ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/case-study.png',
       width = 290, height = 200, units = 'mm')


##end##



