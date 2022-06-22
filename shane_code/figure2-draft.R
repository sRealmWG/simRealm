# preliminary figure 2 showing affinity and linear model results (fit to neutral simulations)

library(tidyverse)

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

# assemblage size (use completely sampled communities only)
load('~/Dropbox/1current/sRealm/local_data/neutral_v2_allYrs_100_mixed.Rdata')

pid = neutral_meta %>% 
  filter(THETA==40 & M==0.2) %>% 
  pull(parameter_id)


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

neutral.res %>% 
  filter(parameter_id %in% pid) %>% 
  ggplot() +
  geom_point(aes(x = N, y = log(raw_Affinity+1)))
