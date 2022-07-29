# visual inspection of coverage
library(tidyverse)

# remote wrangle
setwd('/data/idiv_chase/simRealm/results/neutral/coverage/')
filelist = dir(pattern=paste0("neutral-v3-coverage-10-"))

dat = tibble()
for(i in 1:length(filelist)){
  load(filelist[i])
  
  dat = bind_rows(dat, coverage_per_yr)
}

# there are repeated values for each year
dat %>%
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(n_vals = map(coverage, ~length(unique(.)))) %>% 
  unnest(n_vals) %>% 
  filter(n_vals > 1)

dat <- dat %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(cov = map(coverage, ~unique(.))) %>% 
  unnest(cov)

cov_summary_10 <- dat %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  summarise(mean_cov = mean(cov),
         sd_cov = sd(cov)) %>% 
  ungroup() %>% 
  mutate(completeness = '10')

save(cov_summary_10,
     file = 'coverage_summary_10.Rdata')

# plot results locally
load('~/Dropbox/1current/sRealm/local_data/coverage_summary_100.Rdata')
# tidy up 
cov_summary_100 = cov_summary %>% 
  mutate(completeness = '100')
rm(cov_summary)

load('~/Dropbox/1current/sRealm/local_data/coverage_summary_75.Rdata')
load('~/Dropbox/1current/sRealm/local_data/coverage_summary_50.Rdata')
load('~/Dropbox/1current/sRealm/local_data/coverage_summary_25.Rdata')
load('~/Dropbox/1current/sRealm/local_data/coverage_summary_10.Rdata')

# meta data
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

# assemblage size (use completely sampled communities only)
pid = neutral_meta %>% 
  filter(THETA==40 & M==0.2) %>% 
  pull(parameter_id)

dat <- bind_rows(cov_summary_100,
          cov_summary_75,
          cov_summary_50,
          cov_summary_25,
          cov_summary_10) 

dat$completeness <- factor(dat$completeness,
                           levels = c('100', '75', '50', '25', '10'))


dat %>% 
  filter(parameter_id %in% pid) %>% 
  left_join(neutral_meta) %>% 
  ggplot() +
  geom_point(aes(x = N, y = mean_cov, colour = completeness)) +
  stat_smooth(aes(x = N, y = mean_cov, colour = completeness),
              method = 'gam', 
              formula = y ~ s(x, bs = 'cs', k = 5)
              ) +
  scale_color_brewer(name = '% of individuals sampled',
                     type = 'seq',
                     direction = -1) +
  labs(y = 'Coverage',
       x = 'Assemblage size (number of individuals)') +
  theme_classic() +
  theme(legend.position = c(0.4, 0.5))

ggsave('~/Dropbox/1current/sRealm/simRealm/simRealm/figures/coverage.pdf',
       width = 180, height = 120, units = 'mm')
