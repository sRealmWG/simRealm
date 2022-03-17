rm(list=ls())

load('~/Dropbox/1current/sRealm/simRealm/prelim_results/mob_sim_local_metrics_alpha_beta_diss_pid_1-24.Rdata')


beta_dist_100 %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  filter(parameter_id == 1 & timeSeriesID=='ts1') %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  # filter(YEAR1==min(YEAR1)) %>% 
  # , 'ts2', 'ts3', 'ts4', 'ts5', 'ts6', 'ts7', 'ts8', 'ts9'
  ggplot() +
  # facet_wrap(~parameter_id, scales = 'free_x') +
  geom_point(aes(x = c_temp_dist, y = Jbeta, colour = timeSeriesID),
             # size = 0.5, alpha = 0.2
             ) +
  stat_smooth(aes(x = c_temp_dist, y = Jbeta, 
                  colour = interaction(parameter_id, timeSeriesID)),
              method = 'lm', se = F) +
  theme(legend.position = 'none')


beta_dist_100 %>% 
  filter(parameter_id %in% c(1:9) & timeSeriesID %in% c('ts2')) %>% 
  # , 'ts2', 'ts3', 'ts4', 'ts5', 'ts6', 'ts7', 'ts8', 'ts9'
  ggplot() +
  facet_wrap(~parameter_id, scales = 'free_x') +
  geom_point(aes(x = YEAR2, y = Jbeta, colour = timeSeriesID),
             size = 0.5, alpha = 0.2) +
  stat_smooth(aes(x = YEAR2, y = Jbeta, 
                  colour = interaction(parameter_id, timeSeriesID)),
              method = 'lm', se = F) +
  theme(legend.position = 'none')
