library(tidyverse)
# get array job task id (use as parameter_id [PID])
PID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

filelist = dir(pattern=paste0("parameter_id-", PID, "-"))

dat = tibble()
for(i in 1:length(filelist)){
  # first version of directed movement
  temp <- read_csv(paste0(filelist[i]))
  
  dat = bind_rows(dat, temp)
}

dat <- dat %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup()

beta_dist_100 <- tibble()
alpha_100 <- tibble()
for(i in 1:nrow(dat)){
  # counter for sanity
  print(paste('calculation ', i, 'of ', nrow(dat)))
  
  # long data
  comm_long = dat %>% 
    slice(i) %>% 
    unnest(data) %>% 
    arrange(-desc(timestep))
  
  alpha_temp <- comm_long %>%
    group_by(parameter_id, timeSeriesID, timestep) %>%
    summarise(S = n_distinct(species),
              S_PIE = vegan::diversity(N, index = 'invsimpson'),
              N_all = sum(N),
              C_hat = mobr::Chat(N, N_all)) %>%
    ungroup()
  
  comm_wide = comm_long %>%
    dplyr::select(-parameter_id, -timeSeriesID) %>%
    tidyr::pivot_wider(names_from = species,
                       values_from = N,
                       values_fill = 0)
  
  # betapart requires presence/absence matrix for Jaccard calculations of turnover/nestedness
  comm_wide_binary <- with(comm_wide[,-c(1,2)], ifelse(comm_wide[,-c(1,2)] > 0, 1, 0))
  
  # initialise matrix for storing all pairs
  yr_pairs = combn(unique(comm_long$timestep), 2)
  all_pairs = tibble(YEAR1 = yr_pairs[1,],
                     YEAR2 = yr_pairs[2,])
  
  # morisita-horn
  MH_dist <- as.matrix(vegan::vegdist(comm_wide[,-c(1,2)], method='horn'))
  
  # two steps for Jaccard components (so as calculation is done only once)
  J_components <-betapart:: beta.pair(comm_wide_binary, index.family='jaccard')	# distance
  Jbeta <- as.matrix(J_components$beta.jac)
  Jtu <- as.matrix(J_components$beta.jtu)
  Jne <- as.matrix(J_components$beta.jne)
  
  # want to keep all pairs
  all_pairs = all_pairs %>%
    mutate(Jbeta = t(Jbeta)[lower.tri(t(Jbeta))],
           Jtu = t(Jtu)[lower.tri(t(Jtu))],
           Jne = t(Jne)[lower.tri(t(Jne))],
           MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
           # put metadata back in
           parameter_id = unique(comm_long$parameter_id),
           timeSeriesID = unique(comm_long$timeSeriesID))
  
  beta_dist_100 = bind_rows(beta_dist_100, all_pairs)
  alpha_100 = bind_rows(alpha_100, alpha_temp)
}

# save(beta_dist_100,
#      file = Sys.getenv('OFILE'))


beta_dist_100 <- beta_dist_100 %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(cYear = YEAR2 - mean(YEAR2)) %>% 
  ungroup()

beta_dist_100 <- beta_dist_100 %>% 
  mutate(temp_dist = YEAR2 - YEAR1) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>% 
  ungroup()


setwd()
pdf('check_temp_dist.pdf')
ggplot() +
  geom_point(data = beta_dist_100,
             aes(x = c_temp_dist, y = Jbeta, colour = timeSeriesID)) +
  stat_smooth(data = beta_dist_100,
              aes(x = c_temp_dist, y = Jbeta, colour = timeSeriesID),
              method = 'lm', se = FALSE) +
  coord_cartesian(ylim = c(0,1)) +
  theme_bw() +
  theme(legend.position = 'none')
dev.off()