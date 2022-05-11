# calculate distance metrics for the neutral simulations

library(tidyverse)

# load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/neutral_time_series.Rdata')
# load('~/Dropbox/1current/sRealm/simRealm/simRealm/data/time_series/neutral_time_series_v2.Rdata')

# separate each of the subsamples and nest all time steps for a given timeSeriesID
neutral_ss75 <- neutral_local_ts %>% 
  unnest(ss75) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)

neutral_ss25 <- neutral_local_ts %>% 
  unnest(ss25) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)


# initialise storage
neutral_alpha_scale_75 <- tibble()
beta_dist_75 <- tibble() # dissimilarity

for(i in 1:nrow(neutral_ss75)){
  # counter for sanity
  print(paste('calculation ', i, 'of ', nrow(neutral_ss75)))
  
  # long data
  comm_long = neutral_ss75 %>% 
    slice(i) %>% 
    unnest(data) %>% 
    arrange(-desc(timestep))
  
  alpha_temp <- comm_long %>% 
    group_by(parameter_id, timeSeriesID, timestep) %>% 
    summarise(S = length(unique(species)),
              S_PIE = vegan::diversity(N, 'invsimpson'),
              N_all = sum(N),
              C_hat = mobr::Chat(N, N_all)) %>% 
    ungroup()
  
  comm_wide = comm_long %>%
    dplyr::select(-parameter_id, -timeSeriesID) %>%
    tidyr::pivot_wider(names_from = species,
                       values_from = N,
                       values_fill = 0)
  
  # betapart requires presence/absence matrix for Jaccard calculations of turnover/nestedness
  comm_wide_binary <- with(comm_wide[,-1], ifelse(comm_wide[,-1] > 0, 1, 0))
  
  # initialise matrix for storing all pairs
  yr_pairs = combn(unique(comm_long$timestep), 2)
  all_pairs = tibble(YEAR1 = yr_pairs[1,],
                     YEAR2 = yr_pairs[2,])
  
  # morisita-horn
  MH_dist <- as.matrix(vegan::vegdist(comm_wide[,-1], method='horn'))
  
  # two steps for Jaccard components (so as calculation is done only once)
  J_components <-betapart:: beta.pair(comm_wide_binary, index.family='jaccard')	# distance
  Jbeta <- as.matrix(J_components$beta.jac)
  Jtu <- as.matrix(J_components$beta.jtu)
  Jne <- as.matrix(J_components$beta.jne)
  
  # want to keep all pairs
  all_pairs = all_pairs %>%
    mutate(Jbeta = t(Jbeta)[lower.tri(t(Jbeta))], # and you say upper.tri
           Jtu = t(Jtu)[lower.tri(t(Jtu))],
           Jne = t(Jne)[lower.tri(t(Jne))],
           MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
           # put metadata back in
           parameter_id = unique(comm_long$parameter_id),
           timeSeriesID = unique(comm_long$timeSeriesID))
  
  beta_dist_75 = bind_rows(beta_dist_75, all_pairs)
  neutral_alpha_scale_75 = bind_rows(neutral_alpha_scale_75, alpha_temp)
}

# repeat for 50 subsample
neutral_alpha_scale_25 <- tibble()
beta_dist_25 <- tibble()
for(i in 1:nrow(neutral_ss25)){
  # counter for sanity
  print(paste('calculation ', i, 'of ', nrow(neutral_ss25)))
  
  # long data
  comm_long = neutral_ss25 %>% 
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
  comm_wide_binary <- with(comm_wide[,-1], ifelse(comm_wide[,-1] > 0, 1, 0))
  
  # initialise matrix for storing all pairs
  yr_pairs = combn(unique(comm_long$timestep), 2)
  all_pairs = tibble(YEAR1 = yr_pairs[1,],
                     YEAR2 = yr_pairs[2,])
  
  # morisita-horn
  MH_dist <- as.matrix(vegan::vegdist(comm_wide[,-1], method='horn'))
  
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
  
  beta_dist_25 = bind_rows(beta_dist_25, all_pairs)
  neutral_alpha_scale_25 = bind_rows(neutral_alpha_scale_25, alpha_temp)
}


beta_dist_25 <- beta_dist_25 %>% 
  mutate(temp_dist = YEAR2 - YEAR1)

beta_dist_75 <- beta_dist_75 %>% 
  mutate(temp_dist = YEAR2 - YEAR1)


save(neutral_alpha_scale_25,
     neutral_alpha_scale_75,
     file = '~/Dropbox/1current/sRealm/simRealm/simRealm/prelim_results/neutral_v2_local_metrics_25_75.Rdata')

write.csv(beta_dist_75, file = '~/Dropbox/1current/sRealm/local_data/neutral_v2_beta_dist_75.csv')
write.csv(beta_dist_25, file = '~/Dropbox/1current/sRealm/local_data/neutral_v2_beta_dist_25.csv')

