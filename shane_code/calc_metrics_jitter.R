# inspection of simulated data for sRealm workshop 1

library(tidyverse)


load('~/Dropbox/1current/sRealm/simRealm/data/time_series/timeSeries_site55_pid-1-24.Rdata')
# load('~/Dropbox/1current/sRealm/local_data/timeSeries_site55_pid-25-48.Rdata')

# separate each of the subsamples and nest all time steps for a given timeSeriesID
ss100_s55 <- site55 %>% 
  unnest(ss100) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)

ss50_s55 <- site55 %>% 
  unnest(ss50) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)

ss10_s55 <- site55 %>% 
  unnest(ss10) %>% 
  select(parameter_id, timeSeriesID, quadrat_id, timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID, quadrat_id) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID, quadrat_id, data)

# initialise storage
alpha_scale_100 <- tibble()
beta_dist_100 <- tibble() # dissimilarity

for(i in 1:nrow(ss100_s55)){
  # counter for sanity
  print(paste('calculation ', i, 'of ', nrow(ss100_s55)))
  
  # long data
  comm_long = ss100_s55 %>% 
    slice(i) %>% 
    unnest(data) %>% 
    arrange(-desc(timestep))
  
  alpha_temp <- comm_long %>% 
    group_by(parameter_id, timeSeriesID, timestep) %>% 
    summarise(S = n_distinct(species),
              S_PIE = mobr::calc_SPIE(N),
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
  alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}

# repeat for 50 subsample
alpha_scale_50 <- tibble()
beta_dist_50 <- tibble() 
for(i in 1:nrow(ss50_s55)){
  # counter for sanity
  print(paste('calculation ', i, 'of ', nrow(ss50_s55)))
  
  # long data
  comm_long = ss50_s55 %>% 
    slice(i) %>% 
    unnest(data) %>% 
    arrange(-desc(timestep))
  
  alpha_temp <- comm_long %>% 
    group_by(parameter_id, timeSeriesID, timestep) %>% 
    summarise(S = n_distinct(species),
              S_PIE = mobr::calc_SPIE(N),
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
  
  beta_dist_50 = bind_rows(beta_dist_50, all_pairs)
  alpha_scale_50 = bind_rows(alpha_scale_50, alpha_temp)
}


# repeat for 10% subsample
alpha_scale_10 <- tibble()
beta_dist_10 <- tibble() 
for(i in 1:nrow(ss10_s55)){
  # counter for sanity
  print(paste('calculation ', i, 'of ', nrow(ss10_s55)))
  
  # long data
  comm_long = ss10_s55 %>% 
    slice(i) %>% 
    unnest(data) %>% 
    arrange(-desc(timestep))
  
  alpha_temp <- comm_long %>% 
    group_by(parameter_id, timeSeriesID, timestep) %>% 
    summarise(S = n_distinct(species),
              S_PIE = mobr::calc_SPIE(N),
              N_all = sum(N),
              C_hat = mobr::Chat(N, N_all)) %>% 
    ungroup()
  
  # initialise matrix for storing all pairs
  if(length(unique(comm_long$timestep)) == 1){
  all_pairs = tibble(YEAR1 = NULL,
                     YEAR2 = NULL)
  }
  
  if(length(unique(comm_long$timestep)) == 1){
    all_pairs = all_pairs %>% 
      mutate(Jbeta = NULL,
             Jtu = NULL,
             Jne = NULL,
             MH_dist = NULL,
             # put metadata back in
             parameter_id = unique(comm_long$parameter_id),
             timeSeriesID = unique(comm_long$timeSeriesID))
  }
  
  if(length(unique(comm_long$timestep)) > 1){
    
    # initialise matrix for storing all pairs
    yr_pairs = combn(unique(comm_long$timestep), 2)
    all_pairs = tibble(YEAR1 = yr_pairs[1,],
                       YEAR2 = yr_pairs[2,])
    
    comm_wide = comm_long %>% 
    dplyr::select(-parameter_id, -timeSeriesID) %>% 
    tidyr::pivot_wider(names_from = species,
                       values_from = N,
                       values_fill = 0)
  
    # betapart requires presence/absence matrix for Jaccard calculations of turnover/nestedness
    comm_wide_binary <- with(comm_wide[,-c(1,2)], ifelse(comm_wide[,-c(1,2)] > 0, 1, 0))  
  
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
    
  }
  beta_dist_10 = bind_rows(beta_dist_10, all_pairs)
  alpha_scale_10 = bind_rows(alpha_scale_10, alpha_temp)
}


beta_dist_10 <- beta_dist_10 %>% 
  mutate(temp_dist = YEAR2 - YEAR1)

beta_dist_50 <- beta_dist_10 %>% 
  mutate(temp_dist = YEAR2 - YEAR1)

beta_dist_100 <- beta_dist_10 %>% 
  mutate(temp_dist = YEAR2 - YEAR1)

save(alpha_scale_10,
     alpha_scale_50,
     alpha_scale_100,
     beta_dist_10,
     beta_dist_50,
     beta_dist_100,
     file = '~/Dropbox/1current/sRealm/simRealm/prelim_results/mob_sim_local_metrics_alpha_beta_diss_pid_1-24.Rdata')
