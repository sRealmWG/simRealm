# calculate whittaker measures of turnover for the neutral simulations

options(tidyverse.quiet = TRUE)
library(tidyverse)

load('~/Dropbox/1current/sRealm/simRealm/data/time_series/neutral_time_series.Rdata')
# load('~/Dropbox/1current/sRealm/local_data/timeSeries_site55_pid-25-48.Rdata')

# separate each of the subsamples and nest all time steps for a given timeSeriesID
neutral_ss100 <- neutral_local_ts %>% 
  unnest(ss100) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, n) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, n)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)

neutral_ss50 <- neutral_local_ts %>% 
  unnest(ss50) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)

neutral_ss10 <- neutral_local_ts %>% 
  unnest(ss10) %>% 
  select(parameter_id, timeSeriesID,  timestep, species, N) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, N)) %>% 
  ungroup() %>% 
  select(parameter_id, timeSeriesID,  data)

# initialise storage
beta_Whittaker_100_allYears <- tibble() 
beta_Whittaker_100_yrPairs <- tibble() 


for(i in 1:nrow(neutral_ss100)){
    # long data
  comm_long = neutral_ss100 %>% 
    slice(i) %>% 
    unnest(data) %>% 
    arrange(-desc(timestep))
  
  # first, Whittaker at the scale of the whole time series
  alpha_bar_allYrs = comm_long %>% 
    group_by(parameter_id, timeSeriesID, timestep) %>% 
    summarise(S = length(unique(species)),
              S_PIE = mobr::calc_SPIE(n)) %>% 
    group_by(parameter_id, timeSeriesID) %>% 
    summarise(aS_bar = mean(S),
              aS_PIE_bar = mean(S_PIE)) %>% 
    ungroup()
  
  gamma_allYears = comm_long %>% 
    group_by(parameter_id, timeSeriesID, species) %>% 
    summarise(N = sum(n)) %>% 
    group_by(parameter_id, timeSeriesID) %>% 
    summarise(gS = length(unique(species)),
              gS_PIE = mobr::calc_SPIE(N)) %>% 
    ungroup()
  
  comm_wide = comm_long %>% 
    dplyr::select(-parameter_id, -timeSeriesID) %>% 
    tidyr::pivot_wider(names_from = species,
                       values_from = n,
                       values_fill = 0)
  
  Ctarget_allYrs = betaC::C_target(comm_wide[,-1], 
                            factor = 1)
  
  betaC_est_allYrs = try(betaC::beta_C(x = comm_wide[,-1],
                                C = Ctarget,
                                extrapolation = FALSE, interrupt = FALSE))
  
  beta_allYears_temp = left_join(alpha_bar, gamma_allYears) %>% 
    mutate(beta_S = gS / aS_bar,
           beta_S_PIE = gS_PIE / aS_PIE_bar,
           beta_C = ifelse(class(betaC_est_allYrs)!='numeric', yes = NA, no = betaC_est_allYrs[[1]]),
           Ctarget = Ctarget_allYrs,
           Ntarget = ifelse(class(betaC_est_allYrs)!='numeric', yes = NA, no = attributes(betaC_est_allYrs)$N))
  
  beta_Whittaker_100_allYears = bind_rows(beta_Whittaker_100_allYears, 
                                          beta_allYears_temp)
  
  # now, do for all year pairs
  yr_pairs <- t(combn(unique(comm_long$timestep), 2))
  whittaker_temp = tibble()
  for(j in 1:nrow(yr_pairs)){
    # counter for sanity
    print(paste('calculation ', i, 'of ', nrow(neutral_ss100)))
    print(paste(j, ' year pair of ', nrow(yr_pairs)))
    
    get_years = yr_pairs[j,]
    
    years = comm_long %>% 
      filter(timestep %in% get_years)
    
    years_wide = years %>% 
      dplyr::select(-parameter_id, -timeSeriesID) %>% 
      tidyr::pivot_wider(names_from = species,
                         values_from = n,
                         values_fill = 0)
    
    gamma_pair = years %>% 
      group_by(parameter_id, timeSeriesID, species) %>% 
      summarise(N = sum(n)) %>% 
      group_by(parameter_id, timeSeriesID) %>% 
      summarise(gS = length(unique(species)),
                gS_PIE = mobr::calc_SPIE(N)) %>% 
      ungroup()
    
    Ctarget = betaC::C_target(years_wide[,-1], 
                              factor = 1)
    
    betaC_est = try(betaC::beta_C(x = years_wide[,-1],
                                  C = Ctarget,
                                  extrapolation = FALSE, interrupt = FALSE))
    
    
    
    alpha_bar = years %>% 
      group_by(parameter_id, timeSeriesID, timestep) %>% 
      summarise(aS = length(unique(species)),
                aS_PIE = mobr::calc_SPIE(n)) %>% 
      group_by(parameter_id, timeSeriesID) %>% 
      summarise(aS_bar = mean(aS),
             aS_PIE_bar = mean(aS_PIE)) %>% 
      ungroup() 
    
    
    
    temp = left_join(alpha_bar, gamma_pair) %>% 
      mutate(YEAR1 = yr_pairs[j,1],
             YEAR2 = yr_pairs[j,2],
             beta_S = gS / aS_bar,
             beta_S_PIE = gS_PIE / aS_PIE_bar,
             beta_C = ifelse(class(betaC_est)!='numeric', yes = NA, no = betaC_est[[1]]),
             Ctarget = Ctarget,
             Ntarget = ifelse(class(betaC_est)!='numeric', yes = NA, no = attributes(betaC_est)$N))
    
    whittaker_temp = bind_rows(whittaker_temp, temp)  
  }
  beta_Whittaker_100_yrPairs = bind_rows(beta_Whittaker_100_yrPairs,
                                         whittaker_temp)
}
