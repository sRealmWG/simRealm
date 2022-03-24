library(tidyverse)

task = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
input_file = paste0('/data/idiv_chase/sablowes/simRealm/data/neutral-ts/parameter_id-1-ts', task,'.csv')
input_file = paste0('~/Dropbox/1current/sRealm/local_data/neutral-ts/parameter_id-1-ts', 2, '.csv')
dat = read_csv(input_file)

# initialise storage
beta_Whittaker_100_allYears <- tibble() 
beta_Whittaker_100_yrPairs <- tibble() 



# first, Whittaker at the scale of the whole time series
alpha_bar_allYrs = dat %>% 
  group_by(parameter_id, timeSeriesID, timestep) %>% 
  summarise(S = length(unique(species)),
            S_PIE = mobr::calc_SPIE(n)) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  summarise(aS_bar = mean(S),
            aS_PIE_bar = mean(S_PIE)) %>% 
  ungroup()

gamma_allYears = dat %>% 
  group_by(parameter_id, timeSeriesID, species) %>% 
  summarise(N = sum(n)) %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  summarise(gS = length(unique(species)),
            gS_PIE = mobr::calc_SPIE(N)) %>% 
  ungroup()

comm_wide = dat %>% 
  dplyr::select(-parameter_id, -timeSeriesID) %>% 
  tidyr::pivot_wider(names_from = species,
                     values_from = n,
                     values_fill = 0)

Ctarget_allYrs = betaC::C_target(comm_wide[,-1], 
                                 factor = 1)

betaC_est_allYrs = try(betaC::beta_C(x = comm_wide[,-1],
                                     C = Ctarget_allYrs,
                                     extrapolation = FALSE, interrupt = FALSE))

beta_allYears_temp = left_join(alpha_bar_allYrs, gamma_allYears) %>% 
  mutate(beta_S = gS / aS_bar,
         beta_S_PIE = gS_PIE / aS_PIE_bar,
         beta_C = ifelse(class(betaC_est_allYrs)!='numeric', yes = NA, no = betaC_est_allYrs[[1]]),
         Ctarget = Ctarget_allYrs,
         Ntarget = ifelse(class(betaC_est_allYrs)!='numeric', yes = NA, no = attributes(betaC_est_allYrs)$N))

beta_Whittaker_100_allYears = bind_rows(beta_Whittaker_100_allYears, 
                                        beta_allYears_temp)
  
# now, do for all year pairs
yr_pairs <- t(combn(unique(dat$timestep), 2))
whittaker_temp = tibble()
for(j in 1:nrow(yr_pairs)){
    # counter for sanity
    # print(paste('parameter_id ', i, 'of ', length(unique(neutral_ss100$parameter_id))))
    # print(paste(j, ' year pair of ', nrow(yr_pairs)))
    
    get_years = yr_pairs[j,]
    
    years = dat %>% 
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

save(beta_Whittaker_100_allYears,
     beta_Whittaker_100_yrPairs, file = Sys.getenv('OFILE'))