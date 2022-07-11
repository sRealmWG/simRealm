# calculate whittaker measures of turnover for the neutral simulations
options(tidyverse.quiet = TRUE)
library(tidyverse)
# get array job task id (use as parameter_id [PID])
PID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

setwd('/data/idiv_chase/simRealm/data/neutral-ts-v3-100/')
filelist = dir(pattern=paste0("parameter_id-", PID, "-"))

dat = tibble()
for(i in 1:length(filelist)){
  # first version of directed movement
  temp <- read_csv(paste0(filelist[i]), show_col_types = FALSE)
  
  dat = bind_rows(dat, temp)
}

dat <- dat %>% 
  group_by(parameter_id, timeSeriesID) %>% 
  nest(data = c(timestep, species, n)) %>% 
  ungroup()

# initialise storage
beta_Whittaker_100_allYears <- tibble() 
beta_Whittaker_100_yrPairs <- tibble() 


for(i in 1:nrow(dat)){
  # counter for sanity
  print(paste('calculation ', i, 'of ', nrow(dat)))
    # long data
  comm_long = dat %>% 
    slice(i) %>% 
    unnest(data) %>% 
    arrange(-desc(timestep))
  
  # first, Whittaker at the scale of the whole time series
  
  # we need the average richness at the alpha scale (i.e., grain of one year)
  alpha_bar_allYrs = comm_long %>% 
    group_by(parameter_id, timeSeriesID, timestep) %>% 
    summarise(S = length(unique(species)),
              S_PIE = vegan::diversity(n, index = 'invsimpson')) %>% 
    group_by(parameter_id, timeSeriesID) %>% 
    summarise(aS_bar = mean(S),
              aS_PIE_bar = mean(S_PIE)) %>% 
    ungroup()
  
  # and we need the gamma-scale, here that is all years combined (for a given time series)
  gamma_allYears = comm_long %>% 
    group_by(parameter_id, timeSeriesID, species) %>% 
    summarise(N = sum(n)) %>% 
    group_by(parameter_id, timeSeriesID) %>% 
    summarise(gS = length(unique(species)),
              gS_PIE = vegan::diversity(N, index = 'invsimpson')) %>% 
    ungroup()
  
  # betaC is done separately: first change data from long to wide
  comm_wide = comm_long %>% 
    dplyr::select(-parameter_id, -timeSeriesID) %>% 
    tidyr::pivot_wider(names_from = species,
                       values_from = n,
                       values_fill = 0)
  
  # we need to calculate a target coverage that we will interpolate to
  # this involves a couple of step (Engel et al. 2021), implemented in this function
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
  yr_pairs <- t(combn(unique(comm_long$timestep), 2))
  whittaker_temp = tibble()
  for(j in 1:nrow(yr_pairs)){
    # counter for sanity
    # print(paste('parameter_id ', i, 'of ', length(unique(neutral_ss100$parameter_id))))
    # print(paste(j, ' year pair of ', nrow(yr_pairs)))
    
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
                gS_PIE = vegan::diversity(N, index = 'invsimpson')) %>% 
      ungroup()
    
    Ctarget = betaC::C_target(years_wide[,-1], 
                              factor = 1)
    
    betaC_est = try(betaC::beta_C(x = years_wide[,-1],
                                  C = Ctarget,
                                  extrapolation = FALSE, interrupt = FALSE))
    
    
    
    alpha_bar = years %>% 
      group_by(parameter_id, timeSeriesID, timestep) %>% 
      summarise(aS = length(unique(species)),
                aS_PIE = vegan::diversity(n, index = 'invsimpson')) %>% 
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

# rename and save
setwd('/data/idiv_chase/simRealm/results/neutral/metric-ts/')
neutral_betaW_allYears_100 = beta_Whittaker_100_allYears
neutral_betaW_yrPairs_100 = beta_Whittaker_100_yrPairs
save(neutral_betaW_allYears_100,
     neutral_betaW_yrPairs_100,
     file = Sys.getenv('OFILE'))