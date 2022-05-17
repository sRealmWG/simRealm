library(tidyverse)
# get array job task id (use as parameter_id [PID])
PID = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

setwd('/data/idiv_chase/simRealm/data/neutral-ts-v2/')
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

neutral_beta_dist_100 <- tibble()
neutral_alpha_100 <- tibble()
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
              S_PIE = vegan::diversity(n, index = 'invsimpson'),
              N_all = sum(n),
              C_hat = mobr::Chat(n, N_all)) %>%
    ungroup()
  
  comm_wide = comm_long %>%
    dplyr::select(-parameter_id, -timeSeriesID) %>%
    tidyr::pivot_wider(names_from = species,
                       values_from = n,
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
  
  neutral_beta_dist_100 = bind_rows(neutral_beta_dist_100, all_pairs)
  neutral_alpha_100 = bind_rows(neutral_alpha_100, alpha_temp)
}

save(neutral_beta_dist_100,
     neutral_alpha_100,
     file = Sys.getenv('OFILE'))

