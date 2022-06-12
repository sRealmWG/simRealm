# compile whittaker results from array job

rm(list=ls())

setwd('/data/idiv_chase/sablowes/simRealm/results/mobsim')

##	use a counter to get put all the files back together
parameter_id = 1:24

# initial storage
whittaker_allYrs_100 = data.frame()
whittaker_yrPairs_100 = data.frame()

ts_count = data.frame()
for(par_id in 1:length(parameter_id)){
  
  print(paste(par_id, 'of 24'))
  
  filelist = dir(pattern=paste0("mobsim-whittaker-parameter_id-", parameter_id[par_id], "-"))
  nSimul = length(filelist)
  
  # check: is there a file for every time series 
  ts_count = bind_rows(ts_count,
                       data.frame(parameter_id = parameter_id[par_id],
                        n_ts = nSimul))

  
  par_i_whittaker_allYrs_100 = data.frame()
  par_i_whittaker_yrPairs_100 = data.frame()
  
  for (iSimul in 1:nSimul) {
    load(filelist[iSimul])
    
    par_i_whittaker_allYrs_100 = rbind(par_i_whittaker_allYrs_100, beta_Whittaker_100_allYears)
    par_i_whittaker_yrPairs_100 = rbind(par_i_whittaker_yrPairs_100, beta_Whittaker_100_yrPairs)
    
    rm(beta_Whittaker_100_allYears, beta_Whittaker_100_yrPairs)
  }
  
  whittaker_allYrs_100 = rbind(whittaker_allYrs_100, par_i_whittaker_allYrs_100)
  whittaker_yrPairs_100 = rbind(whittaker_yrPairs_100, par_i_whittaker_yrPairs_100)
}
  
  
save(whittaker_allYrs_100, 
     whittaker_yrPairs_100, 
     ts_count, file = 'mobsim-whittaker-beta-time.Rdata')