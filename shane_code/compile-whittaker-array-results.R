# compile whittaker results from array job

rm(list=ls())

setwd('/data/idiv_chase/sablowes/simRealm/results/neutral')
##	set the pattern to load the files to be compiled
parameter_id = 1:12


for(par_id in 1:length(parameter_id){
  filelist = dir(pattern=paste0("netural-whittaker-parameter_id-", parameter_id[par_id])
  nSimul = length(filelist)
  
  # check: is there a file for every time series (might also have doubled up on parameter_id's??)
  ts_count = data.frame(parameter_id = parameter_id[par_id],
                        nSimul)

  
  for (iSimul in 1:nSimul) {
    load(filelist[iSimul])
    
    whittaker_allYrs_100 = rbind(whittaker_allYrs, beta_Whittaker_100_allYears)
    whittaker_yrPairs_100 = rbind(whittaker_allYrs, beta_Whittaker_100_yrPairs)
    
    rm(beta_Whittaker_100_allYears, beta_Whittaker_100_yrPairs)
  }
  
save(whittaker_allYrs_100, 
     whittaker_yrPairs_100, 
     ts_check, file = Sys.getenv('OFILE'))