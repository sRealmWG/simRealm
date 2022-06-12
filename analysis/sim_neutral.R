library(untb)
library(data.table)

## Parameter values ----
### Community
source("./analysis/parameters/community_neutral_v3.r") # loads parameter_table
outname <- 'v3'


## Functions ----
# convert untb format to long format used for analysis
# x is the output from untb (a matrix)
tolong <- function(x, parameter_id = 1){
  if(is.null(nrow(x))){
    ngen <- 1
  } else {
    ngen <- nrow(x)
  }

  for(i in 1:ngen){ # for each generation
    if(ngen==1){
      thisrle <- rle(sort(x)) # count up how many of each spp
      temp <- data.table(timestep = i, species = thisrle$value, n = thisrle$lengths) # data for this generation
    } else {
      thisrle <- rle(sort(x[i,])) # count up how many of each spp
      temp <- data.table(parameter_id = parameter_id, timestep = i, species = thisrle$value, n = thisrle$lengths)
    }
    if(i == 1){
      out <- temp
    } else {
      out <- rbind(out, temp) # add this generation onto the output
    }
  }
  return(out)
}



## Neutral simulations ----
# Each replicate has a burnin and then a simulation period
set.seed(parameter_table$SEED[1]) # set the seed for the random number generator
print(nrow(parameter_table))
for(i in 1:nrow(parameter_table)){
  cat(i)
  nburn <- parameter_table$NBURN[i]*parameter_table$N[i] # number of burnin timesteps
  nstep <- parameter_table$NSAMPS[i]*round(parameter_table$STEPL[i]*parameter_table$N[i]) # number of simulation
  meta <- rand.neutral(100000, parameter_table$THETA[i])
  simburnin <- untb(start = isolate(meta, size=parameter_table$N[i], replace=TRUE),
                    prob = parameter_table$M[i],
                    D = 1,
                    gens = nburn,
                    keep = TRUE,
                    meta = meta) # burnin
  simsamps <- untb(start = simburnin[nburn,],
                   prob = parameter_table$M[i],
                   D=1,
                   gens = nstep,
                   keep = TRUE,
                   meta = meta) # simulation

  # plot an example
  # plot(as.count(meta), main = 'Regional SAD')
  # plot(species.count(simburnin), type = 'b', main = 'Burnin: num spp through time') # evaluate burnin
  # matplot(species.table(simburnin), type='l', lty=1, main = 'Burnin: spp dynamics')
  # plot(count(simburnin[1,]), main = 'Burnin: Initial local SAD')
  # plot(count(simburnin[nburn,]), main = 'Burnin: Final local SAD')
  # plot(species.count(simsamps), type = 'b', main = 'Sim: num spp through time')
  # matplot(species.table(simsamps), type='l', lty=1, main = 'Sim: spp dynamics')
  # plot(count(simsamps[nstep,]), main = 'Sim: Final local SAD')

  # make output to save
  simkeep <- simsamps[seq(from=1,
                          by=round(parameter_table$STEPL[i]*parameter_table$N[i]),
                          length.out = parameter_table$NSAMPS[i]),] # keep a subset
  temp <- tolong(simkeep, parameter_id = parameter_table$parameter_id[i]) # convert to long format

  if(i == 1){
    out <- temp
  } else {
    out <- rbind(out, temp)
  }

}

nrow(out)

## Simulation metadata ----
metadata <- data.table::as.data.table(parameter_table)
metadata[, unique_id := outname]
metadata[, date := Sys.time()]
metadata[, simulation_function := "sim_neutral.R"]
metadata[, untb_version := as.character(utils::packageVersion("untb"))]
data.table::setcolorder(metadata, neworder = c("unique_id","parameter_id", "simulation_function", "untb_version", "date"))

# Saving results and metadata ----
data.table::fwrite(out, file = paste0("./data/simulations/neutral_sim_", outname, ".csv"))
data.table::fwrite(metadata, file = paste0("./data/simulations/neutral_metadata", outname, ".csv"))
