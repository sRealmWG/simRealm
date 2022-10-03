###Laura 06-04-2022
###updated 17-05-2022
###updated 10-08-2022 and 29-08-2022


##select appropriate time series from RivFishTIME for case study #####################

## Steps are:
## 1. filtering data from USA

## 2. following initial selection from Shane to have:
##       minimum of 4 unique locations for each SourceID
##       same/unique sampling protocol (the one with most data)
##       same Quarter of the year (one with the most years sampled)
##       keep only one survey per year for each time series

## 3. select sources with at least 2 time points and 2 species

## 4. cherry-pick a few studies to exemplify the aspects of interest -> variation in duration and in number of species


##from Shane's script:
# "To standardise extent for SourceID, this script:
# 1. counts number of unique locations within SourceID's (want at least 4)
# 2. finds the Protocol (i.e., sampling methodology) with the most samples
# 3. Identifies the Quarter (of the year) with the most years sampled
# 4. identifies the two years within each region (with at least 10 years duration expired between samples) for which we get the greatest number of sites"
#(NOT doing #4)

##[[and from Wubing??]]
##i.e. also matching locations across years to keep locations in similar configuration across years??
##NOT doing this extra filtering for now



library(tidyverse)

##first setwd to project directory
setwd('~/Dropbox/1current/sRealm/simRealm/simRealm/')
##import data and metadata
ft <- read_csv("case_study/RivFishTIME/1873_10_1873_2_RivFishTIME_SurveyTable.csv")
meta <- read_csv("case_study/RivFishTIME/1873_10_1873_2_RivFishTIME_TimeseriesTable.csv")[-13]



##1. filter USA data and sources with 4 or more sites
study_USA_4loc<- meta %>%
   filter(Country == "USA") %>%
   group_by(SourceID) %>%
   summarise(n_loc = n_distinct(TimeSeriesID),
             n_protocol = n_distinct(Protocol)) %>%
   ungroup() %>%
   filter(n_loc > 3)

##20 sources


##2. check if studies have different protocols
study_mp <- study_USA_4loc  %>%
   filter(n_protocol > 1)  ##5 studies with more than 1 protocol


##Choose the protocol with the maximum number of time series
study_mp_selected <- meta %>%
   filter(SourceID %in% pull(study_mp, SourceID)) %>%
   group_by(SourceID, Protocol) %>%
   summarise(n_loc = n_distinct(TimeSeriesID)) %>%
   filter(n_loc > 3) %>%
   group_by(SourceID) %>%
   filter(n_loc == max(n_loc)) %>%
   ungroup() %>%
   distinct(SourceID, .keep_all = TRUE) #one study has 2 protocols with the same number of time series -> Keep only one of them.


##3. get the time series in the studies with at least 4 time series in the same protocol
meta_4loc_USA <- bind_rows(meta %>% filter(SourceID %in%
                                              (study_USA_4loc  %>% filter(n_protocol == 1) %>% pull(SourceID))),
                           meta %>% inner_join(study_mp_selected %>% dplyr::select(-n_loc)))


n_distinct(meta_4loc_USA$SourceID)
n_distinct(meta_4loc_USA$TimeSeriesID)
n_distinct(meta_4loc_USA$Region)
n_distinct(meta_4loc_USA$Waterbody)

##20 sources with 1040 time-series/sites with a single protocol
##31 regions and 644 water bodies



##mapping locations
USAmap <- map_data("state")

ggplot() +
   geom_polygon(data=USAmap, aes(long, lat, group = group), fill="gray60", size=0, alpha=0.6) +
   geom_point(data= meta_4loc_USA,
              aes(x=Longitude, y=Latitude, colour= Region), alpha= 0.7) +
   # geom_point(data= meta_4loc_USA,
   #            aes(x=Longitude, y=Latitude), alpha= 0.7, shape=1, color="black") +
   # scale_size(range=c(1, 5)) +
   scale_colour_viridis_d() +
   theme_bw() +
   coord_cartesian() +
   theme(legend.position = "none")




# check number of sites and time series for each study, and whether they are equal
meta_4loc_USA %>%
   group_by(SourceID) %>%
   summarise(n_site = n_distinct(SiteID),
             n_ts = n_distinct(TimeSeriesID)) %>%
   filter(n_site != n_ts) # each time series has unique site


## ## ##

##4. get communities data for the selected time series
ft_4loc <- inner_join(ft, meta_4loc_USA, by = "TimeSeriesID") %>%
   relocate(SourceID)


##calculate number of species and years in each time series, and identify the "best quarter"
metainfo <- ft_4loc %>%
   group_by(SourceID, TimeSeriesID, Latitude, Longitude, HydroBasin) %>% ##was Region - replaced with HydroBasin (10-08-2022)
   summarise(n_sps= n_distinct(Species),
             n_years= n_distinct(Year),
             min_year= min(Year),
             max_year= max(Year),
             n_years_Q1 = n_distinct(Year[Quarter == 1]),
             n_years_Q2 = n_distinct(Year[Quarter == 2]),
             n_years_Q3 = n_distinct(Year[Quarter == 3]),
             n_years_Q4 = n_distinct(Year[Quarter == 4]))

metainfo$max_years_quater <- apply(metainfo[,10:13], 1, max)


# the quarter with the most years sampled
metainfo$best_quarter <- apply(metainfo[,10:13], 1, function(x) { which(x == max(x))[1]})
# table(metainfo$best_quarter)


##check max and min duration
min(metainfo$n_years)  ##2
max(metainfo$n_years)  ##31


##keep the years in the quarter with most surveys to control similar survey time through years for each time series
ft_4loc <- ft_4loc %>%
   inner_join(metainfo %>%
                 select(TimeSeriesID, n_sps, n_years, Quarter = best_quarter) %>%  ##keeping also info of n_sps and n_years
                 mutate(Quarter = as.character(Quarter)))


##sanity checks
n_distinct(ft_4loc$SourceID)
n_distinct(ft_4loc$TimeSeriesID)
n_distinct(ft_4loc$Region)
n_distinct(ft_4loc$Waterbody)

##added
n_distinct(ft_4loc$HydroBasin)  ##30


##5. check how many time series have different number of surveys across years
n_survey <- ft_4loc %>%
   # dplyr::select(-(n_site:duration)) %>%
   group_by(TimeSeriesID, Year) %>%
   summarise(n_survey = n_distinct(SurveyID)) %>%
   group_by(TimeSeriesID) %>%
   summarise(min_survey = min(n_survey),
             max_survey = max(n_survey))

# ~10% of time series have different number of surveys between years
table(n_survey$min_survey == n_survey$max_survey)
# only 2 time series have multiple surveys at all year
table(n_survey$min_survey > 1)

# keep one survey for each year of each time series
one_survey_perYear <- ft_4loc %>%
   dplyr::select(SourceID, TimeSeriesID, Year, SurveyID) %>%
   distinct(SourceID, TimeSeriesID, Year, .keep_all=TRUE)

ft_4loc_filtered <-  ft_4loc %>%
   inner_join(one_survey_perYear)


##!!have to recalculate basic metadata info (n_sps and n_years)
##because removing quarters means some years within timeseries will be excluded!!
metainfo2 <- ft_4loc_filtered %>%
   group_by(SourceID, TimeSeriesID, Latitude, Longitude, HydroBasin) %>%
   summarise(n_sps= n_distinct(Species),
             n_years= n_distinct(Year),
             min_year= min(Year),
             max_year= max(Year)) %>%

   ##removing time series with too few years and/or species
   filter(n_years>2 & n_sps>4)   ###decided adding this filter to see if the errors in beta calculations would be solved (10-08-2022)

###decided on 24-08 that to have slopes of change we ideally need at least 3 years right?
##this now reduces the number of time series to 657...

##this reduces from 1040 to 967 TimeSeriesID with more than one year and more than one sps (10-08-22)
##having min of 5 sps further reduces to 891 time series
##having min of 3 years further reduces to 657 (24-08-22)

##check max and min duration
min(metainfo2$n_years)  ##3
max(metainfo2$n_years)  ##31

##check max and min richness
min(metainfo2$n_sps)  ##5
max(metainfo2$n_sps)  ##76



###AND NOW re-filter the raw data too!

ft_4loc_filtered1 <-  ft_4loc_filtered %>%
   filter(TimeSeriesID %in% metainfo2$TimeSeriesID)


##sanity checks
n_distinct(ft_4loc_filtered1$SourceID)       #19
n_distinct(ft_4loc_filtered1$TimeSeriesID)   #657
n_distinct(ft_4loc_filtered1$Region)         #31
#n_distinct(ft_4loc_filtered1$Waterbody)     #434
n_distinct(ft_4loc_filtered1$HydroBasin)     #26



##more informative map
ggplot() +
   geom_polygon(data=USAmap, aes(long, lat, group = group), fill="gray60", size=0, alpha=0.6) +
   geom_point(data= metainfo2,
              aes(x=Longitude, y=Latitude, size= n_years, color=n_sps), alpha= 0.8) +
   # scale_size(range=c(2, 5)) +
   scale_colour_viridis_c() +
   theme_bw() +
   coord_cartesian() +
   theme(legend.position = "bottom")


##and plotting with info for the basins
ggplot() +
   geom_polygon(data=USAmap, aes(long, lat, group = group), fill="gray60", size=0, alpha=0.6) +
   geom_point(data= metainfo2,
              aes(x=Longitude, y=Latitude, size= n_sps, color=as.factor(HydroBasin)), alpha= 0.6) +
   # scale_size(range=c(2, 5)) +
   scale_colour_viridis_d(option = "inferno") +
   theme_bw() +
   coord_cartesian() +
   theme(legend.position = "none")




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###




## ## ## ##  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
###10-08-2022
## calculate regional species number ####
## can focus on the hydrographical basins right?

##calculating number of sps per basin from the unfiltered original dataset

regionalpool <- ft %>%
   inner_join(., meta, by = "TimeSeriesID") %>%
   group_by(HydroBasin) %>%
   summarise(sps_pool = n_distinct(Species))


##and add this info per basin to the filtered data
ft_4loc_filtered1 <- ft_4loc_filtered1 %>%
   left_join(., regionalpool, by="HydroBasin")





## ## ## ##  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## calculate beta metrics  #####################

data_for_beta <- ft_4loc_filtered1 %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, data)


##to save results
beta_metrics1 <- tibble() # dissimilarity


for(i in 1:nrow(data_for_beta)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(data_for_beta)))

   # long data
   comm_long = data_for_beta %>%
      slice(i) %>%
      unnest(data) %>%
      arrange(-desc(Year))

   # alpha_temp <- comm_long %>%
   #   group_by(SourceID, TimeSeriesID, Year) %>%
   #   summarise(S = n_distinct(Species),
   #             S_PIE = vegan::diversity(Abundance, index = 'invsimpson'),
   #             N_all = sum(Abundance),
   #             C_hat = mobr::Chat(Abundance, N_all)) %>%
   #   ungroup()

   comm_wide = comm_long %>%
      dplyr::select(-SourceID, -TimeSeriesID) %>%
      tidyr::pivot_wider(names_from = Species,
                         values_from = Abundance,
                         values_fill = 0)

   # betapart requires presence/absence matrix for Jaccard calculations of turnover/nestedness
   comm_wide_binary <- with(comm_wide[,-c(1,2)], ifelse(comm_wide[,-c(1,2)] > 0, 1, 0))

   # initialise matrix for storing all pairs
   yr_pairs = combn(unique(comm_long$Year), 2)
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
             Jtu = t(Jne)[lower.tri(t(Jne))],
             MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
             # put metadata back in
             SourceID = unique(comm_long$SourceID),
             TimeSeriesID = unique(comm_long$TimeSeriesID))

   beta_metrics1 = bind_rows(beta_metrics1, all_pairs)
   # alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}

##some warning messages re "horn":
##"In vegan::vegdist(comm_wide[, -c(1, 2)], method = "horn") :you have empty rows: their dissimilarities may be meaningless in method “horn”")
aux_horn<- beta_metrics1[is.na(beta_metrics1$MH_dist),]
n_distinct(aux_horn$TimeSeriesID)  ##5 time series with Jtu and MH_dist = NaN


# beta_metrics1 <- beta_metrics1 %>%
#    mutate(temp_dist = YEAR2 - YEAR1) %>%
#
#    ##adding the initial info
#    inner_join(., select(metainfo2, TimeSeriesID:n_years), by="TimeSeriesID") %>%
#
#    select(-SourceID.y) %>%
#    rename(., SourceID = SourceID.x)



## ## ## ##  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##calculate slopes #####################

##from Shane's script "fit-linear-models-array-25-allYrs"
##not calculating betareg slopes (10-08-2022)

# fit linear models to turnover (dissimilarity)
allYrs_fish <- data.frame(beta_metrics1) %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%
   group_by(SourceID, TimeSeriesID) %>%
   mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, c_temp_dist, temp_dist, Jbeta, MH_dist) %>%
   nest(data = c(c_temp_dist, temp_dist, Jbeta, MH_dist)) %>%
   mutate(Jac_lm_allYrs = map(data, ~lm(.x$Jbeta ~ .x$c_temp_dist)),
          mean_Jac = map(data, ~mean(.x$Jbeta)),
          mh_lm_allYrs = map(data, ~lm(.x$MH_dist ~ .x$c_temp_dist)),
          mean_mh = map(data, ~mean(.x$MH_dist))) %>%
          #Jac_glm_allYrs = map(data, possibly(~betareg::betareg(.x$Jbeta ~ .x$c_temp_dist), otherwise = NULL)),
          #mh_glm_allYrs = map(data, possibly(~betareg::betareg(.x$MH_dist ~ .x$c_temp_dist), otherwise = NULL))) %>%
   mutate(Jac_lm_allYrs_tidy = map(Jac_lm_allYrs, broom::tidy),
          MH_lm_allYrs_tidy = map(mh_lm_allYrs, broom::tidy)) %>%
          # Jac_glm_allYrs_tidy = map(Jac_glm_allYrs, possibly(broom::tidy, otherwise = NULL)),
          # MH_glm_allYrs_tidy = map(mh_glm_allYrs, possibly(broom::tidy, otherwise = NULL))) %>%
   ungroup()

##to calculate max "distance" between years in each time series
ts_max<- data.frame(beta_metrics1) %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%
   group_by(SourceID, TimeSeriesID) %>%
   mutate(ts_max = max(temp_dist,na.rm=T)) %>%
   ungroup() %>%
   select("SourceID", "TimeSeriesID", "ts_max") %>%
   distinct()



##extracting lm coefs for Jaccard and Morisita
jac_horn_coefs <- left_join(allYrs_fish %>%
                               unnest(Jac_lm_allYrs_tidy) %>%
                               rename(jac_estimate = estimate) %>%
                               dplyr::select(SourceID, TimeSeriesID, term, jac_estimate),
                            allYrs_fish %>%
                               unnest(MH_lm_allYrs_tidy) %>%
                               rename(mh_estimate = estimate) %>%
                               dplyr::select(SourceID, TimeSeriesID, term, mh_estimate)) %>%

   ##adding mean jac and horn values too
   left_join(allYrs_fish %>%
                unnest(c(mean_Jac, mean_mh)) %>%
                select(mean_Jac, mean_mh, TimeSeriesID), by= "TimeSeriesID") %>%

   ##adding back info re n_sps, duration etc
   left_join(metainfo2, by= c("SourceID", "TimeSeriesID")) %>%

   ##adding regional pool info
   left_join(regionalpool, by= "HydroBasin") %>%

   ##adding calculation of time series length
   left_join(ts_max, by= c("SourceID", "TimeSeriesID")) %>%
   ungroup()




### ### ### ###
##plotting Jaccard slopes and mean
jac_horn_coefs %>%
   filter(term=='.x$c_temp_dist') %>%
   ggplot() +
   geom_point(aes(x = n_years, y = jac_estimate, color=n_sps, size=n_sps), alpha= 0.85) +
   scale_color_viridis_c() +
   stat_smooth(aes(x = n_years, y = jac_estimate), se = F, size = 1) +
   labs(x = 'n_years',
        y = 'Jaccard slope (rate of turnover)',
        subtitle = 'All years comparison') +
   #geom_hline(yintercept = 0, lty=2) +
   theme_minimal()



jac_horn_coefs %>%
   filter(term=='.x$c_temp_dist') %>%
   ggplot() +
   geom_point(aes(x = n_years, y = mean_Jac, color=n_sps, size=n_sps), alpha= 0.85) +
   scale_color_viridis_c() +
   stat_smooth(aes(x = n_years, y = mean_Jac), se = F, size = 1) +
   labs(x = 'n_years',
        y = 'Mean Jaccard',
        subtitle = 'All years comparison') +
   theme_minimal()


###trying to mimic Shane's plots
linear_col = '#bc5090'
affinity_col = '#ffa600'
mean_col = '#003f5c'



##time series duration
jac_horn_coefs %>%
   filter(term=='.x$c_temp_dist') %>%
   ggplot() +
   # geom_point(aes(x = n_years, y = mean_Jac, color=n_sps, size=n_sps), alpha= 0.85) +
   stat_smooth(aes(x = n_years, y = jac_estimate), color= "#bc5090", 
               method = "gam", se = TRUE, size = 0.5) +
   labs(x = 'n_years',
        y = 'Jaccard slope (rate of turnover)',
        subtitle = 'All years comparison') +
   theme_minimal()

##plot looks quite different, change in axis range etc


jac_horn_coefs %>%
   filter(term=='.x$c_temp_dist') %>%
   ggplot() +
   #geom_point(aes(x = n_years, y = jac_estimate, color=n_sps, size=n_sps), alpha= 0.85) +
   stat_smooth(aes(x = n_years, y = jac_estimate, color= "linear_col"), size = 2, method = "lm") +
   stat_smooth(aes(x = n_years, y = mean_Jac/10, color="mean_col"), size = 2, method = "lm") +
   labs(x = 'n_years',
        y = 'Jaccard slope (rate of turnover)',
        subtitle = 'All years comparison') +
   theme_minimal() +
   scale_y_continuous(name = 'Jaccard slope (rate of turnover)',
                      sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                          name = 'Mean Jaccard (all year pairs)')) +
   scale_colour_manual(values = c('linear_col' = linear_col,
                                  'affinity_col' = affinity_col,
                                  'mean_col' = mean_col),
                       guide = 'none')

##sps pool
jac_horn_coefs %>%
   filter(term=='.x$c_temp_dist') %>%
   ggplot() +
   stat_smooth(aes(x = sps_pool, y = jac_estimate, color= "linear_col"), size = 2, method = "lm") +
   stat_smooth(aes(x = sps_pool, y = mean_Jac/10, color="mean_col"), size = 2, method = "lm") +
   labs(x = 'sps_pool',
        y = 'Jaccard slope (rate of turnover)',
        subtitle = 'All years comparison') +
   theme_minimal() +
   scale_y_continuous(name = 'Jaccard slope (rate of turnover)',
                      sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                          name = 'Mean Jaccard (all year pairs)')) +
   scale_colour_manual(values = c('linear_col' = linear_col,
                                  'affinity_col' = affinity_col,
                                  'mean_col' = mean_col),
                       guide = 'none')



##only slopes - Jaccard and Morisita
jac_horn_coefs %>%
   filter(term=='.x$c_temp_dist') %>%
   ggplot() +
   stat_smooth(aes(x = sps_pool, y = jac_estimate), color= "#bc5090", size = 2, method = "lm") +
   stat_smooth(aes(x = sps_pool, y = mh_estimate), color="#003f5c", size = 2, method = "lm") +
   labs(x = 'sps_pool',
        y = 'Jaccard slope (rate of turnover)',
        subtitle = 'All years comparison') +
   theme_minimal() +
   theme(legend.position = "bottom")
# +
   # scale_y_continuous(name = 'Jaccard slope (rate of turnover)',
   #                    sec.axis = sec_axis(name = 'Morisita slope (rate of turnover)')) +
   # scale_colour_manual(values = c('linear_col' = linear_col,
   #                                'affinity_col' = affinity_col,
   #                                'mean_col' = mean_col),
   #                     guide = 'none') ###need to fix second y-axis etc




###jaccard slopes on a map
ggplot() +
   geom_polygon(data=USAmap, aes(long, lat, group = group), fill="gray60", size=0, alpha=0.6) +
   geom_point(data = jac_horn_coefs %>%
                 filter(term=='.x$c_temp_dist') %>%
                 arrange(jac_estimate),
              aes(x=Longitude, y=Latitude, size= n_sps, color=jac_estimate), alpha= 0.5) +
   # scale_size(range=c(2, 5)) +
   scale_colour_viridis_c() +
   theme_bw() +
   coord_cartesian() +
   theme(legend.position = "right")










## ## ## ##  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
## ## ## ##  ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
##calculate affinity #####################

## following Vicente's script "affinity_neutralv3.R" and adjusting as needed (similar to above for beta metrics)
#devtools::install_version("island",version="0.2.7")
library(island)
# library(gsl)

data_for_aff <- ft_4loc_filtered1 %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup()  ##this is the same as above


affinity_metrics1 <- tibble()


for(i in 1:nrow(data_for_aff)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(data_for_aff)))


   ##select each timeseries data and calculate same steps as in original "neutral" script
   temp <- data_for_aff[i,]%>%
      unnest (data) %>%
      select(-SourceID ,-TimeSeriesID) %>%
      arrange(Year) %>%
      pivot_wider(names_from = Year, values_from = Abundance, values_fill = 0)

   ##calculations
   temp <- (temp > 0)/1.0

   columns <- ncol(temp)

   raw_ce <- regular_sampling_scheme(temp, 2:(columns))
   raw_T_c <- 1/(raw_ce[1] + raw_ce[4])
   raw_J_t_c <- 1 - (raw_ce[1] + raw_ce[4] * exp(-1))/(raw_ce[1] + 2*raw_ce[4] - raw_ce[4] * exp(-1))
   raw_h_s <- raw_T_c * log((2 * raw_ce[1] + 3 * raw_ce[4])/(raw_ce[1] + 2 * raw_ce[4]))
   raw_a_j <- 1 - (raw_ce[1] / (raw_ce[1] + 2 * raw_ce[4]))
   raw_affinity <- raw_a_j / raw_h_s

   ##combine results per timeseries to be saved
   aux_aff = c(SourceID = data_for_aff$SourceID[i],
               TimeSeriesID = data_for_aff$TimeSeriesID[i],
               # T_c = unname(T_c),
               # h_t = unname(h_s),
               # a_j = unname(a_j),
               # Affinity = unname(affinity),
               # J_t_c = unname(J_t_c),
               raw_J_t_c = unname(raw_J_t_c),
               raw_T_c = unname(raw_T_c),
               raw_h_t = unname(raw_h_s),
               raw_a_j = unname(raw_a_j),
               raw_Affinity = unname(raw_affinity),
               Duration = columns - 1)


   affinity_metrics1 = bind_rows(affinity_metrics1, aux_aff)
}

##some warnings re NAs produced...



## ## ##
###add affinity to the beta metrics object
##and ignoring intercepts for now?

jac_horn_coefs$SourceID <- as.factor(jac_horn_coefs$SourceID)
jac_horn_coefs$TimeSeriesID <- as.factor(jac_horn_coefs$TimeSeriesID)

affinity_metrics1$SourceID <- as.factor(affinity_metrics1$SourceID)
affinity_metrics1$TimeSeriesID <- as.factor(affinity_metrics1$TimeSeriesID)


all_metrics<- jac_horn_coefs %>%
   filter(term=='.x$c_temp_dist') %>%
   left_join(affinity_metrics1 %>%
                select(SourceID, TimeSeriesID, raw_Affinity, Duration),
             by= c("SourceID", "TimeSeriesID"))



##plots from Shane's script "figure2-draft-3.R"

##sps pool
all_metrics %>%

   ggplot() +
   # geom_point(aes(x = sps_pool, y = jac_estimate)) +
   stat_smooth(aes(x = sps_pool, y = jac_estimate, colour = 'linear_col'),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
   # geom_point(aes(x = sps_pool, y = raw_Affinity, colour = 'affinity_col')) +
   stat_smooth(aes(x = sps_pool, y = raw_Affinity, colour = 'affinity_col'),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
   stat_smooth(aes(x = sps_pool, y = mean_Jac/10, colour = 'mean_col'),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
   # geom_hline(yintercept = 0, lty = 2) +
   scale_y_continuous(name = 'Parameter estimate',
                      sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                          name = 'Mean Jaccard (all year pairs)')) +
   scale_colour_manual(values = c('linear_col' = linear_col,
                                  'affinity_col' = affinity_col,
                                  'mean_col' = mean_col),
                       guide = 'none') +
   labs(x = 'Regional species pool size') +
   theme_classic() +
   theme(axis.title.y = element_text(size=7),
         axis.title.y.right = element_text(size=7),
         axis.title.x = element_text(size = 7),
         axis.line.y.left = element_line(),
         axis.text.y.left = element_text(size = 6),
         axis.line.y.right = element_line(colour = mean_col),
         axis.text.y.right = element_text(colour = mean_col, size = 6),
         axis.text.x = element_text(size = 6))



##time series duration
all_metrics %>%

   ggplot() +
   # geom_point(aes(x = n_years, y = jac_estimate)) +
   stat_smooth(aes(x = n_years, y = jac_estimate, colour = 'linear_col'),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
   # geom_point(aes(x = n_years, y = raw_Affinity, colour = 'affinity_col')) +
   stat_smooth(aes(x = n_years, y = raw_Affinity, colour = 'affinity_col'),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
   stat_smooth(aes(x = n_years, y = mean_Jac/10, colour = 'mean_col'),
               method = 'gam',
               formula = y ~ s(x, bs = 'cs', k = 5)) +
   # geom_hline(yintercept = 0, lty = 2) +
   scale_y_continuous(name = 'Parameter estimate',
                      sec.axis = sec_axis(trans = ~.*10, # back transform so as numbers are correct on label
                                          name = 'Mean Jaccard (all year pairs)')) +
   scale_colour_manual(values = c('linear_col' = linear_col,
                                  'affinity_col' = affinity_col,
                                  'mean_col' = mean_col),
                       guide = 'none') +
   labs(x = 'Number of years') +
   theme_classic() +
   theme(axis.title.y = element_text(size=7),
         axis.title.y.right = element_text(size=7),
         axis.title.x = element_text(size = 7),
         axis.line.y.left = element_line(),
         axis.text.y.left = element_text(size = 6),
         axis.line.y.right = element_line(colour = mean_col),
         axis.text.y.right = element_text(colour = mean_col, size = 6),
         axis.text.x = element_text(size = 6)
   )




##end##



