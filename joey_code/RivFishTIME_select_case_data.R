###Laura 06-04-2022
###updated 17-05-2022

##select appropriate time series from RivFishTIME for case study


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

##import data and metadata
ft <- read_csv("case_study/RivFishTIME/1873_10_1873_2_RivFishTIME_SurveyTable.csv")
meta <- read_csv("case_study/RivFishTIME/1873_10_1873_2_RivFishTIME_TimeseriesTable.csv")[-13]


View(meta)
unique(meta$Protocol)

##1. filter USA data and sources with 4 or more sites
study_USA_4loc <- meta %>%
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
### note from JB -- could we lump together all of the electrofishing protocols?

?pull

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
   theme(legend.position = "bottom")




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
   group_by(SourceID, TimeSeriesID, Latitude, Longitude, Region) %>%
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



### Note to JB -- I don't quite understand this step
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


##!!have to recalculate basic metadata info (n_sps and n_years) because removing quarters means some years within timeseries will be excluded!!
metainfo2 <- ft_4loc_filtered %>%
   group_by(SourceID, TimeSeriesID, Latitude, Longitude, Region) %>%
   summarise(n_sps= n_distinct(Species),
             n_years= n_distinct(Year),
             min_year= min(Year),
             max_year= max(Year)) %>%

   ##removing time series with only 1 year or only 1 species
   filter(n_years>1 & n_sps>1)


##check max and min duration
min(metainfo2$n_years)  ##2
max(metainfo2$n_years)  ##31

##check max and min richness
min(metainfo2$n_sps)  ##2
max(metainfo2$n_sps)  ##76


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
ggsave("joey_code/figures/rivfishtime_map.png", width = 8, height = 6)


##!!from this we can easily select time series from a given Region, and add extra criteria

aux_Region <- metainfo2 %>%
   group_by(Region) %>%
   summarise(n_source = n_distinct(SourceID),
             n_ts = n_distinct(TimeSeriesID),
             mean_sps= mean(n_sps),
             max_sps= max(n_sps),
             mean_years= mean(n_years),
             min_year= min(min_year),
             max_year= max(max_year))



##one example ----> selecting 3 states with low diversity, and 3 states with high diversity???

subset_6regions <- ft_4loc_filtered %>%
   filter(Region %in% c("COLORADO", "ARIZONA", "NEW MEXICO", "ILLINOIS", "WISCONSIN", "MINNESOTA"))


n_distinct(subset_6regions$SourceID)  ##8
n_distinct(subset_6regions$TimeSeriesID) ##172
n_distinct(subset_6regions$Region)  ##6
n_distinct(subset_6regions$Waterbody)  ##96


ggplot() +
   geom_polygon(data=USAmap, aes(long, lat, group = group), fill="gray60", size=0, alpha=0.6) +
   geom_point(data= metainfo2 %>%
                 filter(TimeSeriesID %in% subset_6regions$TimeSeriesID),
              aes(x=Longitude, y=Latitude, size= n_years, color=n_sps), alpha= 0.8) +
   scale_colour_viridis_c() +
   theme_bw() +
   coord_cartesian() +
   theme(legend.position = "bottom")




##trying to calculate some metrics based on Shane's code
##from "shane_code/calc_metrics_jitter.R"

##nest data for each time series ID to have year-species-abundance

data6regions <- subset_6regions %>%
   # unnest(ss100) %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, data)


##to save results
beta_test_6regions <- tibble() # dissimilarity


# i<- 1

for(i in 1:nrow(data6regions)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(data6regions)))

   # long data
   comm_long = data6regions %>%
      slice(i) %>%
      unnest(data) %>%
      arrange(-desc(Year))

   # alpha_temp <- comm_long %>%
   #    group_by(parameter_id, timeSeriesID, timestep) %>%
   #    summarise(S = n_distinct(species),
   #              S_PIE = mobr::calc_SPIE(N),
   #              N_all = sum(N),
   #              C_hat = mobr::Chat(N, N_all)) %>%
   #    ungroup()

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
             Jne = t(Jne)[lower.tri(t(Jne))],
             MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
             # put metadata back in
             SourceID = unique(comm_long$SourceID),
             TimeSeriesID = unique(comm_long$TimeSeriesID))

   beta_test_6regions = bind_rows(beta_test_6regions, all_pairs)
   # alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}



##for a given source ID, plotting Jtu ~ year (and MH_dist ~ year)

ggplot(beta_test_6regions) +
   geom_point(aes(x=YEAR2, y=Jtu)) +
   geom_smooth(method = "lm", aes(x=YEAR2, y=Jtu)) +
   theme_bw()

ggplot(beta_test_6regions) +
   geom_point(aes(x=YEAR2, y=MH_dist)) +
   geom_smooth(method = "lm", aes(x=YEAR2, y=Jtu)) +
   theme_bw()




##loops throws an error for i=58 -- to be fixed.... ???


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


##17-05-2022
##cherry-picking ####

## #1. low richness and short duration ####
poor_short1<- data.frame(metainfo2) %>%
   filter(n_sps>10) %>%  ##can adjust as needed
   slice_min(n_years) %>%
   slice_min(n_sps)

##6 timeseries


##following script "mobsim_calc_metrics"
##nest data for each time series ID to have year-species-abundance

subset_poor_short1 <- ft_4loc_filtered %>%
   filter(TimeSeriesID %in% poor_short1$TimeSeriesID) %>%
   # unnest(ss100) %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, data)


##to save results
beta_poor_short1 <- tibble() # dissimilarity


for(i in 1:nrow(subset_poor_short1)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(subset_poor_short1)))

   # long data
   comm_long = subset_poor_short1 %>%
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
             Jne = t(Jne)[lower.tri(t(Jne))],
             MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
             # put metadata back in
             SourceID = unique(comm_long$SourceID),
             TimeSeriesID = unique(comm_long$TimeSeriesID))

   beta_poor_short1 = bind_rows(beta_poor_short1, all_pairs)
   # alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}


beta_poor_short1 <- beta_poor_short1 %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%

   ##adding the initial info
   inner_join(., select(poor_short1, TimeSeriesID:n_years), by="TimeSeriesID")



ggplot(beta_poor_short1) +
   geom_point(aes(x=YEAR2, y=Jtu)) +
   geom_smooth(method = "lm", aes(x=YEAR2, y=Jtu)) +
   theme_bw() +
   ylim(0,1) +
   facet_wrap(~TimeSeriesID)  ##a single value of dissimilarity because only 2 years are used





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## #2. relatively low richness and short duration (but >2) ####
poor_short2<- data.frame(metainfo2) %>%
   filter(n_sps>10 & n_sps<15) %>%  ##can adjust as needed
   filter(n_years>2 & n_years< 10) #%>%
   # slice_min(n_years) %>%
   # slice_min(n_sps)

##33 timeseries


##nest data for each time series ID to have year-species-abundance
subset_poor_short2 <- ft_4loc_filtered %>%
   filter(TimeSeriesID %in% poor_short2$TimeSeriesID) %>%
   # unnest(ss100) %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, data)


##to save results
beta_poor_short2 <- tibble() # dissimilarity


for(i in 1:nrow(subset_poor_short2)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(subset_poor_short2)))

   # long data
   comm_long = subset_poor_short2 %>%
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
             Jne = t(Jne)[lower.tri(t(Jne))],
             MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
             # put metadata back in
             SourceID = unique(comm_long$SourceID),
             TimeSeriesID = unique(comm_long$TimeSeriesID))

   beta_poor_short2 = bind_rows(beta_poor_short2, all_pairs)
   # alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}


beta_poor_short2 <- beta_poor_short2 %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%

   ##adding the initial info
   inner_join(., select(poor_short2, TimeSeriesID:n_years), by="TimeSeriesID")



ggplot(beta_poor_short2) +
   geom_point(aes(x=YEAR2, y=Jtu)) +
   geom_smooth(method = "lm", aes(x=YEAR2, y=Jtu)) +
   theme_bw() +
   ylim(0,1) +
   facet_wrap(~TimeSeriesID, scales = "free_x")




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## #3. relatively low richness and long duration ####
poor_long<- data.frame(metainfo2) %>%
   filter(n_sps>10 & n_sps<15) %>%  ##can adjust as needed
   filter(n_years>15)

##7 timeseries


##nest data for each time series ID to have year-species-abundance
subset_poor_long <- ft_4loc_filtered %>%
   filter(TimeSeriesID %in% poor_long$TimeSeriesID) %>%
   # unnest(ss100) %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, data)


##to save results
beta_poor_long <- tibble() # dissimilarity


for(i in 1:nrow(subset_poor_long)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(subset_poor_long)))

   # long data
   comm_long = subset_poor_long %>%
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
             Jne = t(Jne)[lower.tri(t(Jne))],
             MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
             # put metadata back in
             SourceID = unique(comm_long$SourceID),
             TimeSeriesID = unique(comm_long$TimeSeriesID))

   beta_poor_long = bind_rows(beta_poor_long, all_pairs)
   # alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}


beta_poor_long <- beta_poor_long %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%

   ##adding the initial info
   inner_join(., select(poor_long, TimeSeriesID:n_years), by="TimeSeriesID")



ggplot(beta_poor_long) +
   geom_point(aes(x=YEAR2, y=Jtu)) +
   geom_smooth(method = "lm", aes(x=YEAR2, y=Jtu)) +
   theme_bw() +
   ylim(0,1) +
   facet_wrap(~TimeSeriesID, scales = "free_x")





### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## #4. high richness and short duration (but >2) ####
rich_short<- data.frame(metainfo2) %>%
   filter(n_sps>35) %>%  ##can adjust as needed
   filter(n_years>2 & n_years< 10)

##24 timeseries


##nest data for each time series ID to have year-species-abundance
subset_rich_short <- ft_4loc_filtered %>%
   filter(TimeSeriesID %in% rich_short$TimeSeriesID) %>%
   # unnest(ss100) %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, data)


##to save results
beta_rich_short <- tibble() # dissimilarity


for(i in 1:nrow(subset_rich_short)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(subset_rich_short)))

   # long data
   comm_long = subset_rich_short %>%
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
             Jne = t(Jne)[lower.tri(t(Jne))],
             MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
             # put metadata back in
             SourceID = unique(comm_long$SourceID),
             TimeSeriesID = unique(comm_long$TimeSeriesID))

   beta_rich_short = bind_rows(beta_rich_short, all_pairs)
   # alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}


beta_rich_short <- beta_rich_short %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%

   ##adding the initial info
   inner_join(., select(rich_short, TimeSeriesID:n_years), by="TimeSeriesID")



ggplot(beta_rich_short) +
   geom_point(aes(x=YEAR2, y=Jtu)) +
   geom_smooth(method = "lm", aes(x=YEAR2, y=Jtu)) +
   theme_bw() +
   ylim(0,1) +
   facet_wrap(~TimeSeriesID, scales = "free_x")




### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

## #5. high richness and long duration ####
rich_long<- data.frame(metainfo2) %>%
   filter(n_sps>35) %>%  ##can adjust as needed
   filter(n_years>15)

##49 timeseries


##nest data for each time series ID to have year-species-abundance
subset_rich_long <- ft_4loc_filtered %>%
   filter(TimeSeriesID %in% rich_long$TimeSeriesID) %>%
   # unnest(ss100) %>%
   select(SourceID, TimeSeriesID, Year, Species, Abundance) %>%
   group_by(SourceID, TimeSeriesID) %>%
   nest(data = c(Year, Species, Abundance)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, data)


##to save results
beta_rich_long <- tibble() # dissimilarity


for(i in 1:nrow(subset_rich_long)){
   # counter for sanity
   print(paste('calculation ', i, 'of ', nrow(subset_rich_long)))

   # long data
   comm_long = subset_rich_long %>%
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
             Jne = t(Jne)[lower.tri(t(Jne))],
             MH_dist = t(MH_dist)[lower.tri(t(MH_dist))],
             # put metadata back in
             SourceID = unique(comm_long$SourceID),
             TimeSeriesID = unique(comm_long$TimeSeriesID))

   beta_rich_long = bind_rows(beta_rich_long, all_pairs)
   # alpha_scale_100 = bind_rows(alpha_scale_100, alpha_temp)
}


beta_rich_long <- beta_rich_long %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%

   ##adding the initial info
   inner_join(., select(rich_long, TimeSeriesID:n_years), by="TimeSeriesID")



ggplot(beta_rich_long) +
   geom_point(aes(x=YEAR2, y=Jtu)) +
   geom_smooth(method = "lm", aes(x=YEAR2, y=Jtu)) +
   theme_bw() +
   ylim(0,1) +
   facet_wrap(~TimeSeriesID, scales = "free_x")



#####
#bind all these objects together

all_dissim<- rbind(beta_poor_short2 %>% mutate(type= "poor_short"),
                   beta_poor_long %>% mutate(type= "poor_long"),
                   beta_rich_short %>% mutate(type= "rich_short"),
                   beta_rich_long %>% mutate(type= "rich_long"))


################################################################################################################
################################################################################################################

##calculate slopes #####################

##from Shane's script "fit-linear-models-array-25-allYrs"


# fit linear models to turnover (dissimilarity)
allYrs_fish <- data.frame(all_dissim) %>%
   mutate(temp_dist = YEAR2 - YEAR1) %>%
   group_by(SourceID, TimeSeriesID) %>%
   mutate(c_temp_dist = temp_dist - mean(temp_dist)) %>%
   ungroup() %>%
   select(SourceID, TimeSeriesID, c_temp_dist, temp_dist, Jbeta, MH_dist) %>%
   nest(data = c(c_temp_dist, temp_dist, Jbeta, MH_dist)) %>%
   mutate(Jac_lm_allYrs = map(data, ~lm(.x$Jbeta ~ .x$c_temp_dist)),
          mean_Jac = map(data, ~mean(.x$Jbeta)),
          mh_lm_allYrs = map(data, ~lm(.x$MH_dist ~ .x$c_temp_dist)),
          mean_mh = map(data, ~mean(.x$MH_dist)),
          Jac_glm_allYrs = map(data, possibly(~betareg::betareg(.x$Jbeta ~ .x$c_temp_dist), otherwise = NULL)),
          mh_glm_allYrs = map(data, possibly(~betareg::betareg(.x$MH_dist ~ .x$c_temp_dist), otherwise = NULL))) %>%
   mutate(Jac_lm_allYrs_tidy = map(Jac_lm_allYrs, broom::tidy),
          MH_lm_allYrs_tidy = map(mh_lm_allYrs, broom::tidy),
          Jac_glm_allYrs_tidy = map(Jac_glm_allYrs, possibly(broom::tidy, otherwise = NULL)),
          MH_glm_allYrs_tidy = map(mh_glm_allYrs, possibly(broom::tidy, otherwise = NULL))) %>%
   ungroup()



##extracting lm coefs for Jaccard and Morisita
##and adding back info re n_sps and duration etc
jac_mh_coefs <- left_join(allYrs_fish %>%
                             unnest(c(Jac_lm_allYrs_tidy)) %>%
                             rename(jac_estimate = estimate) %>%
                             select(SourceID, TimeSeriesID, term, jac_estimate),
                          allYrs_fish %>%
                             unnest(c(MH_lm_allYrs_tidy)) %>%
                             rename(mh_estimate = estimate) %>%
                             select(SourceID, TimeSeriesID, term, mh_estimate)) %>%
   left_join(all_dissim %>%
                select(TimeSeriesID, Latitude:type) %>%
                distinct(), by="TimeSeriesID")


jac_mh_coefs$type<- factor(jac_mh_coefs$type, levels=c("poor_short", "poor_long", "rich_short", "rich_long"))




##[[plots following script "inspection.R"]]

##comparing mean(intercept) of Jaccard and Morisita
ggplot() +
   # facet_grid(~type,# scales = 'free',
   #            labeller = label_wrap_gen(width = 10)) +
   geom_point(data = jac_mh_coefs %>%
                 filter(term=='(Intercept)'),
              aes(x = jac_estimate, y = mh_estimate, colour = n_years),
              alpha = 0.85) +
   geom_hline(yintercept = 0, lty = 2) +
   geom_vline(xintercept = 0, lty = 2) +
   geom_abline(intercept = 0, slope = 1, lty = 2) +
   scale_color_viridis_c(name = 'Duration', direction = -1) +
   labs(x = 'jaccard intercept (mean)',
        y = 'morisita-horn (mean)') +
   theme_minimal() +
   coord_fixed() +
   theme(plot.background = element_rect(fill = 'white', colour = 'white'),
         legend.position = 'top',
         legend.direction = 'horizontal',
         strip.text = element_text(size = 8),
         axis.text.x = element_text(size = 6))


##comparing slopes of Jaccard and Morisita
ggplot() +
   # facet_grid(~type,# scales = 'free',
   #            labeller = label_wrap_gen(width = 10)) +
   geom_point(data = jac_mh_coefs %>%
                 filter(term=='.x$c_temp_dist'),
              aes(x = jac_estimate, y = mh_estimate, colour = n_years),
              alpha = 0.85) +
   geom_hline(yintercept = 0, lty = 2) +
   geom_vline(xintercept = 0, lty = 2) +
   geom_abline(intercept = 0, slope = 1, lty = 2) +
   scale_color_viridis_c(name = 'Duration', direction = -1) +
   labs(x = 'jaccard slope',
        y = 'morisita-horn slope') +
   theme_minimal() +
   coord_fixed() +
   theme(plot.background = element_rect(fill = 'white', colour = 'white'),
         legend.position = 'top',
         legend.direction = 'horizontal',
         strip.text = element_text(size = 8),
         axis.text.x = element_text(size = 6))


##histograms of Jaccard intercept
allYrs_fish %>%
   unnest(Jac_lm_allYrs_tidy) %>%
   filter(term=='(Intercept)') %>%
   left_join(all_dissim %>%
                select(TimeSeriesID, Latitude:type) %>%
                distinct(), by="TimeSeriesID") %>%
   ggplot() +
   facet_wrap(~type, ncol = 4) +
   geom_histogram(aes(x = estimate)) +
   labs(x = 'Jaccard intercept (mean turnover magnitude)') +
   theme_minimal()


##histograms of Jaccard slope
allYrs_fish %>%
   unnest(Jac_lm_allYrs_tidy) %>%
   filter(term=='.x$c_temp_dist') %>%
   left_join(all_dissim %>%
                select(TimeSeriesID, Latitude:type) %>%
                distinct(), by="TimeSeriesID") %>%
   ggplot() +
   facet_wrap(~type, ncol = 4) +
   geom_histogram(aes(x = estimate)) +
   labs(x = 'Jaccard slope (rate of turnover)') +
   theme_minimal()



##
allYrs_fish %>%
   unnest(Jac_lm_allYrs_tidy) %>%
   filter(term=='.x$c_temp_dist') %>%
   left_join(all_dissim %>%
                select(TimeSeriesID, Latitude:type) %>%
                distinct(), by="TimeSeriesID") %>%
   ggplot() +
   # facet_wrap(~type, ncol = 4, scales = "free_x") +
   geom_point(aes(x = n_years, y = estimate, color=n_sps, size=n_sps), alpha= 0.85) +
   scale_color_viridis_c() +
   stat_smooth(aes(x = n_years, y = estimate),
               se = F, size = 0.5) +
   labs(x = 'n_years',
        y = 'Jaccard slope (rate of turnover)',
        subtitle = 'All years comparison') +
   theme_minimal()




allYrs_fish %>%
   left_join(all_dissim %>%
                select(TimeSeriesID, Latitude:type) %>%
                distinct(), by="TimeSeriesID") %>%
   unnest(data) %>%
   ggplot() +
   facet_wrap(~type) +
   geom_point(aes(x = temp_dist, y = Jbeta, colour = TimeSeriesID),
              size = 0.5, alpha = 0.5) +
   stat_smooth(method = 'lm', se = F,
               aes(x = temp_dist, y = Jbeta, colour = TimeSeriesID),
               size = 0.75) +
   theme_minimal() +
   theme(legend.position = 'none')



allYrs_fish %>%
   left_join(all_dissim %>%
                select(TimeSeriesID, Latitude:type) %>%
                distinct(), by="TimeSeriesID") %>%
   unnest(data) %>%
   ggplot() +
   facet_wrap(~type) +
   geom_point(aes(x = n_years, y = Jbeta, colour = TimeSeriesID),
              size = 0.5, alpha = 0.5) +
   stat_smooth(method = 'lm', se = F,
               aes(x = n_years, y = Jbeta, colour = TimeSeriesID),
               size = 0.75) +
   theme_minimal() +
   theme(legend.position = 'none')





###jaccard slopes on map
ggplot() +
   geom_polygon(data=USAmap, aes(long, lat, group = group), fill="gray60", size=0, alpha=0.6) +
   geom_point(data = jac_mh_coefs %>%
                 filter(term=='.x$c_temp_dist') %>%
                 arrange(jac_estimate),
              aes(x=Longitude, y=Latitude, size= n_sps, color=jac_estimate), alpha= 0.5) +
   # scale_size(range=c(2, 5)) +
   scale_colour_viridis_c() +
   theme_bw() +
   coord_cartesian() +
   theme(legend.position = "bottom")










