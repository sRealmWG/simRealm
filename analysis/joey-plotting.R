

##### Joey playing around with simulation outputs
library(tidyverse)
library(janitor)
library(cowplot)
theme_set(theme_cowplot())


beta_100 <- read_csv("~/Documents/beta_dist_100.csv")
beta_10 <- read_csv("prelim_results/beta_dist_10.csv")
key <- read_csv("data/simulations/mobsim/IFYAH3130E_jitter_metadata.csv")

###S_POOL: Number of species in the landscape (20, 200)
###N_SIM: Number of individuals in the landscape (2000, 20 000)
###SAD_COEF: Evenness of the SAD (0.1, 0.5, 1)
###SIGMA: spatial dispersion around the mother point (high 0.05 -> low 0.2 -> random 0.8 aggregation)

identical(beta_100, beta_10)
head(beta_10)
head(beta_100)

### unique sad_coef 0.1 0.5 1.0
### sigma 0.05 10.00 .... 10 is random
### n_sim 2000 20000
### s_pool 20 200
### quadrat_area 0.01
### n_quadrats 100

unique(all_beta$sigma)

all_beta <- left_join(beta_100, key, by = "parameter_id") %>%
   clean_names() %>%
   filter(temp_dist %in% c(1:100), parameter_id %in% (1:24), time_series_id == 'ts40')
#filter(temp_dist %in% c(1:100), parameter_id == 11, sigma == 0.05, n_sim == 20000, sad_coef %in% c(1.0), quadrat_area == 0.01, n_quadrats == 100)


all_beta %>%
   group_by(time_series_id) %>%
   summarise(max_time = max(temp_dist)) %>% View
unique(all_beta$quadrat_area)
View(all_beta)

key %>%
   filter(parameter_id == 13) %>% View

### jbetas
all_beta %>%
   ggplot(aes(x = jbeta)) + geom_histogram(binwidth = 0.01) +
   facet_wrap( ~ sad_coef)

all_beta[all_beta$year1 ==min(all_beta$year1),] %>%
   ggplot(aes(x = temp_dist, y = jbeta)) + geom_point() + stat_smooth(method = "lm", se = FALSE) +
   facet_wrap( ~ parameter_id, scales = "free")

# ggplot() +
#    # facet_wrap(~sad_coef) +
#    geom_jitter(data = all_beta %>%
#                  # filter(time_series_id %in% c('ts1', 'ts2', 'ts3', 'ts4', 'ts5', 'ts6')),
#               aes(x = temp_dist, y = jbeta, colour = time_series_id)) +
#    stat_smooth(data = all_beta %>%
#                   # filter(time_series_id %in% c('ts1', 'ts2', 'ts3', 'ts4', 'ts5', 'ts6')),
#                method = 'lm', se = F,
#                aes(x = temp_dist, y = jbeta, colour = time_series_id))


####


neutral_beta_100 <- read_csv("prelim_results/neutral_beta_dist_100.csv")
key_neutral <- read_csv("data/simulations/QEAYJ1252R_neutral_metadata.csv")

all_neutral <- left_join(neutral_beta_100, key_neutral, by = "parameter_id") %>%
   clean_names()

all_neutral %>%
   ggplot(aes(x = jbeta)) + geom_histogram(binwidth = 0.01) +
   facet_wrap( ~ theta)

all_neutral %>%
   filter(parameter_id == 1) %>%
   ggplot(aes(x = temp_dist, y = jbeta)) + geom_point() + stat_smooth(method = "lm")

all_neutral %>%
   filter(parameter_id %in% c(1:12)) %>%
   ggplot(aes(x = temp_dist, y = jbeta)) + geom_point() + stat_smooth(method = "lm") +
   facet_grid(m ~ theta )

ggsave("figures/neutral-jbeta-temp_dist.pdf", width = 12, height = 14)


