

##### Joey playing around with simulation outputs
library(tidyverse)
library(janitor)
library(cowplot)
theme_set(theme_cowplot())


beta_100 <- read_csv("prelim_results/beta_dist_100.csv")
key <- read_csv("data/simulations/mobsim/IFYAH3130E_jitter_metadata.csv")


all_beta <- left_join(beta_100, key, by = "parameter_id") %>%
   clean_names()


### jbetas
all_beta %>%
   ggplot(aes(x = jbeta)) + geom_histogram()
