
#### Case study exploring
library(tidyverse)


biotime <- read_csv("~/Documents/BioTIMEQuery_24_06_2021.csv")


rivfish <- read_csv("~/Documents/1873_11_1873_2_RivFishTIME_SourceTable.csv")
rf <- read_csv("~/Documents/1873_11_1873_2_RivFishTIME_SurveyTable.csv")
rf2 <- read_csv("~/Documents/1873_11_1873_2_RivFishTIME_TimeseriesTable.csv")


can_data <- rf2 %>%
   filter(Country == "CAN")

can_data_series <- rf %>%
   filter(TimeSeriesID %in% c(can_data$TimeSeriesID))


