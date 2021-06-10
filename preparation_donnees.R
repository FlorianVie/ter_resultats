library(tidyverse)
library(formattable)
library(rstatix)

recap <- read.csv("data/recap.csv", fileEncoding="UTF-8-BOM")

formattable(recap)

recap %>%
  ungroup %>%
  group_by(groupe, test) %>%
  summarise_if(is.numeric, mean) %>%
  write_csv(file = "recap_export/moyennes.csv")



recap %>%
  select(-age) %>%
  write_csv(file = "recap_export/recap.csv")






