library(tidyverse)
library(ggpubr)
library(rstatix)
library(xtable)

sujet <- read.csv2("data/sujet.csv", sep = ",", fileEncoding="UTF-8-BOM")

sujets <- sujet %>%
  group_by(id_sujet) %>%
  pivot_wider(names_from = id_question_sub, values_from = reponse_sub_sujet, values_fn = mean) %>%
  rename(Pre_motiv = `1`) %>%
  rename(Mem_diffic = `2`) %>%
  rename(Transc_diffic = `3`) %>%
  rename(Quest_diffic = `4`) %>%
  rename(Fatigue = `5`) %>%
  rename(Recommencer = `6`) %>%
  arrange(id_sujet)

write.csv(sujets, file = "data/comp_sub.csv")

