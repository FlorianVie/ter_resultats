library(tidyverse)
library(ggpubr)
library(rstatix)
library(xtable)

nback <- read.csv2("data/nback.csv", sep = ",", fileEncoding="UTF-8-BOM")

nback_correct <- nback %>%
  filter(Target == 1 | Non.target == 1)

ggboxplot(nback_correct, x = "groupe", y = "rt")
