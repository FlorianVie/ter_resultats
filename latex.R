library(tidyverse)
library(ggpubr)
library(rstatix)
library(xtable)

xtable(nback_summary %>% select(groupe, part, Score, rt))
