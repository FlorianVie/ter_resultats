library(tidyverse)
library(ggcorrplot)

transc <- read.csv2("./data/transcription.csv", sep = ",")
transc$Intervalle <- as.character(transc$Intervalle)

corr <- round(cor(transc[,-1]), 2)
p.mat <- cor_pmat(transc[,-1])

ggcorrplot(corr, type = "upper", ggtheme = theme_minimal(), colors = c("#6D9EC1", "white", "#E46726"), lab = T, p.mat = p.mat)

