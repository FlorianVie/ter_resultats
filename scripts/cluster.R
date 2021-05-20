library("FactoMineR")
library("factoextra")
library(tidyverse)
library(ggcorrplot)
library(gridExtra)

df <- read.csv("./data/acp.csv", sep = ";", row.names = 1) %>%
  select(c(-erreurs))

prepost <- df %>% select(pre_test, post_test)

df$delta <- df$pre_test - df$post_test

str(df)

res.mfa <- MFA(df, ncp = 20, graph = F, 
               group = c(5, 2, 1, 6, 1, 1, 1), 
               name.group = c("Transcription", "WM", "Comprehension", "Subjective", "Frappe", "Groupe", "Delta"),
               type = c("s", "s", "s", "s", "s", "n", "s"))

df <- df %>%
  select(-pre_test, -post_test)

res.mfa <- MFA(df, ncp = 20, graph = F, 
               group = c(5, 1, 6, 1, 1, 1), 
               name.group = c("Transcription", "Comprehension", "Subjective", "Frappe", "Groupe", "Delta"),
               type = c("s", "s", "s", "s", "n", "s"))


# Classification hiérarchique
res.hcpc <- HCPC(res.mfa, graph = FALSE)

# Contributions à la dimension 1
fviz_contrib (res.mfa, choice = "quanti.var", axes = 1, top = 20, palette = "jco")

# Contributions à la dimension 2
fviz_contrib (res.mfa, choice = "quanti.var", axes = 2, top = 20, palette = "jco")

fviz_mfa_var(res.mfa, "quanti.var", palette = "jco", repel = TRUE)

corr <- cor(df%>%select(-groupe))
mat.p <- cor_pmat(df%>%select(-groupe))
ggcorrplot(corr, type = "upper", ggtheme = theme_minimal(), colors = c("#6D9EC1", "white", "#E46726"), lab = T, lab_size = 3, p.mat = mat.p)

fviz_mfa_ind(res.mfa, 
             habillage = "groupe", # color by groups 
             palette = "JCO",
             addEllipses = F, ellipse.type = "confidence", 
             repel = TRUE 
) 

fviz_dend(res.hcpc, cex = 0.7, palette = "jco", show_labels = T)

fviz_cluster(res.hcpc, geom = "point", main = "Factor map", palette = "jco", ggtheme = theme_minimal())

dfClusters <- res.hcpc$data.clust

dfClusters$pre_test <- prepost$pre_test
dfClusters$post_test <- prepost$post_test

dfClustersMeans <- dfClusters %>%
  group_by(clust) %>%
  summarise_all(mean)

dfClustersMeans <- dfClustersMeans %>%
  pivot_longer(c(pre_test, post_test))

dfClustersMeans$name <- factor(dfClustersMeans$name, levels = c("pre_test", "post_test"))

ggplot(dfClustersMeans)+
  geom_col(aes(clust, value, fill=name), position = position_dodge(0.6), color = "black", width = 0.5) +
  theme_minimal()+
  xlab("Clusters") +
  ylab("Score moyen") +
  ggtitle("Clusters - Pré-test Post-test")


dfClustersMeans2 <- dfClusters %>%
  group_by(clust) %>%
  summarise_all(mean) %>%
  select(-groupe)

View(dfClustersMeans2)





