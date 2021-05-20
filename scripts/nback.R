library(tidyverse)
library(tidyr)
library(ggpubr)

nback <- read.csv2("./data/retran_nback.csv", sep = ",")
nback <- nback %>%
  filter(response_type != "") %>%
  select(subject_id, trial_index, part, trial_id, id_2_back, block_part, is_target, correct, response_type)

nback_scores <- nback %>%
  group_by(subject_id, part, response_type) %>%
  count(response_type) %>%
  pivot_wider(names_from = response_type, values_from = n) %>%
  replace(is.na(.), 0) %>%
  arrange(subject_id, desc(part))

nback_scores$score <- (nback_scores$Target - (nback_scores$`False-alarm` + nback_scores$Mismatch + nback_scores$`No-input`))/20

nback_means <- nback_scores %>%
  group_by(part) %>%
  summarise_all(mean) %>%
  pivot_longer(c(`False-alarm`, Mismatch, Target, `No-input`, `Non-target`, score), values_to = "mean") %>%
  arrange(desc(part)) %>%
  select(-subject_id)


nback_plot <- nback_means %>%
  filter(name != "Non-target") %>%
  filter(name != "score")

nback_plot2 <- nback_means %>%
  filter(name != "Non-target") %>%
  filter(name != "score")
nback_plot$part <- factor(nback_plot$part, levels = c("2-back-pre", "2-back-post"))

p1 <- ggplot(nback_plot, aes(name, mean, fill=part)) +
  geom_col(color="black", position = position_dodge(0.6), width=0.5) +
  theme_minimal() +
  xlab("Type de réponse") +
  ylab("Nombre moyen") +
  xlab("") +
  ylab("Score moyen") +
  ggtitle("Scores n-back en pre-test et post-test en fonction du type de réponse, condition épuisement")  +
  scale_fill_discrete(name = "Moment", labels = c("Pré-test", "Post-test"))
p1


nback_plot2 <- nback_means %>%
  filter(name == "score")
nback_plot2$part <- factor(nback_plot2$part, levels = c("2-back-pre", "2-back-post"))

p2 <- ggplot(nback_plot2, aes(name, mean, fill=part)) +
  geom_col(color="black", position = position_dodge(0.6), width=0.5) +
  theme_minimal() +
  xlab("") +
  ylab("Score moyen") +
  xlab("") +
  ylab("Score moyen") +
  ggtitle("Scores n-back en pre-test et post-test, condition épuisement") +
  scale_fill_discrete(name = "Moment", labels = c("Pré-test", "Post-test"))
p2





library(ez)
model <- ezANOVA(data = nback_scores, wid=subject_id, dv=score, within=part, return_aov = T)
model


model <- ezANOVA(data = nback_scores, wid=subject_id, dv=Target, within=part, return_aov = T)
model

















