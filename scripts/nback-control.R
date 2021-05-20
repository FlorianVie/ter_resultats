library(tidyverse)
library(tidyr)
library(ggpubr)

nback_control <- read.csv2("./data/control_nback.csv", sep = ",")
nback_control <- nback_control %>%
  filter(response_type != "") %>%
  filter(subject_id != 13) %>%
  select(subject_id, trial_index, part, trial_id, id_2_back, block_part, is_target, correct, response_type)

nback_control_scores <- nback_control %>%
  group_by(subject_id, part, response_type) %>%
  count(response_type) %>%
  pivot_wider(names_from = response_type, values_from = n) %>%
  replace(is.na(.), 0) %>%
  arrange(subject_id, desc(part))

nback_control_scores$score <- (nback_control_scores$Target - (nback_control_scores$`False-alarm` + nback_control_scores$Mismatch + nback_control_scores$`No-input`))/20

nback_control_means <- nback_control_scores %>%
  group_by(part) %>%
  summarise_all(mean) %>%
  pivot_longer(c(`False-alarm`, Mismatch, Target, `No-input`, `Non-target`, score), values_to = "mean") %>%
  arrange(desc(part)) %>%
  select(-subject_id)


nback_control_plot <- nback_control_means %>%
  filter(name != "Non-target") %>%
  filter(name != "score")

nback_control_plot2 <- nback_control_means %>%
  filter(name != "Non-target") %>%
  filter(name != "score")
nback_control_plot$part <- factor(nback_control_plot$part, levels = c("2-back-pre", "2-back-post"))

p1 <- ggplot(nback_control_plot, aes(name, mean, fill=part)) +
  geom_col(color="black", position = position_dodge(0.6), width=0.5) +
  theme_minimal() +
  xlab("Type de réponse") +
  ylab("Nombre moyen") +
  ggtitle("Scores n-back en pre-test et post-test en fonction du type de réponse, condition contrôle") +
  scale_fill_discrete(name = "Moment", labels = c("Pré-test", "Post-test"))
p1


nback_control_plot2 <- nback_control_means %>%
  filter(name == "score")
nback_control_plot2$part <- factor(nback_control_plot2$part, levels = c("2-back-pre", "2-back-post"))

p2 <- ggplot(nback_control_plot2, aes(name, mean, fill=part)) +
  geom_col(color="black", position = position_dodge(0.6), width=0.5) +
  theme_minimal() +
  xlab("") +
  ylab("Score moyen") +
  ggtitle("Scores n-back en pre-test et post-test, condition contrôle") +
  scale_fill_discrete(name = "Moment", labels = c("Pré-test", "Post-test"))
p2

library(ez)
model <- ezANOVA(data = nback_control_scores, wid=subject_id, dv=score, within=part, return_aov = T)
model








