library(tidyverse)

rspan <- read.csv2("./data/control_rspan.csv", sep = ",")
rspan_scored <- read.csv2("./data/control_rspan_scored.csv", sep = ",")
rspan <- rspan %>%
  select(subject_id, part, time_elapsed, rt, set_number, size, correct, letters_recalled, correct_letters) %>%
  filter(part != "") %>%
  filter(part != "instruction") %>%
  filter(part != "Pause") %>%
  filter(part != "Beep Beep Beep") %>%
  filter(part != "Audio") %>%
  filter(part != "Retranscription") %>%
  filter(part != "letter-pre") %>%
  filter(part != "letter-post")

rspan_wm <- rspan %>%
  filter(part != "sentence-pre")%>%
  filter(part != "sentence-post")

rspan_sent <- rspan %>%
  filter(part != "recall-pre")%>%
  filter(part != "recall-post")

sentences <- rspan_sent %>%
  group_by(part) %>%
  summarise(
    sentence_score = mean(correct)
  ) %>%
  arrange(desc(part))

sentences$part <- factor(sentences$part, levels = c("sentence-pre", "sentence-post"))

p1 <- ggplot(sentences, aes(part, sentence_score)) +
  geom_col(color="black", position = position_dodge(1), width=0.5) +
  theme_minimal() +
  xlab("Type de réponse") +
  ylab("Nombre moyen") +
  xlab("") +
  ylab("Score moyen") +
  ggtitle("RSPAN Phrases - Contrôle")
p1

rspan_scored$PCU <- as.numeric(rspan_scored$PCU)

rspan_wm <- rspan_scored %>%
  group_by(part) %>%
  summarise(
    ANL = mean(ANL),
    PCL = mean(PCL)
  ) %>%
  arrange(desc(part))

rspan_wm$part <- factor(rspan_wm$part, levels = c("recall-pre", "recall-post"))

p2 <- ggplot(rspan_wm, aes(part, PCL)) +
  geom_col(color="black", position = position_dodge(1), width=0.5) +
  theme_minimal() +
  xlab("Type de réponse") +
  ylab("Nombre moyen") +
  xlab("") +
  ylab("Score moyen") +
  ggtitle("RSPAN Score PCL - Contrôle")
p2

p3 <- ggplot(rspan_wm, aes(part, ANL)) +
  geom_col(color="black", position = position_dodge(1), width=0.5) +
  theme_minimal() +
  xlab("Type de réponse") +
  ylab("Nombre moyen") +
  xlab("") +
  ylab("Score moyen") +
  ggtitle("RSPAN Score ANL - Contrôle")
p3


rspan_wm_size <- rspan_scored %>%
  group_by(part, size) %>%
  summarise(
    ANL = mean(ANL),
    PCL = mean(PCL)
  ) %>%
  arrange(size, desc(part))

rspan_wm_size$part <- factor(rspan_wm_size$part, levels = c("recall-pre", "recall-post"))
rspan_wm_size$size <- as.character(rspan_wm_size$size)

p4 <- ggplot(rspan_wm_size, aes(size, PCL, fill=part)) +
  geom_col(color="black", position = position_dodge(0.6), width=0.5) +
  theme_minimal() +
  xlab("Type de réponse") +
  ylab("Nombre moyen") +
  xlab("Taille de l'essai") +
  ylab("Score moyen") +
  ggtitle("RSPAN Score PCL - Contrôle")
p4












