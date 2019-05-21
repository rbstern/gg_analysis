library(abjutils)
library(janitor)
library(magrittr)
library(tidyverse)

rm_accent_from_names <- function(dt)
{
  dt %<>% set_names(rm_accent(names(.)))
  invisible(dt)
}

data <- read_csv("./data-raw/amparo/amparo.csv") %>%
  as.tibble() %>%
  rm_accent_from_names() %>%
  clean_names()

# Use only players who answered correctly
# at least 3 out of 5 questions in the training stage
# of the Goalkeeper game.
players <- data %>%
  filter(game == "AQ") %>%
  group_by(player_alias) %>% 
  summarise(total_correct = sum(correct)) %>% 
  filter(total_correct >= 3) %>% 
  select(player_alias) %>%
  distinct()

data %<>% 
  inner_join(players, by="player_alias") %>%
  mutate(player_alias_fct  = as.factor(player_alias), 
         play_id_chr       = as.character(play_id),
         new_play_id_chr   = paste(game, play_id, sep="-"),
         alias_int         = player_alias %>% as.factor() %>% as.numeric(),
         id_int            = new_play_id_chr %>% as.factor() %>% as.numeric(),
         fator             = max(id_int)*(alias_int - 1) + id_int,
         id_alias_fct      = as.factor(paste(new_play_id_chr, player_alias, sep = "")),
         correct_lgl       = correct %>% as.numeric() %>% as.logical(),
         movement_time     = as.numeric(movement_time),
         move              = as.numeric(move)
  ) %>%
  arrange(id_alias_fct, move)

labels <- read_csv("./data-raw/amparo/labels.csv") %>%
  rm_accent_from_names() %>%
  clean_names() %>%
  inner_join(players, by="player_alias")

labels_clean <- labels %>%
  mutate(player_alias_fct  = as.factor(player_alias),
         hy           = hy %>% as.numeric %% 4,
         hy           = ifelse(is.na(hy), 0, hy),
         escolaridade = 1 + grepl("high", escolaridade) +
                        2*grepl("university", escolaridade)
  ) %>% 
  select(player_alias, player_alias_fct, hy, escolaridade, moca_total)

data <- inner_join(data, labels_clean, by="player_alias")
write.csv(data, "./data/amparo/data.csv")
#nao rodar por sigilo do DB
#saveRDS(data, "../data/amparo/data.rds")
