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
         escol        = rm_accent(escol),
         escolaridade = 1+grepl("high", escol)+
                        2*grepl("university", escol),
         moca_abs     = moca_abstracao,
         moca_ate     = moca_atencao,
         moca_evoc    = moca_evoc_tardia,
         moca_lin     = moca_linguagem,
         moca_nom     = moca_nomeacao,
         moca_ori     = moca_orientacao,
         moca_vis     = moca_visuoesp_exec,
         moca_tot     = moca_total,
         best_lim     = as.numeric(best_lim_estab_vert),
         best_marcha  = as.numeric(best_estab_marcha),
         best_orient  = best_orient_sens,
         best_reat    = best_reat,
         best_rest    = as.numeric(best_rest_biom),
         best_trans   = as.numeric(best_trans_antec),
         best_tot     = as.numeric(best),
         updrs_post   = as.numeric(est_post_updrs_iii),
         updrs_rig    = as.numeric(rigidez_updrs_iii),
         updrs_trem   = as.numeric(tremor_updrs_iii),
         updrs_tot    = as.numeric(updrs_iii)) %>%
  select(playeralias, idade, sexo, hy, escolaridade,
         moca_abs, moca_ate, moca_evoc, moca_lin, moca_nom, moca_ori, moca_vis, moca_tot,
         best_lim, best_marcha, best_orient, best_reat, best_rest, best_trans, best_tot,
         updrs_post, updrs_rig, updrs_trem, updrs_tot)

table_hy <- labels_clean$hy %>% 
  table()
table_hy[-1] %>% sum()

data <- inner_join(data, labels_clean, by="playeralias")
write.csv(data, "./data/amparo/data.csv")
#nao rodar por sigilo do DB
#saveRDS(data, "../data/amparo/data.rds")
