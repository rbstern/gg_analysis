library(magrittr)
library(rstan)
library(tidyverse)

#Load clean data
data <- read_rds("./data/data.rds") %>%
  as_tibble()

#################################################################
# Extract covariates from goalkeeper game that are used by stan #
#################################################################
stages <- data %>%
  select(new_play_id_chr) %>%
  distinct() %>%
  filter((grepl("JG", new_play_id_chr) & 
            grepl("v1", new_play_id_chr)) |
           grepl("JM", new_play_id_chr))
data %<>% inner_join(stages, by = "new_play_id_chr")

factors <- data %>% 
  select(alias_int, escolaridade, fator, id_int, hy) %>%
  group_by(fator) %>%
  summarise_all(funs(first(na.omit(.)))) %>%
  arrange(fator) %>%
  rowid_to_column("fator_id")

data %<>% inner_join(factors %>% select(fator, fator_id), 
                     by="fator")

############################
# Data format used by stan #
############################
stan_data <- data %>%
  select(correct_lgl, fator_id, move) %>%
  mutate(acerto = correct_lgl) %>%
  as.list()
stan_data$e <- factors$escolaridade %>% max
stan_data$escol <- factors$escolaridade
stan_data$m <- factors %>% nrow
stan_data$n = stan_data$acerto %>% length()

#############
# stan call #
#############
stan_fit <- stan(file = "./code/01-gg-stan.stan", 
                 data = stan_data, 
                 iter = 2000, chains=1)

#####################################
# extract gg parameters fit by stan #
#####################################
alfa <- stan_fit %>% rstan::extract("alfa")
beta <- stan_fit %>% rstan::extract("beta")
gamma <- stan_fit %>% rstan::extract("gamma")
alfa_m <- alfa$alfa %>% colMeans
beta_m <- beta$beta %>% colMeans
gamma_m <- gamma$gamma %>% colMeans

factors %<>% cbind(beta_m, gamma_m)
factors_alfa <- factors %>% 
  select(escolaridade) %>%
  distinct() %>%
  arrange(escolaridade) %>% 
  cbind(alfa_m)
factors %<>% inner_join(factors_alfa, by = "escolaridade")

stan_info <- list(stages = stages,
                  param  = factors)
write_rds(stan_info, "./data/gg-stan-data.rds")
