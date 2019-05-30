library(magrittr)
library(tidyverse)

# data from 00-gg-cleaner.R
data <- read_rds("./data/data.rds")

# data from 01-gg-stan.R
stan_info <- read_rds("./data/gg-stan-data.rds")
stages <- stan_info$stages
data %<>% inner_join(stages, by = "new_play_id_chr")


param <- stan_info$param
# Spread beta parameters
aux1 <- param %>% select(alias_int, id_int, beta_m) %>% spread(id_int, beta_m)
colnames(aux1) <- c(colnames(aux1)[1],
                    "beta_" %>% paste(colnames(aux1)[-1], sep=""))

# Spread gamma parameters
aux2 <- param %>% select(alias_int, id_int, gamma_m) %>% spread(id_int, gamma_m)
colnames(aux2) <- c(colnames(aux2)[1],
                    "gamma_" %>% paste(colnames(aux2)[-1], sep=""))

# Spread time parameters
times <- data %>% 
  filter(move != 1) %>%
  select(fator, alias_int, id_int, movement_time) %>%
  group_by(fator) %>%
  summarise(alias_int = mean(alias_int), id_int = mean(id_int), time_m = mean(movement_time, na.rm=TRUE)) %>%
  select(alias_int, id_int, time_m)
aux3 <- times %>% select(alias_int, id_int, time_m) %>% spread(id_int, time_m)
colnames(aux3) <- c(colnames(aux3)[1],
                    "time_" %>% paste(colnames(aux3)[-1], sep=""))

extras <- data %>%
  select(alias_int, escolaridade) %>%
  group_by(alias_int) %>%
  summarise_all(funs(first(na.omit(.))))

# Join all of stan parameters into single DB.
stan_vars <- aux1 %>% 
  inner_join(aux2, by = "alias_int") %>% 
  inner_join(aux3, by = "alias_int") %>%
  inner_join(extras, by = "alias_int")
has_na <- (stan_vars %>% is.na() %>% rowMeans() > 0) %>% which()
stan_vars <- stan_vars[-has_na,]

alias_data <- data %>% 
  select(alias_int, player_alias) %>% 
  distinct()
stan_vars %<>% inner_join(alias_data, by = "alias_int")

# Join with clinical variables
clinical_data <- read_csv("./data-raw/clinical.csv")

complete_data <- inner_join(stan_vars, clinical_data, by = "player_alias") %>% 
  mutate(tug_cost = (tug_st - tug_dt)/tug_st) %>% 
  na.omit()

# Transform response variables into boolean
resp_variables = c("dgi", "tug_cost")
for(tvar in resp_variables)
{
  tvar_lgl = paste(tvar, "lgl", sep = "_")
  t_median = median(complete_data[[tvar]])
  prop_1 = abs(mean(complete_data[[tvar]] >= t_median) - 0.5)
  prop_2 = abs(mean(complete_data[[tvar]] > t_median) - 0.5)
  if(prop_1 < prop_2) complete_data[[tvar_lgl]] = complete_data[[tvar]] >= t_median
  else complete_data[[tvar_lgl]] = complete_data[[tvar]] > t_median
}

write_rds(complete_data, "./data/complete_data.rds")
