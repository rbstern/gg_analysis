dt = read_rds("./data/complete_data.rds")
gg_variables = c("beta_3", "beta_4", "beta_7",
                 "gamma_3", "gamma_4", "gamma_7",
                 "time_3", "time_4", "time_7")

########################
# Correlation analysis #
########################

test = cor.test(dt$moca_total, dt$dgi, method = "pearson")
cor_table = c("moca_total", test$estimate, test$p.value)
for(gg_var in gg_variables)
{
  test = cor.test(dt[[gg_var]], dt$dgi, method = "pearson")
  cor_table = rbind(cor_table, c(gg_var, test$estimate, test$p.value))
}
cor_table %<>% 
  as.data.frame() %>%
  as_tibble() %>%
  select(var = V1,
         cor,
         p_value = V3) %>% 
  mutate(p_value = round(as.numeric(as.character(p_value)), 3),
         cor = round(as.numeric(as.character(cor)), 3))

#######################
# Regression analysis #
#######################

#beta_7   -3.4  0.00716 ** 
#gamma_7   7.6  0.00157 **
dt %>% 
  select(gg_variables, dgi) %>% 
  lm(dgi~., data = .) %>% 
  summary()

#moca_total  0.2124 0.0673
dt %>% 
  select(moca_total, dgi) %>% 
  lm(dgi~., data = .) %>% 
  summary()

# GG means
# beta_3  beta_4  beta_7 gamma_3 gamma_4 gamma_7  time_3  time_4  time_7 
# 1.74    1.76    2.00    0.68    0.64    0.78    4.47    3.28    2.52 
dt %>% 
  select(gg_variables) %>% 
  colMeans() %>% 
  round(2)

# GG deviations
# beta_3  beta_4  beta_7 gamma_3 gamma_4 gamma_7  time_3  time_4  time_7 
# 0.53    0.38    0.37    0.16    0.14    0.18    6.21    3.78    2.40 
dt %>% 
  select(gg_variables) %>% 
  apply(2, sd) %>% 
  round(2)
