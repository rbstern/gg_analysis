library(Hmisc)
library(ordinal)
library(polycor)
library(tidyverse)
library(xtable)

dt = read_rds("./data/complete_data.rds")
gg_variables = c("beta_3", "beta_4", "beta_7",
                 "gamma_3", "gamma_4", "gamma_7",
                 "time_3", "time_4", "time_7")

########################
# Correlation analysis #
########################

# Partial correlations
# A tibble: 10 x 3
# 1 beta_3     -0.00222  0.988
# 2 beta_4     -0.125    0.344
# 3 beta_7     -0.122    0.334
# 4 gamma_3     0.242    0.062
# 5 gamma_4    -0.0624   0.628
# 6 gamma_7     0.387    0.001
# 7 time_3     -0.226    0.084
# 8 time_4     -0.208    0.127
# 9 time_7     -0.241    0.054
# 0 moca_total  0.195    0.08 

cor_table = tibble()
B = 1000
for(gg_var in gg_variables)
{
  errors = 0
  this_estimate = polyserial(dt[[gg_var]], dt$dgi)
  for(ii in 1:B)
  {
    new_permut = sample(1:nrow(dt), replace = FALSE)
    permut_estimate = abs(polyserial(dt[[gg_var]], dt$dgi[new_permut]))
    if(permut_estimate >= abs(this_estimate)) errors = errors + 1
  }
  cor_table = rbind(cor_table, tibble(variable = gg_var, 
                                      estimate = this_estimate,
                                      pvalue   = errors/B))
}

B = 100
errors = 0
this_estimate = polychor(dt$moca_total, dt$dgi)
for(ii in 1:B)
{
  new_permut = sample(1:nrow(dt), replace = FALSE)
  permut_estimate = abs(polychor(dt$moca_total, dt$dgi[new_permut]))
  if(permut_estimate > abs(this_estimate)) errors = errors + 1
}
cor_table = rbind(cor_table, tibble(variable = "moca_total", 
                                    estimate = this_estimate,
                                    pvalue   = errors/B))

#######################
# Regression analysis #
#######################

# beta_3  -0.430187   0.40434   
# beta_4  -0.253721   0.72960   
# beta_7  -2.569858   0.00711 **
# gamma_3  3.959327   0.03409 * 
# gamma_4 -3.608890   0.06483 . 
# gamma_7  5.285219   0.00305 **
# time_3  -0.031000   0.75142   
# time_4  -0.005414   0.96586   
# time_7  -0.112040   0.61301
dt %>% 
  select(gg_variables, dgi) %>% 
  mutate(dgi = ordered(dgi)) %>% 
  clm(dgi~., family = "binomial", data = .) %>% 
  summary()

  # moca_total  0.10922    0.104
dt %>% 
  select(moca_total, dgi) %>% 
  mutate(dgi = ordered(dgi)) %>% 
  clm(dgi~., data = .) %>% 
  summary()

# GG means
# beta_3  beta_4  beta_7 gamma_3 gamma_4 gamma_7  time_3  time_4  time_7 
# 1.74    1.76    2.00    0.68    0.64    0.78    4.47    3.28    2.52 
m_table = dt %>% 
  select(gg_variables) %>% 
  colMeans() %>% 
  round(2) %>% 
  as.matrix() %>% 
  t() %>% 
  as.tibble()

v_table = dt %>% 
  select(gg_variables) %>% 
  apply(2, sd) %>% 
  round(2) %>% 
  as.matrix() %>% 
  t() %>% 
  as.tibble()

rbind(m_table, v_table) %>% 
  xtable(type = "latex")
