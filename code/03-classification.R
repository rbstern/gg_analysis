library(boot)
library(glmnet)
library(magrittr)
library(pROC)
library(ROCR)
library(tidyverse)

dt = read_rds("./data/complete_data.rds")
gg_variables = c("beta_3", "beta_4", "beta_7",
                 "gamma_3", "gamma_4", "gamma_7",
                 "time_3", "time_4", "time_7")
resp_var = "dgi_lgl"
data_roc_all = NULL
data_acc_all = NULL

# Penalized logistic regression for GG variables
n = nrow(dt)
x = dt %>% select(gg_variables) %>% as.matrix()
y = dt %>% select(resp_var) %>% as.matrix() %>% as.factor()
# Cross validation
predictions_gg = rep(NA, n)
for(ii in 1:n)
{
  aux <- cv.glmnet(x[-ii,], y[-ii], family = "binomial",
                   keep = TRUE, nfolds = nrow(x[-ii,]))
  predictions_gg[ii] = predict(aux, x, s = "lambda.min")[ii]
  #predictions_gg[ii] = predict(aux, x)[ii]
}
# Accuracy and ROC
preds_gg <- prediction(predictions_gg, dt[[resp_var]])
auc = performance(preds_gg, measure = "auc")@y.values[[1]]
t_acc = max(preds_gg@tp[[1]] + preds_gg@tn[[1]])
t_n = max(preds_gg@tp[[1]] + preds_gg@tn[[1]] + preds_gg@fp[[1]] + preds_gg@fn[[1]])
roc_gg <- performance(preds_gg, measure = "tpr", x.measure = "fpr") 
data_roc <- tibble("auc"      = auc,
                   "espec"    = roc_gg@x.values[[1]],
                   "sens"     = roc_gg@y.values[[1]],
                   "variable" = resp_var,
                   "covariate"   = "GG")
data_roc_all %<>% rbind(data_roc)
data_acc = tibble(
  acc = t_acc/t_n,
  variable = resp_var,
  covariate = "GG"
)
data_acc_all %<>% rbind(data_acc)

# 9 x 1 sparse Matrix of class "dgCMatrix"
# s0
# beta_3   .          
# beta_4   0.356176224
# beta_7  -3.491689850
# gamma_3  0.917535171
# gamma_4  .          
# gamma_7  2.166144158
# time_3  -0.002545215
# time_4   .          
# time_7  -0.216721650
aux <- cv.glmnet(x, y, family = "binomial",
                 keep = TRUE, nfolds=nrow(x[-ii,]))
glmnet(x, y, family = "binomial", lambda = aux$lambda.min)$beta

# Logistic regression based on MoCA
predictions_moca = rep(NA, nrow(dt))
aux = dt %>% glm(dgi_lgl~moca_total, family = "binomial", data=.)
predictions_moca = predict(aux, dt)
preds_moca <- prediction(predictions_moca, dt[[resp_var]])
t_acc = max(preds_moca@tp[[1]] + preds_moca@tn[[1]])
t_n = max(preds_moca@tp[[1]] + preds_moca@tn[[1]] + preds_moca@fp[[1]] + preds_moca@fn[[1]])
auc = performance(preds_moca, measure = "auc")@y.values[[1]]
roc_moca <- performance(preds_moca, measure = "tpr", x.measure = "fpr") 
data_roc <- tibble("auc" = auc,
                   "espec"    = roc_moca@x.values[[1]],
                   "sens"     = roc_moca@y.values[[1]],
                   "variable" = resp_var,
                   "covariate"   = "MOCA")
data_roc_all %<>% rbind(data_roc)
data_acc = rbind(data_acc,
                 tibble(acc = t_acc/t_n,
                        variable = resp_var,
                        covariate = "MOCA")
)
data_acc_all %<>% rbind(data_acc)

# ROC curves for DGI
theme = theme_set(theme_minimal(base_size = 26))
theme = theme_update(legend.position="top")
g <- data_roc_all %>%
  filter(variable == "dgi_lgl") %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = covariate), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)+
  ggtitle("ROC curves for predicting the DGI score")
ggsave("./plots/gg-classify-dgi.pdf", height = 17, width = 14)

# covariate auc
# GG        0.652
# MOCA      0.507
data_roc_all %>% 
  group_by(variable, covariate) %>% 
  summarise(auc_m = mean(auc))

# p-value = 0.08429
gg_roc <- roc(y, preds_gg@predictions[[1]])
moca_roc <- roc(y, preds_moca@predictions[[1]])
roc.test(gg_roc, moca_roc, paired = TRUE, alternative = "greater")

compareROCdep(cbind(preds_gg@predictions[[1]], preds_moca@predictions[[1]]),
              t(y), method = "auc")
 
# p-value = 0.01866
gg_acc = data_acc %>% 
  filter(covariate == "GG") %>% 
  select(acc) %>% 
  unlist()
binom.test(x = round(gg_acc*nrow(dt)), n = nrow(dt))

# p-value = 0.5386
gg_moca = data_acc %>% 
  filter(covariate == "MOCA") %>% 
  select(acc) %>% 
  unlist()
binom.test(x = round(gg_moca*nrow(dt)), n = nrow(dt))
