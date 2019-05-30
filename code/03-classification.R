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
x = dt %>% select(gg_variables) %>% as.matrix()
y = dt %>% select(resp_var) %>% as.matrix() %>% as.factor()
aux <- cv.glmnet(x, y, family = "binomial",
                 keep = TRUE, nfolds=nrow(dt))
i <- which(aux$lambda == aux$lambda.min)
preds_gg <- prediction(aux$fit.preval[,i], dt[[resp_var]])
auc = performance(preds_gg, measure = "auc")@y.values[[1]]
t_acc = max(preds_gg@tp[[1]] + preds_gg@tn[[1]])
t_n = max(preds_gg@tp[[1]] + preds_gg@tn[[1]] + preds_gg@fp[[1]] + preds_gg@fn[[1]])
roc_gg <- performance(preds_gg, measure = "tpr", x.measure = "fpr") 
data_roc <- tibble("auc"     = auc,
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

# Logistic regression based on MoCA
preds_moca <- prediction(dt$moca_total, dt[[resp_var]])
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
data_acc = tibble(
  acc = t_acc/t_n,
  variable = resp_var,
  covariate = "MOCA"
)
data_acc_all %<>% rbind(data_acc)

# ROC curves for DGI
g <- data_roc_all %>%
  filter(variable == "dgi_lgl") %>%
  ggplot()+
  geom_line(aes(x = espec, y = sens, color = covariate), size = 1.2)+
  xlab("1-Specificity")+
  ylab("Sensitivity")+
  geom_abline(size = 1.2)+
  ggtitle("ROC curves for predicting the DGI score")
ggsave("./plots/gg-classify-dgi.pdf", height = 17, width = 14)

gg_roc <- roc(y, preds_gg@predictions[[1]])
moca_roc <- roc(y, preds_moca@predictions[[1]])
roc.test(gg_roc, moca_roc, paired = TRUE, alternative = "greater")

data_roc_all %>% 
  group_by(variable, covariate) %>% 
  summarise(auc_m = mean(auc))

