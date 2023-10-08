setwd("~/Desktop/summer23_seg_proj/phono_acq_wordseg/")

source("./Pearl_analysis.R")
library(caret)


kfold <- function(df,rand_seed) {
  # Specify type of training method used and the number of folds
  ctrlspecs <<- trainControl(method="cv", 
                            number=10, 
                            savePredictions="all",
                            classProbs=TRUE)
  
  # Set random seed for subsequent random selection and assignment operations
  set.seed(rand_seed)
  
  # Specify logistic regression model to be estimated using training data
  # and k-fold cross-validation process
  model1 <- train(List ~ scaled_uni_prob * scaled_bi_prob_smoothed, data=df_tp, 
                  method="glm", 
                  family="binomial", 
                  trControl=ctrlspecs)
  
  # Print information about model
  print(model1)
  summary(model1)
  
  # predictions
  preds_df <- model1$pred %>%
    mutate(Hits = ifelse(pred == obs, 1, 0)) %>%
    group_by(Resample) %>%
    summarise(accuracy = sum(Hits)/n()) 
  
  return(preds_df)
}

# DFs
df_tp = filter(exp3_full_df, model == 'tp')
df_unseg = filter(exp3_full_df, model == 'unseg')
df_dibs = filter(exp3_full_df, model == 'dibs')
df_ag = filter(exp3_full_df, model == 'ag')
df_puddle = filter(exp3_full_df, model == 'puddle')
df_baseline = filter(exp3_full_df, model == 'baseline')


# accuracy scores
acc_tp = kfold(df_tp, 49) 
acc_dibs = kfold(df_dibs, 824)
acc_ag = kfold(df_ag, 12)
acc_puddle = kfold(df_puddle, 40)
acc_baseline = kfold(df_baseline, 54)
acc_unseg = kfold(df_unseg, 33)


## Baseline
# Set random seed for subsequent random selection and assignment operations
set.seed(198)

base_mod <- df_dibs %>%
  mutate(baseline = 1) %>%
  train(List ~ baseline, data=., 
                method="glm", 
                family="binomial", 
                trControl=ctrlspecs)

print(base_mod)
summary(base_mod)
preds_base <- base_mod$pred %>%
  mutate(Hits = ifelse(pred == obs, 1, 0)) %>%
  group_by(Resample) %>%
  summarise(accuracy = sum(Hits)/n()) 

# I couldn't find a way to implement a baseline model with the train function 
# so I went with a dummy variable




format_acc <- function(df, model_str) {
  format_df <- df %>%
    mutate(model = model_str)
  return(format_df)
}

comb_acc <- list(format_acc(acc_tp, 'tp'), format_acc(acc_dibs, 'dibs'), 
                 format_acc(acc_ag, 'ag'), format_acc(acc_puddle, 'puddle'),
                 format_acc(acc_baseline, 'baseline'), 
                 format_acc(acc_unseg, 'unseg'), 
                 format_acc(preds_base, 'no pred')) %>%
  reduce(full_join)

ggplot(data = comb_acc, aes(x = model, y = accuracy)) +
  geom_boxplot() 


### TODO: add in gold seg baseline 


# # baseline model 
# base_mod <- nullModel(y=df_tp$List, data=df_tp, 
#                   method="glm", 
#                   family="binomial", 
#                   trControl=ctrlspecs)
# print(base_mod)
# summary(base_mod)

