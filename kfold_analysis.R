library(tidyverse)
library(stringr)
library(caret)

### performing a kfold linear regression analysis on stimuli sets 2a and 2b
### for each of the models 

# load in Pearl ngram out files 
# load in infant stimuli dataset with high vs low distinction 
setwd("~/Desktop/summer23_seg_proj/phono_acq_wordseg/Pearl_ngram_out/")

temp = list.files(pattern="*.txt")
for (i in 1:length(temp)) assign(str_split_fixed(temp[i], "\\.", 2)[,1], # lopping off the .txt
                                 read.table(temp[i], sep = ",", header = TRUE)) 

setwd("~/Desktop/summer23_seg_proj/phono_acq_wordseg/original_infant_data/")

temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(str_split_fixed(temp[i], "_s", 2)[,1], 
                                 read.csv(temp[i])) 

infant_3 <- infant_3 %>% rename(X = "...1")

# standardize Klatbet 
rev_infant_2a = infant_2a %>%
  mutate(word = gsub("@", "A", Klattbet))

rev_infant_2b = infant_2b %>%
  mutate(word = gsub("@", "A", Klattbet))


### formatting the data so the klatbet matches
# format output data and join with infant 2a
create_df2a = function(df, df_str) {
  rev_df = df %>%
    mutate(word = gsub(' ', '', word),
           model = df_str,
           scaled_bi_prob_smoothed = scale(bi_prob_smoothed),
           scaled_uni_prob = scale(uni_prob), 
           scaled_pos_uni_score = scale(pos_uni_score),
           scaled_pos_bi_score = scale(pos_bi_score))
  
  join_df <- inner_join(rev_df[1:132,], rev_infant_2a) %>%
    mutate(List = as.factor(List))
  return(join_df)
}

df2a_ag = create_df2a(ag, "ag")
df2a_baseline = create_df2a(baseline, "baseline")
df2a_dibs = create_df2a(dibs, "dibs")
df2a_dpseg = create_df2a(dpseg, "dpseg")
df2a_puddle = create_df2a(puddle, "puddle")
df2a_tp = create_df2a(tp, "tp")
df2a_unseg = create_df2a(unseg, "unseg")
df2a_gold = create_df2a(gold_seg, "gold")

# format output data and join with infant 2b
create_df2b = function(df, df_str) {
  rev_df = df %>%
    mutate(word = gsub(' ', '', word),
           model = df_str,
           scaled_bi_prob_smoothed = scale(bi_prob_smoothed),
           scaled_uni_prob = scale(uni_prob), 
           scaled_pos_uni_score = scale(pos_uni_score),
           scaled_pos_bi_score = scale(pos_bi_score))
  
  join_df <- inner_join(rev_df[133:264,], rev_infant_2b) %>%
    mutate(List = as.factor(List))
  return(join_df)
}

# TODO comb create_df2a and create_df2b functions

df2b_ag = create_df2b(ag, "ag")
df2b_baseline = create_df2b(baseline, "baseline")
df2b_dibs = create_df2b(dibs, "dibs")
df2b_dpseg = create_df2b(dpseg, "dpseg")
df2b_puddle = create_df2b(puddle, "puddle")
df2b_tp = create_df2b(tp, "tp")
df2b_unseg = create_df2b(unseg, "unseg")
df2b_gold = create_df2b(gold_seg, "gold")



## analysis 
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
  model1 <<- train(List ~ scaled_uni_prob * scaled_bi_prob_smoothed, data=df, 
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


# TODO: function for creating comb_acc df for both 2a and 2b 
# 2a accuracy scores
acc_tp_2a = kfold(df2a_tp, 49) 
acc_dibs_2a = kfold(df2a_dibs, 49)
acc_ag_2a = kfold(df2a_ag, 49)
acc_puddle_2a = kfold(df2a_puddle, 49)
acc_baseline_2a = kfold(df2a_baseline, 49)
acc_unseg_2a = kfold(df2a_unseg, 49)
acc_gold_2a = kfold(df2a_gold, 49)

format_acc <- function(df, model_str) {
  format_df <- df %>%
    mutate(model = model_str)
  return(format_df)
}

comb_acc_2a <- list(format_acc(acc_tp_2a, 'tp'), format_acc(acc_dibs_2a, 'dibs'), 
                 format_acc(acc_ag_2a, 'ag'), format_acc(acc_puddle_2a, 'puddle'),
                 format_acc(acc_baseline_2a, 'baseline'), 
                 format_acc(acc_unseg_2a, 'unseg'), 
                 format_acc(acc_gold_2a, 'gold')) %>%
  reduce(full_join)

ggplot(data = comb_acc_2a, aes(x = model, y = accuracy)) +
  geom_boxplot() 


p_2a <- ggplot(data = comb_acc_2a, aes(x = model, y = accuracy)) + 
  geom_point(stat = "summary", fun = "mean", size = 2) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5) +
  ylim(0,1) +
  ggtitle("Stimuli 2a")
         
mean_ci(acc_gold_2a$accuracy, ci = .95)
sd(acc_gold_2a$accuracy)


# pdf("../accuracy_inf2a.pdf")
# p_2a
# dev.off()



# 2b accuracy scores
acc_tp_2b = kfold(df2b_tp, 49) 
acc_dibs_2b = kfold(df2b_dibs, 49)
acc_ag_2b = kfold(df2b_ag, 49)
acc_puddle_2b = kfold(df2b_puddle, 49)
acc_baseline_2b = kfold(df2b_baseline, 49)
acc_unseg_2b = kfold(df2b_unseg, 49)
acc_gold_2b = kfold(df2b_gold, 49)


comb_acc_2b <- list(format_acc(acc_tp_2b, 'tp'), format_acc(acc_dibs_2b, 'dibs'), 
                    format_acc(acc_ag_2b, 'ag'), format_acc(acc_puddle_2b, 'puddle'),
                    format_acc(acc_baseline_2b, 'baseline'), 
                    format_acc(acc_unseg_2b, 'unseg'), 
                    format_acc(acc_gold_2b, 'gold')) %>%
  reduce(full_join)

ggplot(data = comb_acc_2b, aes(x = model, y = accuracy)) +
  geom_boxplot() 


p_2b <- ggplot(data = comb_acc_2b, aes(x = model, y = accuracy)) + 
  geom_point(stat = "summary", fun = "mean", size = 2) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5) +
  ylim(0,1) +
  ggtitle("Stimuli 2b")



# pdf("../accuracy_inf2b.pdf")
# p_2b
# dev.off()







### positional scores analysis
kfold_pos <- function(df,rand_seed) {
  # Specify type of training method used and the number of folds
  ctrlspecs <<- trainControl(method="cv", 
                             number=10, 
                             savePredictions="all",
                             classProbs=TRUE)
  
  # Set random seed for subsequent random selection and assignment operations
  set.seed(rand_seed)
  
  # Specify logistic regression model to be estimated using training data
  # and k-fold cross-validation process
  model1 <<- train(List ~ scaled_pos_uni_score * scaled_pos_bi_score, data=df, 
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


# TODO: function for creating comb_acc df for both 2a and 2b 
# 2a accuracy scores
acc_tp_2a_pos = kfold_pos(df2a_tp, 49) 
acc_dibs_2a_pos = kfold_pos(df2a_dibs, 49)
acc_ag_2a_pos = kfold_pos(df2a_ag, 49)
acc_puddle_2a_pos = kfold_pos(df2a_puddle, 49)
acc_baseline_2a_pos = kfold_pos(df2a_baseline, 49)
acc_unseg_2a_pos = kfold_pos(df2a_unseg, 49)
acc_gold_2a_pos = kfold_pos(df2a_gold, 49)

comb_acc_2a_pos <- list(format_acc(acc_tp_2a_pos, 'tp'), format_acc(acc_dibs_2a_pos, 'dibs'), 
                    format_acc(acc_ag_2a_pos, 'ag'), format_acc(acc_puddle_2a_pos, 'puddle'),
                    format_acc(acc_baseline_2a_pos, 'baseline'), 
                    format_acc(acc_unseg_2a_pos, 'unseg'), 
                    format_acc(acc_gold_2a_pos, 'gold')) %>%
  reduce(full_join)

ggplot(data = comb_acc_2a_pos, aes(x = model, y = accuracy)) +
  geom_boxplot() 


p_2a_pos <- ggplot(data = comb_acc_2a_pos, aes(x = model, y = accuracy)) + 
  geom_point(stat = "summary", fun = "mean", size = 2) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5) +
  ylim(0,1) +
  ggtitle("Stimuli 2a Positional Scores")




# 2b accuracy scores
acc_tp_2b_pos = kfold_pos(df2b_tp, 49) 
acc_dibs_2b_pos = kfold_pos(df2b_dibs, 49)
acc_ag_2b_pos = kfold_pos(df2b_ag, 49)
acc_puddle_2b_pos = kfold_pos(df2b_puddle, 49)
acc_baseline_2b_pos = kfold_pos(df2b_baseline, 49)
acc_unseg_2b_pos = kfold_pos(df2b_unseg, 49)
acc_gold_2b_pos = kfold_pos(df2b_gold, 49)


comb_acc_2b_pos <- list(format_acc(acc_tp_2b_pos, 'tp'), format_acc(acc_dibs_2b_pos, 'dibs'), 
                    format_acc(acc_ag_2b_pos, 'ag'), format_acc(acc_puddle_2b_pos, 'puddle'),
                    format_acc(acc_baseline_2b_pos, 'baseline'), 
                    format_acc(acc_unseg_2b_pos, 'unseg'), 
                    format_acc(acc_gold_2b_pos, 'gold')) %>%
  reduce(full_join)

ggplot(data = comb_acc_2b_pos, aes(x = model, y = accuracy)) +
  geom_boxplot() 


p_2b_pos <- ggplot(data = comb_acc_2b_pos, aes(x = model, y = accuracy)) + 
  geom_point(stat = "summary", fun = "mean", size = 2) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5) +
  ylim(0,1) +
  ggtitle("Stimuli 2b Positional Scores")














