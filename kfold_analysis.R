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
           scaled_uni_prob = scale(uni_prob))
  
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
           List = as.factor(List), 
           scaled_bi_prob_smoothed = scale(bi_prob_smoothed),
           scaled_uni_prob = scale(uni_prob))
  
  join_df <- inner_join(rev_df[133:264,], rev_infant_2b) %>%
    mutate(List = as.factor(List))
  return(join_df)
}

df2b_ag = create_df2a(ag, "ag")
df2b_baseline = create_df2a(baseline, "baseline")
df2b_dibs = create_df2a(dibs, "dibs")
df2b_dpseg = create_df2a(dpseg, "dpseg")
df2b_puddle = create_df2a(puddle, "puddle")
df2b_tp = create_df2a(tp, "tp")
df2b_unseg = create_df2a(unseg, "unseg")
df2b_gold = create_df2a(gold_seg, "gold")



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


# TODO: add '2a' in the name. Then run on 2b and then use different scores (positional)
# 2a accuracy scores
acc_tp = kfold(df2a_tp, 49) 
acc_dibs = kfold(df2a_dibs, 49)
acc_ag = kfold(df2a_ag, 49)
acc_puddle = kfold(df2a_puddle, 49)
acc_baseline = kfold(df2a_baseline, 49)
acc_unseg = kfold(df2a_unseg, 49)
acc_gold = kfold(df2a_gold, 49)

format_acc <- function(df, model_str) {
  format_df <- df %>%
    mutate(model = model_str)
  return(format_df)
}

comb_acc <- list(format_acc(acc_tp, 'tp'), format_acc(acc_dibs, 'dibs'), 
                 format_acc(acc_ag, 'ag'), format_acc(acc_puddle, 'puddle'),
                 format_acc(acc_baseline, 'baseline'), 
                 format_acc(acc_unseg, 'unseg'), 
                 format_acc(acc_gold, 'gold')) %>%
  reduce(full_join)

ggplot(data = comb_acc, aes(x = model, y = accuracy)) +
  geom_boxplot() 


p <- ggplot(data = comb_acc, aes(x = model, y = accuracy)) + 
  geom_point(stat = "summary", fun = "mean", size = 2) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5) +
  ylim(0,1)
         


pdf("../accuracy_inf2a.pdf")
p
dev.off()
