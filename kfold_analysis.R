library(tidyverse)
library(stringr)
library(caret)
library(ggpubr)

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
# format output data and join with infant data
create_df = function(df, df_str, inf_df, range) { # range picks out which rows in the combined df correspond to the correct infant df stimuli. For example rows 1-132 are the results for the infant 2a stimuli
  rev_df = df %>%
    mutate(word = gsub(' ', '', word),
           model = df_str,
           scaled_bi_prob_smoothed = scale(bi_prob_smoothed),
           scaled_uni_prob = scale(uni_prob), 
           scaled_pos_uni_score_smoothed = scale(pos_uni_score_smoothed),
           scaled_pos_bi_score_smoothed = scale(pos_bi_score_smoothed))
  
  join_df <- inner_join(rev_df[range,], inf_df) %>%
    mutate(List = as.factor(List))
  return(join_df)
}

# results for testing on 2a set 
df2a_ag = create_df(ag, "ag", rev_infant_2a, 1:132)
df2a_baseline = create_df(baseline, "baseline", rev_infant_2a, 1:132)
df2a_dibs = create_df(dibs, "dibs", rev_infant_2a, 1:132)
df2a_dpseg = create_df(dpseg, "dpseg", rev_infant_2a, 1:132)
df2a_puddle = create_df(puddle, "puddle", rev_infant_2a, 1:132)
df2a_tp = create_df(tp, "tp", rev_infant_2a, 1:132)
df2a_unseg = create_df(unseg, "unseg", rev_infant_2a, 1:132)
df2a_gold = create_df(gold_seg, "gold", rev_infant_2a, 1:132)

# results for testing on 2b set 
df2b_ag = create_df(ag, "ag", rev_infant_2b, 133:264)
df2b_baseline = create_df(baseline, "baseline", rev_infant_2b, 133:264)
df2b_dibs = create_df(dibs, "dibs", rev_infant_2b, 133:264)
df2b_dpseg = create_df(dpseg, "dpseg", rev_infant_2b, 133:264)
df2b_puddle = create_df(puddle, "puddle", rev_infant_2b, 133:264)
df2b_tp = create_df(tp, "tp", rev_infant_2b, 133:264)
df2b_unseg = create_df(unseg, "unseg", rev_infant_2b, 133:264)
df2b_gold = create_df(gold_seg, "gold", rev_infant_2b, 133:264)



## analysis 
# kfold analysis for segmented output
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
  print(summary(model1))
  
  # predictions
  preds_df <- model1$pred %>%
    mutate(Hits = ifelse(pred == obs, 1, 0)) %>%
    group_by(Resample) %>%
    summarise(accuracy = sum(Hits)/n()) 
  
  return(preds_df)
}

# # baseline 
# set.seed(907)
# 
# train(List ~ 0, data=df2a_dibs,
#       method="glm",
#       family="binomial",
#       trControl=ctrlspecs)
# 
# base_mod <- df2a_dibs %>%
#   mutate(dummy = 0) %>%
#   train(List ~ dummy, data=., 
#         method="glm", 
#         family="binomial", 
#         trControl=ctrlspecs)
# 
# print(base_mod)
# summary(base_mod)
# preds_base <- base_mod$pred %>%
#   mutate(Hits = ifelse(pred == obs, 1, 0)) %>%
#   group_by(Resample) %>%
#   summarise(accuracy = sum(Hits)/n()) 

 
# 2a accuracy scores
acc_tp_2a = kfold(df2a_tp, 907) 
acc_dibs_2a = kfold(df2a_dibs, 907)
acc_ag_2a = kfold(df2a_ag, 907)
acc_puddle_2a = kfold(df2a_puddle, 907)
acc_baseline_2a = kfold(df2a_baseline, 907)
acc_unseg_2a = kfold(df2a_unseg, 907)
acc_gold_2a = kfold(df2a_gold, 907)

format_acc <- function(df, model_str) {
  format_df <- df %>%
    mutate(model = model_str)
  return(format_df)
}

comb_acc_2a <- list(format_acc(acc_tp_2a, 'tp'), format_acc(acc_dibs_2a, 'dibs'), 
                 format_acc(acc_ag_2a, 'ag'), format_acc(acc_puddle_2a, 'puddle'),
                 format_acc(acc_baseline_2a, 'random seg'), # renaming some of the models for clearer labels in the plots
                 format_acc(acc_unseg_2a, 'unseg'), 
                 format_acc(acc_gold_2a, 'gold seg')) %>%
  reduce(full_join) %>%
  dplyr::mutate(model = fct_relevel(model, "gold seg", 
                                    "unseg", "random seg", 
                                    "ag", "dibs", "puddle", "tp"),
                category = ifelse(model %in% c('gold seg', 
                                               'unseg'), 
                                  'base text', 
                                  'wordseg models'))



# 2b accuracy scores
acc_tp_2b = kfold(df2b_tp, 907) 
acc_dibs_2b = kfold(df2b_dibs, 907)
acc_ag_2b = kfold(df2b_ag, 907)
acc_puddle_2b = kfold(df2b_puddle, 907)
acc_baseline_2b = kfold(df2b_baseline, 907)
acc_unseg_2b = kfold(df2b_unseg, 907)
acc_gold_2b = kfold(df2b_gold, 907)


comb_acc_2b <- list(format_acc(acc_tp_2b, 'tp'), format_acc(acc_dibs_2b, 'dibs'), 
                    format_acc(acc_ag_2b, 'ag'), format_acc(acc_puddle_2b, 'puddle'),
                    format_acc(acc_baseline_2b, 'random seg'), # renaming some of the models for clearer labels in the plots
                    format_acc(acc_unseg_2b, 'unseg'), 
                    format_acc(acc_gold_2b, 'gold seg')) %>%
  reduce(full_join) %>%
  dplyr::mutate(model = fct_relevel(model, "gold seg", 
                                    "unseg", "random seg", 
                                    "ag", "dibs", "puddle", "tp"),
                category = ifelse(model %in% c('gold seg', 
                                               'unseg'), 'base text', 
                                  'wordseg models'))


##### baseline-- random High vs Low guessing 
## the ymin and ymax vary a lot from run to run so i am going to take an average
v_min=vector()
v_max=vector()
for (n in c(1:100)) {
  v=vector()
  for (i in c(1:132)) {
    v[i] = sample(x=c('High','Low'), size = 1)
  }
  # taking the model pred df and replacing the predictions with the random predictions
  rand_pred_acc <- model1$pred %>%
    select(!(pred)) %>%
    dplyr::mutate(pred = v) %>%
    mutate(Hits = ifelse(pred == obs, 1, 0)) %>%
    group_by(Resample) %>%
    summarise(accuracy = sum(Hits)/n()) 
  
  baseline_ci <- mean_ci(rand_pred_acc$accuracy, ci = .95)
  v_min[n] = baseline_ci$ymin 
  v_max[n] = baseline_ci$ymax 
}




### plots 
p_2a <- ggplot(data = comb_acc_2a, aes(x = model, y = accuracy)) + 
  geom_hline(yintercept = mean(v_min), linetype="dashed") +
  geom_hline(yintercept = mean(v_max), linetype="dashed") +
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5, aes(color = category)) +
  geom_point(stat = "summary", fun = "mean", size = 4, shape = 'square') +
  ylim(0,1) +
  ggtitle("Stimuli 2a Probability Scores") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

mean_ci(acc_gold_2a$accuracy, ci = .95)
sd(acc_gold_2a$accuracy)

p_2b <- ggplot(data = comb_acc_2b, aes(x = model, y = accuracy)) + 
  geom_hline(yintercept = mean(v_min), linetype="dashed") +
  geom_hline(yintercept = mean(v_max), linetype="dashed") +
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5, aes(color = category)) +
  geom_point(stat = "summary", fun = "mean", size = 4, shape = 'square') +
  ylim(0,1) +
  ggtitle("Stimuli 2b Probability Scores") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))




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
  model1 <<- train(List ~ scaled_pos_uni_score_smoothed * scaled_pos_bi_score_smoothed, data=df, 
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


# 2a accuracy scores
acc_tp_2a_pos = kfold_pos(df2a_tp, 907) 
acc_dibs_2a_pos = kfold_pos(df2a_dibs, 907)
acc_ag_2a_pos = kfold_pos(df2a_ag, 907)
acc_puddle_2a_pos = kfold_pos(df2a_puddle, 907)
acc_baseline_2a_pos = kfold_pos(df2a_baseline, 907)
acc_unseg_2a_pos = kfold_pos(df2a_unseg, 907)
acc_gold_2a_pos = kfold_pos(df2a_gold, 907)

comb_acc_2a_pos <- list(format_acc(acc_tp_2a_pos, 'tp'), format_acc(acc_dibs_2a_pos, 'dibs'), 
                        format_acc(acc_ag_2a_pos, 'ag'), format_acc(acc_puddle_2a_pos, 'puddle'),
                        format_acc(acc_baseline_2a_pos, 'random seg'), # renaming some of the models for clearer labels in the plots
                        format_acc(acc_unseg_2a_pos, 'unseg'), 
                        format_acc(acc_gold_2a_pos, 'gold seg')) %>%
  reduce(full_join) %>%
  dplyr::mutate(model = fct_relevel(model, "gold seg", 
                                    "unseg", "random seg", 
                                    "ag", "dibs", "puddle", "tp"),
                category = ifelse(model %in% c('gold seg', 
                                             'unseg'), 'base text', 
                                  'wordseg models'))



# 2b accuracy scores
acc_tp_2b_pos = kfold_pos(df2b_tp, 907) 
acc_dibs_2b_pos = kfold_pos(df2b_dibs, 907)
acc_ag_2b_pos = kfold_pos(df2b_ag, 907)
acc_puddle_2b_pos = kfold_pos(df2b_puddle, 907)
acc_baseline_2b_pos = kfold_pos(df2b_baseline, 907)
acc_unseg_2b_pos = kfold_pos(df2b_unseg, 907)
acc_gold_2b_pos = kfold_pos(df2b_gold, 907)


comb_acc_2b_pos <- list(format_acc(acc_tp_2b_pos, 'tp'), format_acc(acc_dibs_2b_pos, 'dibs'), 
                        format_acc(acc_ag_2b_pos, 'ag'), format_acc(acc_puddle_2b_pos, 'puddle'),
                        format_acc(acc_baseline_2b_pos, 'random seg'), # renaming some of the models for clearer labels in the plots
                        format_acc(acc_unseg_2b_pos, 'unseg'), 
                        format_acc(acc_gold_2b_pos, 'gold seg')) %>%
  reduce(full_join) %>%
  dplyr::mutate(model = fct_relevel(model, "gold seg", 
                                    "unseg", "random seg", 
                                    "ag", "dibs", "puddle", "tp"),
                category = ifelse(model %in% c('gold seg', 
                                               'unseg'), 'base text', 
                                  'wordseg models'))


### plots 
p_2a_pos <- ggplot(data = comb_acc_2a_pos, aes(x = model, y = accuracy)) + 
  geom_hline(yintercept = mean(v_min), linetype="dashed") +
  geom_hline(yintercept = mean(v_max), linetype="dashed") +
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5, aes(color = category)) +
  geom_point(stat = "summary", fun = "mean", size = 4, shape = 'square') +
  ylim(0,1) +
  ggtitle("Stimuli 2a Positional Scores") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

p_2b_pos <- ggplot(data = comb_acc_2b_pos, aes(x = model, y = accuracy)) + 
  geom_hline(yintercept = mean(v_min), linetype="dashed") +
  geom_hline(yintercept = mean(v_max), linetype="dashed") +
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5, aes(color = category)) +
  geom_point(stat = "summary", fun = "mean", size = 4, shape = 'square') +
  ylim(0,1) +
  ggtitle("Stimuli 2b Positional Scores") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# pdf("../accuracy_seed907.pdf")
p_2a
p_2a_pos
p_2b
p_2b_pos
# dev.off()


### combining the plots
df <- list(dplyr::mutate(comb_acc_2a, type = '2a Probability Scores'),
           dplyr::mutate(comb_acc_2a_pos, type = '2a Positional Scores'),
           dplyr::mutate(comb_acc_2b, type = '2b Probability Scores'),
           dplyr::mutate(comb_acc_2b_pos, type = '2b Positional Scores')) %>%
  reduce(full_join) %>%
  dplyr::mutate(type = fct_relevel(type, '2a Probability Scores', '2a Positional Scores', 
                                   '2b Probability Scores', '2b Positional Scores'))

p_facet <- ggplot(data = df, aes(x = model, y = accuracy)) + 
  geom_hline(yintercept = mean(v_min), linetype="dashed") +
  geom_hline(yintercept = mean(v_max), linetype="dashed") +
  geom_point(alpha = 0.5) +
  stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
               width = 0.5, aes(color = category)) +
  geom_point(stat = "summary", fun = "mean", size = 4, shape = 'square') +
  ylim(0,1) +
  # ggtitle("Stimuli 2b Positional Scores") +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  theme_bw() +
  facet_wrap(~type, ncol = 2) + 
  theme(legend.position = 'bottom', 
        axis.text=element_text(size=13),
        axis.title = element_text(size=18),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 17))
  # theme(plot.title = element_text(hjust = 0.5))

# ggsave("../accuracy_facet.pdf", plot = p_facet, width = 12, height = 8)




