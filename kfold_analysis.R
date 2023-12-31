library(tidyverse)
library(stringr)
library(caret)
library(ggpubr)

### performing a kfold linear regression analysis on stimuli sets 2a and 2b
### for each of the models 


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

# format accuracy output to combine the dfs
format_acc <- function(df, model_str) {
  format_df <- df %>%
    mutate(model = model_str)
  return(format_df)
}


##### baseline-- random High vs Low guessing 
## the ymin and ymax vary a lot from run to run so i am going to take an average
baseline_model <- function(model) {
  v_min=vector()
  v_max=vector()
  for (n in c(1:100)) {
    v=vector()
    for (i in c(1:132)) {
      v[i] = sample(x=c('High','Low'), size = 1)
    }
    # taking the model pred df and replacing the predictions with the random predictions
    rand_pred_acc <- model$pred %>%
      select(!(pred)) %>%
      dplyr::mutate(pred = v) %>%
      mutate(Hits = ifelse(pred == obs, 1, 0)) %>%
      group_by(Resample) %>%
      summarise(accuracy = sum(Hits)/n()) 
    
    baseline_ci <- mean_ci(rand_pred_acc$accuracy, ci = .95)
    v_min[n] <- baseline_ci$ymin 
    v_max[n] <- baseline_ci$ymax 
  }
  df = data.frame(min = v_min, max = v_max)
  return(df)
}


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
  print(summary(model1))
  
  # predictions
  preds_df <- model1$pred %>%
    mutate(Hits = ifelse(pred == obs, 1, 0)) %>%
    group_by(Resample) %>%
    summarise(accuracy = sum(Hits)/n()) 
  
  return(preds_df)
}


### plots 
generate_plot <- function(acc_df, baseline_df, title) {
  p <- ggplot(data = acc_df, aes(x = model, y = accuracy)) + 
    geom_hline(yintercept = mean(baseline_df$min), linetype="dashed") +
    geom_hline(yintercept = mean(baseline_df$max), linetype="dashed") +
    geom_point(alpha = 0.5) +
    stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
                 width = 0.5, aes(color = category)) +
    geom_point(stat = "summary", fun = "mean", size = 4, shape = 'square') +
    ylim(0,1) +
    ggtitle(title) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}



### load in Pearl ngram out (types) files with original stimuli list 
# setwd("~/Desktop/summer23_seg_proj/phono_acq_wordseg/Pearl_ngram_out/")
setwd("~/Desktop/summer23_seg_proj/phono_acq_wordseg/Pearl_ngram_out_types/") # this is the analysis on word types with the original full stimuli list (2a, 2b, and 3)

temp = list.files(pattern="*.txt")
for (i in 1:length(temp)) assign(str_split_fixed(temp[i], "\\.", 2)[,1], # lopping off the .txt
                                 read.table(temp[i], sep = ",", header = TRUE)) 
# load infant stim list
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

### format data 
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

# write.csv(df2a_gold, "../dfs_for_Canaan/GoldSeg_2a.csv", row.names = FALSE)
# write.csv(df2b_gold, "../dfs_for_Canaan/GoldSeg_2b.csv", row.names = FALSE)
# write.csv(df2a_unseg, "../dfs_for_Canaan/Unseg_2a.csv", row.names = FALSE)
# write.csv(df2b_unseg, "../dfs_for_Canaan/Unseg_2b.csv", row.names = FALSE)


### Probability score results
# 2a accuracy scores
acc_tp_2a = kfold(df2a_tp, 905) 
acc_dibs_2a = kfold(df2a_dibs, 905)
acc_ag_2a = kfold(df2a_ag, 905)
acc_puddle_2a = kfold(df2a_puddle, 905)
acc_baseline_2a = kfold(df2a_baseline, 905)
acc_unseg_2a = kfold(df2a_unseg, 905)
acc_gold_2a = kfold(df2a_gold, 905)
# baseline 
baseline_2a = baseline_model(model1)

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
acc_tp_2b = kfold(df2b_tp, 905) 
acc_dibs_2b = kfold(df2b_dibs, 905)
acc_ag_2b = kfold(df2b_ag, 905)
acc_puddle_2b = kfold(df2b_puddle, 905)
acc_baseline_2b = kfold(df2b_baseline, 905)
acc_unseg_2b = kfold(df2b_unseg, 905)
acc_gold_2b = kfold(df2b_gold, 905)
# baseline 
baseline_2b = baseline_model(model1)

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


## positional score data
# 2a accuracy scores
acc_tp_2a_pos = kfold_pos(df2a_tp, 905) 
acc_dibs_2a_pos = kfold_pos(df2a_dibs, 905)
acc_ag_2a_pos = kfold_pos(df2a_ag, 905)
acc_puddle_2a_pos = kfold_pos(df2a_puddle, 905)
acc_baseline_2a_pos = kfold_pos(df2a_baseline, 905)
acc_unseg_2a_pos = kfold_pos(df2a_unseg, 905)
acc_gold_2a_pos = kfold_pos(df2a_gold, 905)
# baseline
baseline_2a_pos = baseline_model(model1)

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
acc_tp_2b_pos = kfold_pos(df2b_tp, 905) 
acc_dibs_2b_pos = kfold_pos(df2b_dibs, 905)
acc_ag_2b_pos = kfold_pos(df2b_ag, 905)
acc_puddle_2b_pos = kfold_pos(df2b_puddle, 905)
acc_baseline_2b_pos = kfold_pos(df2b_baseline, 905)
acc_unseg_2b_pos = kfold_pos(df2b_unseg, 905)
acc_gold_2b_pos = kfold_pos(df2b_gold, 905)
# baseline
baseline_2b_pos = baseline_model(model1)

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
# first running baseline 
generate_plot(comb_acc_2a, baseline_2a, "Stimuli 2a Probability Scores")
generate_plot(comb_acc_2b, baseline_2b, "Stimuli 2b Probability Scores")
generate_plot(comb_acc_2a_pos, baseline_2a_pos, "Stimuli 2a Positional Scores")
generate_plot(comb_acc_2b_pos, baseline_2b_pos, "Stimuli 2b Positional Scores")
# # pdf("../accuracy_seed905.pdf")
# p_2a
# p_2a_pos
# p_2b
# p_2b_pos
# # dev.off()




### replicating Breiss analysis 
# load in data 
df_gold_breiss <- read.csv("../Breiss_analysis/PearlWordTypes_scored.csv") %>%
  dplyr::mutate(scaled_bi_prob_smoothed = scale(bi_prob_smoothed),
                scaled_uni_prob = scale(uni_prob),
                scaled_pos_uni_score_smoothed = scale(pos_uni_score_smoothed),
                scaled_pos_bi_score_smoothed = scale(pos_bi_score_smoothed)) %>%
  distinct()
df_unseg_breiss <- read.csv("../Breiss_analysis/PearlUtteranceTypes_scored.csv") %>%
  dplyr::mutate(scaled_bi_prob_smoothed = scale(bi_prob_smoothed),
                scaled_uni_prob = scale(uni_prob),
                scaled_pos_uni_score_smoothed = scale(pos_uni_score_smoothed),
                scaled_pos_bi_score_smoothed = scale(pos_bi_score_smoothed)) %>%
  distinct()

# # seems to be a few duplicates? --- RESOLVED with distinct()
# df_gold_breiss[30,] == df_gold_breiss[29,]

## 2b (new Bigram contrast)
df_gold_breiss_2b = df_gold_breiss %>%
  filter(Contrast == 'NewBigram')
df_unseg_breiss_2b = df_unseg_breiss %>%
  filter(Contrast == 'NewBigram')

# kfold analysis
acc_gold_2b_breiss = kfold(df_gold_breiss_2b, 905)
acc_unseg_2b_breiss = kfold(df_unseg_breiss_2b, 905)
acc_gold_2b_breiss_pos = kfold_pos(df_gold_breiss_2b, 905)
acc_unseg_2b_breiss_pos = kfold_pos(df_unseg_breiss_2b, 905)

## 2a (Unigram contrast)
df_gold_breiss_2a = df_gold_breiss %>%
  filter(Contrast == 'Unigram')
df_unseg_breiss_2a = df_unseg_breiss %>%
  filter(Contrast == 'Unigram')

# kfold analysis
acc_gold_2a_breiss = kfold(df_gold_breiss_2a, 905)
acc_unseg_2a_breiss = kfold(df_unseg_breiss_2a, 905)
acc_gold_2a_breiss_pos = kfold_pos(df_gold_breiss_2a, 905)
acc_unseg_2a_breiss_pos = kfold_pos(df_unseg_breiss_2a, 905)


format_breiss <- function(df, model_str, StimuliSet_str, score_type_str) {
  new_df <- df %>%
    dplyr::mutate(model = model_str, stim = StimuliSet_str, 
                  score_type = score_type_str)
  return(new_df)
}

comb_acc_breiss <- list(format_breiss(acc_gold_2a_breiss, 'gold', '2a', 'probability'),
                        format_breiss(acc_unseg_2a_breiss, 'unseg', '2a', 'probability'),
                        format_breiss(acc_gold_2a_breiss_pos, 'gold', '2a', 'positional'),
                        format_breiss(acc_unseg_2a_breiss_pos, 'unseg', '2a', 'positional'),
                        format_breiss(acc_gold_2b_breiss, 'gold', '2b', 'probability'),
                        format_breiss(acc_unseg_2b_breiss, 'unseg', '2b', 'probability'),
                        format_breiss(acc_gold_2b_breiss_pos, 'gold', '2b', 'positional'),
                        format_breiss(acc_unseg_2b_breiss_pos, 'unseg', '2b', 'positional')) %>%
  reduce(full_join)

# plot with x axis model (gold vs unseg) y axis accuracy and grouped by 
# stimuli list (2a vs 2b) as a color and faceted by probability score vs positional
gold_unseg_plot <- function(df, title) {
  p <- ggplot(data = df, aes(x = model, y = accuracy, color = stim)) + 
    # geom_hline(yintercept = mean(baseline_df$min), linetype="dashed") +
    # geom_hline(yintercept = mean(baseline_df$max), linetype="dashed") +
    geom_point(alpha = 0.5, position = position_dodge(width = 0.6)) +
    stat_summary(fun.data = mean_ci, geom = "errorbar", linewidth = 1, 
                 width = 0.5, position = position_dodge(width = 0.6)) + # mean_ci is calculating a 95% confidence interval. but mean_ci(acc_ag$accuracy) is not 2 sd's away from the mean. maybe it removes outliers? 
    geom_point(stat = "summary", fun = "mean", size = 4, shape = 'square',
               position = position_dodge(width = 0.6)) +
    ylim(0,1) +
    facet_wrap(~score_type) +
    ggtitle(title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}


# comparison plots with our results
format_og_acc_to_breiss <- function(df, model_str, StimuliSet_str, score_type_str) {
  new_df <- df %>%
    filter(model == model_str) %>%
    dplyr::mutate(stim = StimuliSet_str, 
                  score_type = score_type_str)
  return(new_df)
}

og_acc_comb <- list(format_og_acc_to_breiss(comb_acc_2a, 'gold seg', '2a', 'probability'),
                    format_og_acc_to_breiss(comb_acc_2a, 'unseg', '2a', 'probability'),
                    format_og_acc_to_breiss(comb_acc_2b, 'gold seg', '2b', 'probability'),
                    format_og_acc_to_breiss(comb_acc_2b, 'unseg', '2b', 'probability'),
                    format_og_acc_to_breiss(comb_acc_2a_pos, 'gold seg', '2a', 'positional'),
                    format_og_acc_to_breiss(comb_acc_2a_pos, 'unseg', '2a', 'positional'),
                    format_og_acc_to_breiss(comb_acc_2b_pos, 'gold seg', '2b', 'positional'),
                    format_og_acc_to_breiss(comb_acc_2b_pos, 'unseg', '2b', 'positional')) %>%
  reduce(full_join)


# full og results with new plot 
df_og_full <- list(dplyr::mutate(comb_acc_2a, score_type = 'probability', stim = '2a'),
           dplyr::mutate(comb_acc_2a_pos, score_type = 'positional', stim = '2a'),
           dplyr::mutate(comb_acc_2b, score_type = 'probability', stim = '2b'),
           dplyr::mutate(comb_acc_2b_pos, score_type = 'positional', stim = '2b')) %>%
  reduce(full_join) 

p_full <- gold_unseg_plot(df_og_full, 'OG data run905')
side_by_side <- (gold_unseg_plot(comb_acc_breiss, "Breiss data run905") + 
    gold_unseg_plot(og_acc_comb, "OG data run905"))

ggsave(filename = "../plots/NewAnalysis905.png", plot = p_full, 
       width = 11, height = 7)
ggsave(filename = "../plots/SideBySide905.png", plot = side_by_side, 
       width = 9, height = 5)


#### OLD code 
# # things i've tried to get a baseline model 
# set.seed(905)
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





