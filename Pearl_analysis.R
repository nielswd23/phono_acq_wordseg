library(tidyverse)
library(stringr)


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

infant_df <- list(infant_2a, infant_2b, infant_3) %>%
  reduce(full_join)

# str_split_fixed("infant_2a_stimuli.csv", "_s", 2)
dpseg$word[1]

### formatting the data so the klatbet matches
# format output data 
rem_spaces = function(df, df_str) {
  rev_df = df %>%
    mutate(word = gsub(' ', '', word),
           model = df_str)
  return(rev_df)
}

rev_ag = rem_spaces(ag, "ag")
rev_baseline = rem_spaces(baseline, "baseline")
rev_dibs = rem_spaces(dibs, "dibs")
rev_dpseg = rem_spaces(dpseg, "dpseg")
rev_puddle = rem_spaces(puddle, "puddle")
rev_tp = rem_spaces(tp, "tp")
rev_unseg = rem_spaces(unseg, "unseg")
rev_gold = rem_spaces(gold_seg, "gold")

comb_df <- list(rev_ag, rev_baseline, rev_dibs, rev_dpseg, rev_puddle, rev_tp,
                rev_unseg, rev_gold) %>%
  reduce(full_join)

# format infant stimuli
rev_infant_df = infant_df %>%
  mutate(word = gsub("@", "A", Klattbet))


full_df <- comb_df %>% inner_join(rev_infant_df) 

# looking at -Inf values
df_inf <- comb_df %>%
  subset(bi_prob == -Inf)



### visualizing 
hists <- function(df, prob_measure) {
  d <- df %>%
    inner_join(rev_infant_df) 
  
  p <- ggplot(data = d, aes(x=.data[[prob_measure]], fill=List)) +
    geom_histogram(alpha = 0.4, position = 'identity') +
    facet_wrap(~ Contrast)
  
  return(p)
}

hists(rev_puddle, "bi_prob_smoothed")
hists(rev_puddle, "uni_prob")

hists(rev_ag, "uni_prob")


filter(full_df, Contrast == 'Bigram') %>%
  ggplot(data = ., aes(x=bi_prob_smoothed, fill=List)) +
  geom_histogram(alpha = 0.4, position = 'identity') +
  facet_wrap(~model)

filter(full_df, Contrast == 'Unigram') %>%
  ggplot(data = ., aes(x=bi_prob_smoothed, fill=List)) +
  geom_histogram(alpha = 0.4, position = 'identity') +
  facet_wrap(~model)


### analyses 
## bi_prob_smoothed
ttest <- function(model_str, contrast_str) {
  test <- full_df %>%
    filter(model == model_str & Contrast == contrast_str) %>%
    t.test(bi_prob_smoothed ~ List, data = .)
  return(test)
}

# Bigram
ttest('puddle', 'Bigram')
ttest('ag', 'Bigram')
ttest('tp', 'Bigram')
ttest('baseline', 'Bigram')
ttest('dibs', 'Bigram')
ttest('dpseg', 'Bigram')
ttest('unseg', 'Bigram')
ttest('gold', 'Bigram')

# Unigram 
ttest('puddle', 'Unigram')
ttest('ag', 'Unigram')
ttest('tp', 'Unigram')
ttest('baseline', 'Unigram')
ttest('dibs', 'Unigram')
ttest('dpseg', 'Unigram')
ttest('unseg', 'Unigram')
ttest('gold', 'Unigram')





### running analyses on experiment 3 stimuli 
filter_exp3 <- function(df) {
  new_df <- df %>%
    slice(265:396)
  return(new_df)
}

exp3_ag = filter_exp3(rev_ag)
exp3_baseline = filter_exp3(rev_baseline)
exp3_dibs = filter_exp3(rev_dibs)
exp3_dpseg = filter_exp3(rev_dpseg)
exp3_puddle = filter_exp3(rev_puddle)
exp3_tp = filter_exp3(rev_tp)
exp3_unseg = filter_exp3(rev_unseg)
exp3_gold = filter_exp3(rev_gold)

exp3_comb_df <- list(exp3_ag, exp3_baseline, exp3_dibs, exp3_dpseg, 
                     exp3_puddle, exp3_tp, exp3_unseg, exp3_gold) %>%
  reduce(full_join)

exp3_full_df <- rev_infant_df %>%
  slice(265:396) %>% 
  inner_join(exp3_comb_df, .) %>%
  # mutate(bin_list = case_when(List == 'High' ~ 1, List == 'Low' ~ 0)) %>% # take this out
  mutate(List = as.factor(List), 
         scaled_bi_prob_smoothed = scale(bi_prob_smoothed),
         scaled_uni_prob = scale(uni_prob))
# add scaled uni_prob


## plot with bi_prob_smoothed
# hist
filter(exp3_full_df, model != 'dpseg') %>%
  ggplot(aes(x=bi_prob_smoothed, fill=List)) +
  geom_histogram(alpha = 0.4, position = 'identity') +
  facet_wrap(~model)
# density
p_density <- filter(exp3_full_df, model != 'dpseg') %>%
  ggplot(aes(x=bi_prob_smoothed, fill=List)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~model)

# pdf("~/Desktop/summer23_seg_proj/phono_acq_wordseg/plots/Pearl_density.pdf")
# p_density
# dev.off()

## t tests
ttest_funct_exp3 <- function(model_str) {
  test = exp3_full_df %>%
    filter(model == model_str) %>%
    t.test(bi_prob_smoothed ~ List, data = .)
  return(test)
}

ttest_funct_exp3('puddle')
ttest_funct_exp3('ag')
ttest_funct_exp3('baseline')
ttest_funct_exp3('tp')
ttest_funct_exp3('dibs')
ttest_funct_exp3('unseg')



## logistic regression 
log_reg_funct <- function(model_str) {
  mod <- exp3_full_df %>%
    filter(model == model_str) %>%
    glm(List ~ scaled_bi_prob_smoothed, data = .,
        family = "binomial")
  return(mod)
}


log_reg_puddle = log_reg_funct('puddle')
log_reg_ag = log_reg_funct('ag')
log_reg_tp = log_reg_funct('tp')
log_reg_baseline = log_reg_funct('baseline')
log_reg_dpseg = log_reg_funct('dpseg')
log_reg_dibs = log_reg_funct('dibs')
log_reg_unseg = log_reg_funct('unseg')
log_reg_gold = log_reg_funct('gold')


summary(log_reg_tp)
summary(log_reg_puddle)
summary(log_reg_ag)
summary(log_reg_baseline)
summary(log_reg_dibs)
summary(log_reg_unseg)
summary(log_reg_gold)







## checking some strange labeling in original data. ycg listed as both low and high 
rev_infant_df %>%
  select(word, Contrast, List) %>%
  filter(Contrast == 'Unigram') %>%
  group_by(word) %>%
  count() %>%
  filter(n>1) 

rev_infant_df %>%
  filter(word == 'CiD')

