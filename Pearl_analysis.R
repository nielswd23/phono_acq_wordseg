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

comb_df <- list(rev_ag, rev_baseline, rev_dibs, rev_dpseg, rev_puddle, rev_tp) %>%
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


filter(full_df, Contrast == 'Unigram') %>%
  ggplot(data = ., aes(x=bi_prob_smoothed, fill=List)) +
  geom_histogram(alpha = 0.4, position = 'identity') +
  facet_wrap(~model)


### analyses 
## bi_prob_smoothed
full_df %>%
  filter(model == 'puddle' & Contrast == 'Bigram') %>%
  t.test(bi_prob_smoothed ~ List, data = .)

full_df %>%
  filter(model == 'ag' & Contrast == 'Bigram') %>%
  t.test(bi_prob_smoothed ~ List, data = .)

full_df %>%
  filter(model == 'tp' & Contrast == 'Bigram') %>%
  t.test(bi_prob_smoothed ~ List, data = .)

full_df %>%
  filter(model == 'baseline' & Contrast == 'Bigram') %>%
  t.test(bi_prob_smoothed ~ List, data = .)

full_df %>%
  filter(model == 'dibs' & Contrast == 'Bigram') %>%
  t.test(bi_prob_smoothed ~ List, data = .)

full_df %>%
  filter(model == 'dpseg' & Contrast == 'Bigram') %>%
  t.test(bi_prob_smoothed ~ List, data = .)

## uni_prob-- DUH all the same
full_df %>%
  filter(model == 'puddle' & Contrast == 'Bigram') %>%
  t.test(uni_prob ~ List, data = .)

full_df %>%
  filter(model == 'ag' & Contrast == 'Bigram') %>%
  t.test(uni_prob ~ List, data = .)

full_df %>%
  filter(model == 'tp' & Contrast == 'Bigram') %>%
  t.test(uni_prob ~ List, data = .)

full_df %>%
  filter(model == 'baseline' & Contrast == 'Bigram') %>%
  t.test(uni_prob ~ List, data = .)

full_df %>%
  filter(model == 'dibs' & Contrast == 'Bigram') %>%
  t.test(uni_prob ~ List, data = .)

full_df %>%
  filter(model == 'dpseg' & Contrast == 'Bigram') %>%
  t.test(uni_prob ~ List, data = .)





