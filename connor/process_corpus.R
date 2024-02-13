library(tidyverse)

# Function that collapses a few distinctions in Klattbet we don't care about
to_klattbet <- function(data_col) {
  str_replace_all(
        data_col,
        c('X'='R',
          'x'='\\^'
        ))
}

pearl_corpus_raw <- read_csv(
  "connor/PearlCorpusPhonetic.txt", col_names = c("utterance")
)

# Create gold corpus, tokenized
gold_token <- pearl_corpus_raw %>% 
  # Split each word into a separate row
  separate_longer_delim(utterance, ' ') %>%
  # Convert to Klattbet
  mutate(utterance = to_klattbet(utterance)) 

# Write type and token versions of gold segmentation
gold_token %>%
  write_csv('connor/gold_token_final.txt', col_names = FALSE) %>%
  distinct() %>%
  write_csv('connor/gold_type_final.txt', col_names = FALSE)
  
# Create unsegmented corpus and write type and token versions
pearl_corpus_raw %>%
  mutate(utterance = to_klattbet(utterance)) %>%
  mutate(utterance = str_remove_all(utterance, ' ')) %>%
  write_csv("connor/unseg_final_token.txt", col_names = FALSE) %>%
  distinct() %>%
  write_csv("connor/unseg_final_type.txt", col_names = FALSE)

# Convert Klattbet in experimental dfs 
df_2a <- read_csv('original_infant_data/infant_2a_stimuli.csv') %>% 
  mutate(Klattbet = to_klattbet(Klattbet)) %>%
  write_csv('connor/infant_2a_stimuli_final.csv')

df_2b <- read_csv('original_infant_data/infant_2b_stimuli.csv') %>% 
  mutate(Klattbet = to_klattbet(Klattbet)) %>%
  write_csv('connor/infant_2b_stimuli_final.csv')

df_3 <- read_csv('original_infant_data/infant_3_stimuli.csv') %>% 
  mutate(Klattbet = to_klattbet(Klattbet)) %>%
  write_csv('connor/infant_3_stimuli_final.csv')

