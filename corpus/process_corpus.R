library(tidyverse)
setwd("E:/git_repos/phono_acq_wordseg/corpus")

pearl_corpus_raw <- read_csv("PearlCorpusPhonetic.txt", col_names = c("utterance"))

# Create gold corpus, tokenized
gold_token <- pearl_corpus_raw %>% 
  # Split each word into a separate row
  separate_longer_delim(utterance, ' ') %>%
  # Add spaces following each non-final character
  mutate(utterance = str_replace_all(utterance, '(?<=.)(?!$)', ' ' )) %>%
  # Convert to ARPABET
  to_arpabet() %>%
  mutate(utterance = str_replace_all(utterance, 'AO', 'AA'))

# List all unique symbols in the corpus to sanity check conversion
paste0(unique(unlist(strsplit(gold_token$utterance, ' '))), collapse = " ")

gold_token %>%
  write_csv('gold_token_final.txt', col_names = FALSE)
  
# Create gold corpus, typized
  gold_token %>%
    distinct() %>%
    write_csv('gold_type_final.txt', col_names = FALSE)
  
# Create unsegmented corpus
pearl_corpus_raw %>%
  # Add spaces following each non-final, non-space character
  mutate(utterance = str_replace_all(utterance, '(?<=[^ ])(?!( |$))', ' ')) %>%
  to_arpabet() %>%
  mutate(utterance = str_replace_all(utterance, 'AO', 'AA')) %>%
  write_csv("unseg_final.txt", col_names = FALSE)

to_arpabet <- function(df) {
  df %>%
    mutate(
      utterance = str_replace_all(
        utterance,
        # Consonants
        c(# Ordering is key here
          'T'='TH',
          'C'='CH',
          'J'='JH',
          'Z'='ZH',
          'D'='DH',
          'S'='SH',
          'G'='NG',
          'I'='IH',
          'E'='EH',
          'W'='AW',
          'Y'='AY',
          'O'='OY',
          'U'='UH',
          'R'='ER',
          'X'='ER',
          'p'='P',
          't'='T',
          'k'='K',
          'b'='B',
          'd'='D',
          'g'='G',
          'j'='JH',
          's'='S',
          'z'='Z',
          'f'='F',
          'v'='V',
          'h'='HH',
          'm'='M',
          'n'='N',
          'l'='L',
          'r'='R',
          'w'='W',
          'y'='Y',
          'i'='IY',
          'e'='EY',
          '@'='AE',
          'a'='AA',
          '//^'='AH',
          'c'='AO',
          'o'='OW',
          'u'='UW',
          'x'='AH'
        )
      )
    )
}

infant_2a <- read_csv("infant_2a_stimuli.csv") %>%
  mutate(Arpabet = str_replace(Arpabet, "AO", "AA")) %>%
  select(-Unigram, -Bigram) %>%
  write_csv('infant_2a_clean.csv')

infant_2b <- read_csv("infant_2b_stimuli.csv") %>%
  mutate(Arpabet = str_replace(Arpabet, "AO", "AA")) %>%
  select(-Unigram, -Bigram) %>%
  write_csv('infant_2b_clean.csv')

infant_3 <- read_csv("infant_3_stimuli.csv") %>%
  mutate(Arpabet = str_replace(Arpabet, "AO", "AA")) %>%
  select(-Unigram, -Bigram) %>%
  write_csv('infant_3_clean.csv')


