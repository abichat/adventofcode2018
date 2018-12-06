library(tidyverse)

#### Input ####

words <- readLines("input_02.txt")


#### Part One ####

words %>%
  tibble(word = .) %>% 
  mutate(df_count = map(word, ~ tibble(letter = letters, count = str_count(., pattern = letters)))) %>% 
  unnest(df_count) %>% 
  filter(count %in% 2:3) %>% 
  distinct(word, count) %>% 
  count(count) %>% 
  pull(n) %>% 
  prod()


#### Part Two ####

N <- str_count(words[1])

tibble(position = 1:N) %>% 
  mutate(sub_words = map(position, ~ str_c(str_sub(words, 0, . - 1), str_sub(words, . + 1, N)))) %>% 
  unnest(sub_words) %>% 
  group_by(position, sub_words) %>%
  summarise(N = n()) %>%
  filter(N == 2) %>% 
  pull(sub_words)
  