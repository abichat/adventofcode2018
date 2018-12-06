library(tidyverse)

#### Input ####

polymer <- readLines("input_05.txt")


#### Part One ####

pattern <- str_c(str_c(str_c(letters, LETTERS), "|", str_c(LETTERS, letters)), collapse = "|")

str_fully_react <- function(string) {
  b <- FALSE
  while (!b){
    l1 <- str_count(string)
    string <- str_remove_all(string, pattern)
    l2 <- str_count(string)
    b <- l1 == l2
    l1 <- l2
  }
  string
}

polymer %>% 
  str_fully_react() %>% 
  str_count()


#### Part Two ####

str_c(letters, LETTERS, sep = "|") %>% 
  tibble(pat = .) %>% 
  mutate(subtxt = str_remove_all(polymer, pat),
         subsubtxt = map(subtxt, str_fully_react),
         length = str_count(subsubtxt)) %>% 
  arrange(length) %>% 
  pull(length) %>% 
  first()
