library(tidyverse)

#### Input ####

nanobots <- 
  read_csv("input_23.txt", col_names = c("X", "Y", "Z", "r")) %>% 
  mutate_if(is.character, str_remove_all, "[a-z=<>]") %>% 
  mutate_if(is.character, as.numeric)

#### Part One ####


strongest <-
  nanobots %>% 
  pull(r) %>% 
  which.max()

pos <- as.numeric(nanobots[strongest, ])

nanobots %>% 
  mutate(distance = abs(X - pos[1]) + abs(Y - pos[2]) + abs(Z - pos[3]),
         inrange = distance <= pos[4]) %>% 
  pull(inrange) %>% 
  sum()
