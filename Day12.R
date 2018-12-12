library(tidyverse)

#### Input ####

rawinput <- readLines("input_12.txt")

initial_state <-
  rawinput[1] %>%
  str_remove("initial state: ")

df_rules <-
  rawinput[- (1:2)] %>% 
  tibble(raw = .) %>% 
  separate(col = raw, into = c("seq", "result"), sep = " => ")

G <- 20
# G <- 50000000000 # Will take > 100 days


#### Part One and Two ####

S <- 0
first_pot <- 0
current_state <- initial_state

for(i in 1:G){
  current_state <- 
    current_state %>% 
    str_c("....", ., "....")
  
  df_current <- 
    current_state %>% 
    str_count() %>% 
    {3:(.-3)} %>% 
    tibble(pos = .) %>% 
    mutate(pot_number =  pos - 5 + first_pot,
           seq = str_sub(current_state, start = pos - 2, end = pos + 2)) %>% 
    left_join(df_rules, by = "seq")
  
  first_pot <-
    current_pot_position <-
    df_current %>% 
    filter(result == "#") %>% 
    pull(pot_number) %>% 
    first()
  
  first_pot <- current_pot_position[1]
  
  current_state <-
    df_current %>% 
    pull(result) %>% 
    str_c(collapse = "") %>% 
    str_remove("^\\.*") %>% 
    str_remove("\\.*$")
}

df_current %>% 
  filter(result == "#") %>% 
  pull(pot_number) %>% 
  sum()
