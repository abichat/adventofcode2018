library(tidyverse)

#### Input ####

raw_input <- readLines("input_12.txt")

initial_state <-
  raw_input[1] %>%
  str_remove("initial state: ")

df_rules <-
  raw_input[- (1:2)] %>% 
  tibble(raw = .) %>% 
  separate(col = raw, into = c("seq", "result"), sep = " => ")


#### Part One ####

G <- 20

first_pot <- 0
current_state <- initial_state

for(i in 1:G) {
  current_state <-
    current_state %>%
    str_c("....", ., "....")
  
  df_current <-
    current_state %>%
    str_count() %>%
    {3:(. - 3)} %>%
    tibble(pos = .) %>%
    mutate(pot_number =  pos - 5 + first_pot,
           seq = str_sub(current_state, start = pos - 2, end = pos + 2)) %>%
    left_join(df_rules, by = "seq")
  
  first_pot <-
    df_current %>%
    filter(result == "#") %>%
    pull(pot_number) %>%
    first()
  
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


#### Part One and Two ####

G <- 50000000000

first_pot <- 0
current_state <- initial_state

seq_sum <- rep(NA, 1000)
i <- 1


while (i < G) {
  current_state <-
    current_state %>%
    str_c("....", ., "....")
  
  df_current <-
    current_state %>%
    str_count() %>%
    {3:(. - 3)} %>%
    tibble(pos = .) %>%
    mutate(pot_number =  pos - 5 + first_pot,
           seq = str_sub(current_state, start = pos - 2, end = pos + 2)) %>%
    left_join(df_rules, by = "seq")
  
  current_pot_position <-
    df_current %>%
    filter(result == "#") %>%
    pull(pot_number)
  
  first_pot <- current_pot_position[1]
  seq_sum[i] <- sum(current_pot_position)
  
  if (i > 200) {
    if (all(diff(seq_sum[(i - 100):i]) == seq_sum[i] - seq_sum[i - 1])) {
      increment <- seq_sum[i] - seq_sum[i - 1]
      break
    }
  }
  
  current_state <-
    df_current %>%
    pull(result) %>%
    str_c(collapse = "") %>%
    str_remove("^\\.*") %>%
    str_remove("\\.*$")
  
  i <- i + 1
}

seq_sum[i] + 81 * (G - i)
