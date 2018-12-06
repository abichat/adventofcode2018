library(tidyverse)
library(lubridate)

#### Input ####

df_records <-
  read_csv("input_04.txt", col_names = FALSE, col_types = "c") %>% 
  separate(X1, into = c("datetime", "action"), sep = c("\\] ")) %>% 
  mutate(datetime = str_sub(datetime, start = 2),
         datetime = ymd_hm(datetime)) %>% 
  arrange(datetime)


#### Part One ####

df_sleeps <-
  df_records %>% 
  mutate(min = minute(datetime)) %>% 
  mutate(guard = str_detect(action, "Guard"), guardID = str_extract(action, "[0-9]+")) %>% 
  fill(guardID, .direction = "down") %>% 
  filter(!guard) %>% 
  mutate(wakeup_time = lead(min)) %>% 
  filter(action == "falls asleep") %>% 
  select(guardID, fallasleep_time = min, wakeup_time) 

guard_max <-
  df_sleeps %>% 
  mutate(sleep_duration = wakeup_time - fallasleep_time + 1) %>% 
  group_by(guardID) %>% 
  summarise(total = sum(sleep_duration)) %>% 
  top_n(1, total) %>% 
  pull(guardID) %>% 
  as.numeric()

is_sleeping <- function(begin, end){
  tibble(minute = 1:60) %>% 
    mutate(sleeping = between(minute, begin, end - 1))
}

minute_max <- 
  df_sleeps %>% 
  filter(guardID == guard_max) %>% 
  mutate(df_sleeping = map2(fallasleep_time, wakeup_time, is_sleeping)) %>% 
  unnest(df_sleeping) %>% 
  group_by(minute) %>% 
  summarise(total = sum(sleeping)) %>% 
  top_n(1, total) %>% 
  pull(minute) 
  
guard_max * minute_max


#### Part Two #### 

df_sleeps %>% 
  mutate(df_sleeping = map2(fallasleep_time, wakeup_time, is_sleeping)) %>% 
  unnest(df_sleeping) %>% 
  group_by(guardID, minute) %>% 
  summarise(total = sum(sleeping)) %>% 
  ungroup() %>%
  top_n(1, total) %>% 
  .[, 1:2] %>% 
  as.numeric() %>% 
  prod()
