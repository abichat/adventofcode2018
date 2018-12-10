library(tidyverse)

#### Input ####

df_position <- 
  readLines("input_10.txt") %>% 
  str_extract_all("[-0-9]+") %>% 
  transpose() %>% 
  `names<-`(c("X", "Y", "velocity_X", "velocity_Y")) %>% 
  as.tibble() %>% 
  mutate_all(unlist) %>% 
  mutate_all(as.numeric)


#### Part One and Two ####

lengthXmin <- Inf
lengthYmin <- Inf
iXmin <- 0
iYmin <- 0

for (i in 1:11000) {
  df_end <- 
    df_position %>% 
    mutate(Xend = X + i * velocity_X, Yend = Y + i * velocity_Y) %>% 
    select(Xend, Yend)
  
  Xlength <- diff(range(df_end$Xend))
  Ylength <- diff(range(df_end$Yend))
  
  if(Xlength < lengthXmin){
    lengthXmin <- Xlength
    iXmin <- i
  }
  
  if(Ylength < lengthYmin){
    lengthYmin <- Ylength
    iYmin <- i
  }
}

df_end <- 
  df_position %>% 
  mutate(Xend = X + iXmin * velocity_X, Yend = Y + iXmin * velocity_Y) %>% 
  select(Xend, Yend)

df_end %>% 
  ggplot(aes(x = Xend, y = Yend)) +
  geom_tile() + 
  scale_y_reverse() +
  theme_void()

iXmin
