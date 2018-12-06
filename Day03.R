library(tidyverse)

#### Input ####

df_square <-
  read_table("input_03.txt", col_names = FALSE, col_types = "c") %>% 
  separate(X1, into = c("ID", "Xinf", "Yinf", "Xlength", "Ylength"), sep = c(" @ |,|: |x")) %>% 
  mutate(ID = str_sub(ID, start = 2)) %>% 
  mutate_all(as.numeric) %>% 
  mutate_at(vars(ends_with("inf")), ~ . + 1) %>% 
  mutate(Xsup = Xinf + Xlength - 1, Ysup = Yinf + Ylength - 1)


#### Part One ####

Xmax <- max(df_square$Xsup)
Ymax <- max(df_square$Ysup)

M <- matrix(0, nrow = Ymax, ncol = Xmax)

overlap <- function(Yinf, Ysup, Xinf, Xsup){
  M[Yinf:Ysup, Xinf:Xsup] <<- M[Yinf:Ysup, Xinf:Xsup] + 1
}

df_square %>% 
  select(-ID) %>% 
  select(-ends_with("length")) %>% 
  as.list() %>% 
  pwalk(overlap)

sum(M > 1)


#### Part Two ####

is_alone <- function(Yinf, Ysup, Xinf, Xsup){
  all(M[Yinf:Ysup, Xinf:Xsup] == 1)
}

df_square %>% 
  mutate(alone = pmap_lgl(list(Yinf, Ysup, Xinf, Xsup), is_alone)) %>% 
  filter(alone) %>% 
  pull(ID)
