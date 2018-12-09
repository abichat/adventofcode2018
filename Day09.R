library(tidyverse)

#### Input ####

parameters <- 
  readLines("input_09.txt") %>% 
  str_extract_all("[0-9]+") %>% 
  unlist() %>% 
  as.numeric()

n_elves <- parameters[1]
n_marbles <- parameters[2]
n_marbles <- 100 * n_marbles


#### Part One and Two ####

scores <- rep(0, n_elves)

i <- 1

current_elf <- 1
current_marble <- 1
current_position <- 2
circle <- c(0, 1)

while (current_marble < n_marbles) {
  i <- i + 1 
  
  current_elf <- (i-1) %% n_elves + 1
  
  if ((current_marble + 1) %% 23) {
    current_marble <- current_marble + 1
    current_position <- current_position %% length(circle) + 2
    
    if(current_position > length(circle)) {
      circle <- c(circle, current_marble)
    } else {
      circle <- c(circle[1:(current_position-1)], current_marble, circle[current_position:length(circle)])
    }
  } else {
    scores[current_elf] <- scores[current_elf] + current_marble + 1
    current_position <- (current_position-8) %% length(circle) +1
    scores[current_elf] <- scores[current_elf] + circle[current_position]
    circle <- circle[-current_position]
    current_marble <- current_marble + 1
  }
}

max(scores)
