library(tidyverse)

#### Input ####

nodes <- 
  readLines("input_08.txt") %>% 
  str_split(pattern = " ", simplify = TRUE) %>% 
  as.vector() %>% 
  as.numeric()


#### Part One ####

S <- 0
pos <- 1

browse_node <- function(){
  n_child <- nodes[pos]
  pos <<- pos + 1
  n_metadata <- nodes[pos]
  
  if (n_child > 0) {
    for (i in 1:n_child) {
      pos <<- pos + 1
      browse_node()
    }
  }
  
  if (n_metadata > 0) {
    S <<- sum(S, nodes[pos + 1:n_metadata])
  }
  
  pos <<- pos + n_metadata
}

browse_node()
S


#### Part Two ####

