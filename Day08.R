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

browse_nodes <- function() {
  n_child <- nodes[pos]
  pos <<- pos + 1
  n_metadata <- nodes[pos]
  
  if (n_child > 0) {
    for (i in 1:n_child) {
      pos <<- pos + 1
      browse_nodes()
    }
  }
  
  if (n_metadata > 0) {
    S <<- sum(S, nodes[pos + 1:n_metadata])
  }
  
  pos <<- pos + n_metadata
}

browse_nodes()
S


#### Part Two ####

tree_table <- tibble(n_children = rep(NA_integer_, length(nodes)),
                     n_metadata = NA_integer_,
                     children = list(integer(0)),
                     metadata = list(integer(0)))

pos <- 1

fill_table <- function(){
  current_node <- pos
  tree_table$n_children[current_node] <<- nodes[pos]
  pos <<- pos + 1
  tree_table$n_metadata[current_node] <<- nodes[pos]
  
  if (tree_table$n_children[current_node] > 0) {
    for (i in 1:tree_table$n_children[current_node]) {
      pos <<- pos + 1
      tree_table$children[[current_node]] <<- c(tree_table$children[[current_node]], pos)
      fill_table()
    }
  }
  
  if (tree_table$n_metadata[current_node] > 0) {
    tree_table$metadata[[current_node]] <<- nodes[pos + 1:tree_table$n_metadata[current_node]]
  }
  
  pos <<- pos + tree_table$n_metadata[current_node]
}

fill_table()

S <- 0

browse_table <- function(pos) {
  children <- tree_table$children[[pos]]
  metadata <- tree_table$metadata[[pos]]
  
  if (length(children) == 0) {
    S <<- S + sum(metadata)
  } else {
    for (i in metadata) {
      if (i <= length(children)) {
        browse_table(children[i])
      }
    }
  }
}

browse_table(1)
S
