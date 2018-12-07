library(tidyverse)

#### Input ####

tasks <-
  read_table("input_07.txt", col_names = FALSE, col_types = "-c-----c--") %>%
  rename(from = X2, to = X8)
  

#### Part One ####

parents <- function(node) {
  tasks %>%
    filter(to == node) %>%
    pull(from) %>%
    sort()
}

children <- function(node) {
  tasks %>%
    filter(from == node) %>%
    pull(to) %>%
    sort()
}

can_be_activated <- function(node) {
  p <- unlist(df_nodes[node, "parents"][[1]])
  df_nodes %>%
    filter(node %in% p) %>%
    pull(is_active) %>%
    all()
}

df_nodes <-
  c(tasks$from, tasks$to) %>%
  unique() %>%
  sort() %>%
  tibble(node = .) %>%
  mutate(children = map(node, children),
         parents = map(node, parents),
         is_root = map_lgl(parents, ~ length(.) == 0),
         is_active = FALSE) %>%
  `row.names<-`(. , .$node)

sequence <- ""

while (any(!df_nodes$is_active)) {
  node_to_activate <-
    df_nodes %>%
    filter(!is_active) %>%
    mutate(to_activate = map_lgl(node, can_be_activated)) %>%
    filter(to_activate) %>%
    pull(node) %>%
    first()
  df_nodes[node_to_activate, "is_active"] <- TRUE
  sequence <- str_c(sequence, node_to_activate)
}

sequence


#### Part Two ####

df_nodes <-
  df_nodes %>%
  select(node, children, parents, is_root) %>%
  mutate(is_active = FALSE,
         time = 1:n()) %>%
  mutate(time = time + 60) %>%
  `row.names<-`(. , .$node)

actualize_activation <- function() {
  df_nodes <<-
    df_nodes %>%
    mutate(to_activate = map_lgl(node, can_be_activated)) %>%
    `row.names<-`(. , .$node)
}

df_time <- tibble(second = 1:sum(df_nodes$time), W1 = NA_character_, W2 = NA_character_, 
                  W3 = NA_character_, W4 = NA_character_, W5 = NA_character_)

row <- 1
next_time <- numeric(0)

while (!all(df_nodes$is_active)) {
  available_workers <-
    df_time[row, c("W1", "W2", "W3", "W4", "W5")] %>%
    as.character() %>%
    is.na() %>%
    which()

  n_available_workers <- length(available_workers)
  actualize_activation()
  
  current_nodes <-
    df_time[row, c("W1", "W2", "W3", "W4", "W5")] %>% 
    as.character()
  
  df_current_tasks <-
    df_nodes %>%
    filter(to_activate, !is_active) %>%
    filter(!node %in% current_nodes) %>%
    top_n(n_available_workers, node) %>%
    select(node, time)
  
  next_time <- c(next_time, row + df_current_tasks$time)
  
  if (nrow(df_current_tasks) > 0) {
    for (task in 1:nrow(df_current_tasks)) {
      df_time[row:(row + df_current_tasks$time[task] - 1), 1 + available_workers[task]] <-
        df_current_tasks$node[task]
    }
  }
  
  row <- min(next_time)
  next_time <- setdiff(next_time, row)
  
  node_to_activate <-
    df_time[(row - 1):row, c("W1", "W2", "W3", "W4", "W5")] %>%
    as.matrix() %>%
    as.character() %>%
    table() %>%
    {. == 1} %>%
    which() %>%
    names()
  
  df_nodes[node_to_activate, "is_active"] <- TRUE
}

row - 1
