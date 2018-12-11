library(tidyverse)

#### Input ####

serial_number <- as.numeric(readLines("input_11.txt"))
N <- 300


#### Part One ####

power_level <- function(x, y){
  rack_ID <- x + 10
  ((rack_ID * y + serial_number) * rack_ID) %/% 100 %% 10 - 5
}

total_power_level <- function(x, y){
  power_level(x, y) + power_level(x + 1, y) + power_level(x + 2, y) +
    power_level(x, y + 1) + power_level(x + 1, y + 1) + power_level(x + 2, y + 1) +
    power_level(x, y + 2) + power_level(x + 1, y + 2) + power_level(x + 2, y + 2)
}

pos <- which.max(outer(1:(N - 2), 1:(N - 2), total_power_level))
paste0(pos %% (N - 2), ',', pos %/% (N - 2) + 1)


#### Part Two ####

total_n_power_level <- function(x, y, n){
  sum(outer(x:(x+n), y:(y+n), power_level))
}

crossing(x = 1:N, y = 1:N, size = 0:(N-1)) %>% 
  filter(x + size <= N, y + size <= N) %>% 
  mutate(level = pmap_dbl(list(x = x, y = y, n = size), total_n_power_level),
         size = size + 1) %>% 
  arrange(desc(level)) %>% 
  select(-level) %>% 
  slice(1) %>% 
  paste(collapse = ",")
