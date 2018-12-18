library(tidyverse)

#### Input ####

acres <-
  readLines("input_18.txt") %>% 
  str_split("", simplify = TRUE)


#### Part One ####

size <- nrow(acres)

current_acres <- acres

get_around <- function(x, y) {
  str_c(current_acres[max(x-1, 1):min(x+1, size), max(y-1, 1):min(y +1, size)], 
        collapse = "")
}

update_acre <- function(x, y) {
  acre <- current_acres[x, y]
  around <- get_around(x, y)
  
  if (acre == ".") {
    if (str_count(around, "\\|") >= 3) {
      return("|")
    } else {
      return(".")
    }
  }
  
  if (acre == "|") {
    if (str_count(around, "#") >= 3) {
      return("#")
    } else {
      return("|")
    }
  }
  
  if (acre == "#") {
    if (str_count(around, "#") >= 2 & str_count(around, "\\|") >= 1) {
      return("#")
    } else {
      return(".")
    }
  }
}

for (i in 1:10) {
  current_acres <-
    crossing(x = 1:size, y = 1:size) %>%
    mutate(new = map2_chr(x, y, update_acre)) %>%
    pull(new) %>%
    matrix(nrow = size, byrow = TRUE)
}

current_acres %>%
  str_c(collapse = "") %>%
  str_count(c("\\|", "#")) %>%
  prod()


#### Part Two ####

current_acres <- acres

N <- 1000000000
product <- c(NA, N)
i <- 1


while (i < N) {
  current_acres <-
    crossing(x = 1:size, y = 1:size) %>%
    mutate(new = map2_chr(x, y, update_acre)) %>%
    pull(new) %>%
    matrix(nrow = size, byrow = TRUE)
  
  product[i]  <-
    current_acres %>%
    str_c(collapse = "") %>%
    str_count(c("\\|", "#")) %>%
    prod()
  
  if (i > 400) {
    minima <- c(min(product[(i-199):(i-150)]), min(product[(i-149):(i-100)]), 
                min(product[(i-99):(i-50)]), min(product[(i-49):i]))
    if (all(minima == minima[1])) {
      break
    }
  }
  
  i <- i + 1
}

new_origin <-
  which(product == minima[1]) %>%
  first()
period <- 
  which(product == minima[1]) %>%
  diff() %>%
  first()

product[(N - new_origin) %% period + new_origin] 

