library(tidyverse)

#### Input ####

coordinates <-
  read_csv("input_06.txt", col_names = c("X", "Y"), col_types = "ii") %>%
  mutate_all(~ . + 1) %>%
  rowid_to_column()


#### Part One ####

Xmax <- max(coordinates$X)
Ymax <- max(coordinates$Y)

M <- matrix(NA, nrow = Ymax + 1, ncol = Xmax + 1)

for (k in 1:nrow(coordinates)) {
  M[coordinates$Y[k], coordinates$X[k]] <- coordinates$rowid[k]
}

neighbours <- function(i, j) {
  unique(c(M[i, j + 1], M[i - 1, j], M[i, j - 1], M[i + 1, j]))
}

bool <- TRUE

while (bool) {
  M_ <- M
  bool <- FALSE
  for (i in 2:Ymax) {
    for (j in 2:Xmax) {
      if (is.na(M_[i, j])) {
        bool <- TRUE
        n <- neighbours(i, j)
        if (!all(is.na(n))) {
          n_num <- unique(n[!is.na(n)])
          if (length(n_num) == 1) {
            M_[i, j] <- n_num
          } else {
            M_[i, j] <- -1
          }
        }
      }
    }
  }
  M <- M_
}

infinite <- unique(c(M[2, 2:Xmax], M[2:Ymax, Xmax], M[Ymax, Xmax:2], M[Ymax:2, 2], -1))

M %>%
  table() %>%
  as.tibble() %>%
  rename(area = ".") %>%
  filter(!area %in% infinite) %>%
  arrange(n) %>%
  pull(n) %>%
  last()


#### Part Two ####

threshold <- 10000

expand.grid(i = 2:Ymax, j = 2:Xmax) %>%
  as.tibble() %>%
  mutate(coord = list(coordinates)) %>%
  unnest(coord) %>%
  mutate(dist = abs(i - X) + abs(j - Y)) %>%
  group_by(i, j) %>%
  summarise(tot_dist = sum(dist)) %>%
  filter(tot_dist < threshold) %>%
  nrow()
