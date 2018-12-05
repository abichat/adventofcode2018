
#### Input ####

vec <- as.numeric(readLines("input_01.txt"))


#### Part One ####

sum(vec)


#### Part Two ####

N <- length(vec)

cs <- 0
i <- 1
s <- vec[1]

while(! s %in% cs){
  i <- i + 1
  cs <- c(cs, s)
  s <- s + vec[(i - 1) %% N + 1]
}

s

## Solution inspired from @mahendra-mariadassou

N <- length(vec)

cs <- new.env(hash = TRUE, emptyenv())
assign(as.character(0), value = NULL, envir = cs)

i <- 1
s <- vec[1]

while (! exists(as.character(s), cs)) {
  i <- i + 1
  assign(as.character(s), NULL, cs)
  s <- s + vec[(i - 1) %% N + 1]
}

s
