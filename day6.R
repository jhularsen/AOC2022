library(tidyverse)
input <- str_split(read_lines("data/day6_input.txt"), "")[[1]]

# star1
fun <- function(x, y) {
  
  check <- 1
  i <- 0
  
  while (check != 0) {
    i <- i + 1
    check <- stri_duplicated_any(x[i:(i+y)])
  }
  i+y
}

fun(x = input, y = 3)

#star2
fun(x = input, y = 13)
