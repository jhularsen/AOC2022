library(tidyverse)
input <- tibble(x = read_lines("data/day5_input.txt")) 

col1 <- c("D", "T", "W", "N", "L")
col2 <- c("H", "P", "C")
col3 <- c("J", "M", "G", "D", "N", "H", "P", "W")
col4 <- c("L", "Q", "T", "N", "S", "W", "C")
col5 <- c("N", "C", "H", "P")
col6 <- c("B", "Q", "W", "M", "D", "N", "H", "T")
col7 <- c("L", "S", "G", "J", "R", "B", "M")
col8 <- c("T", "R", "B", "V", "G", "W", "N", "Z")
col9 <- c("L", "P", "N", "D", "G", "W")

cols <- list(col1, col2, col3, col4, col5, col6, col7, col8, col9)

df <- input %>% 
  extract(x, c("move", "from", "to"),
          "move ([0-9]+) from ([0-9]+) to ([0-9]+)", convert = TRUE) 

# star1
fun <- function(data, puzzle = 1) {
  
  for (i in 1:nrow(df)) {
    
    move <- df$move[i]
    from <- df$from[i]
    to <- df$to[i]
    
    if (puzzle == 1) {
      moved <- rev(data[[from]][c(1:move)]) # reverse order in first puzzle
    } else if (puzzle == 2) {
      moved <- data[[from]][c(1:move)] 
    }
    
    data[[from]] <- data[[from]][-c(1:move)]
    data[[to]] <- c(moved, data[[to]])
  }
  
  data %>% 
    map_chr(first) %>% 
    paste(collapse = "")
}

fun(data = cols, puzzle = 1)

# star2
fun(data = cols, puzzle = 2)
