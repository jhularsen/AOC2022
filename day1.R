library(tidyverse)
input <- tibble(calories = as.integer(read_lines("data/day1_input.txt")))

# star 1
df <- input %>%
  group_by(elf = cumsum(is.na(calories))) %>% 
  summarise(total_calories = sum(calories, na.rm = TRUE))

df %>% slice_max(total_calories, n = 1)

# star 2
df %>% 
  slice_max(total_calories, n = 3) %>% 
  summarise(res = sum(total_calories))
