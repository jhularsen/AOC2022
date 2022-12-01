library(tidyverse)
input <- tibble(calories = as.integer(read_lines("day1_input.txt")))

# star 1
df <- input %>%
  mutate(elf = cumsum(is.na(calories))+1) %>%
  group_by(elf) %>% 
  summarise(total_calories = sum(calories, na.rm = TRUE))

df %>% 
  slice_max(total_calories, n = 1)

# star 2
df %>% 
  slice_max(total_calories, n = 3) %>% 
  summarise(res = sum(total_calories))