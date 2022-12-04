library(tidyverse)
input <- tibble(x = read_lines("data/day4_input.txt")) 

# star1
df <- input %>% 
  extract(x, c("elf1_start", "elf1_end", "elf2_start", "elf2_end"), 
          "(.*)-(.*),(.*)-(.*)", convert = TRUE) %>% 
  mutate(elf1 = map2(elf1_start, elf1_end, seq),
         elf2 = map2(elf2_start, elf2_end, seq),
         intersect = map2(elf1, elf2, intersect)) %>% 
  rowwise() 

df %>% 
  filter(length(elf1) <= length(elf2) & all(elf1 %in% intersect) |
           length(elf1) > length(elf2) & all(elf2 %in% intersect)) %>% 
  nrow()

# star2
df %>% 
  filter(length(elf1) <= length(elf2) & any(elf1 %in% intersect) |
           length(elf1) > length(elf2) & any(elf2 %in% intersect)) %>% 
  nrow()