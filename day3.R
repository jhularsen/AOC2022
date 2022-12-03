library(tidyverse)
input <- tibble(x = read_lines("data/day3_input.txt")) 

# star 1
input %>% 
  mutate(bag = row_number(),
         first_half = substring(x, 1, nchar(x)/2),
         second_half = substring(x, nchar(x)/2 + 1, nchar(x)),
         first_half_letters = str_extract_all(first_half, "[A-z]"),
         second_half_letters = str_extract_all(second_half, "[A-z]")) %>% 
  unnest(first_half_letters) %>% 
  unnest(second_half_letters) %>% 
  filter(first_half_letters == second_half_letters) %>% 
  unique() %>% 
  group_by(bag) %>% 
  mutate(prio = which(first_half_letters == c(letters, LETTERS))) %>% 
  ungroup() %>% 
  summarise(res = sum(prio))

# star 2
input %>% 
  mutate(bag = row_number(),
         group = (bag + 2) %/% 3) %>% 
  group_by(group) %>% 
  mutate(bag_group = c("bag1", "bag2", "bag3")) %>% 
  pivot_wider(id_cols = group, names_from = bag_group, values_from = x) %>% 
  mutate(first_bag = str_extract_all(bag1, "[A-z]"),
         second_bag = str_extract_all(bag2, "[A-z]"),
         third_bag = str_extract_all(bag3, "[A-z]")) %>% 
  unnest(first_bag) %>% 
  unnest(second_bag) %>% 
  unnest(third_bag) %>% 
  group_by(group) %>% 
  filter(first_bag == second_bag & first_bag == third_bag & second_bag == third_bag) %>% 
  unique() %>% 
  mutate(prio = which(first_bag == c(letters, LETTERS))) %>% 
  ungroup() %>% 
  summarise(res = sum(prio))

