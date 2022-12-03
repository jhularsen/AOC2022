library(tidyverse)
input <- tibble(x = read_lines("data/day2_input.txt")) 

# star 1
input %>% 
  extract(x, c("opponent", "myself"), "([A-Z]) ([A-Z])", convert = TRUE) %>%
  mutate(outcome = case_when(opponent == "A" & myself == "X" ~ 3,
                             opponent == "A" & myself == "Y" ~ 6,
                             opponent == "A" & myself == "Z" ~ 0,
                             opponent == "B" & myself == "X" ~ 0,
                             opponent == "B" & myself == "Y" ~ 3,
                             opponent == "B" & myself == "Z" ~ 6,
                             opponent == "C" & myself == "X" ~ 6,
                             opponent == "C" & myself == "Y" ~ 0,
                             opponent == "C" & myself == "Z" ~ 3),
         myself_new = case_when(myself == "X" ~ 1,
                                myself == "Y" ~ 2,
                                myself == "Z" ~ 3)) %>% 
  summarise(res = sum(outcome + myself_new))

# star2
input %>% 
  extract(x, c("opponent", "outcome"), "([A-Z]) ([A-Z])", convert = TRUE) %>% 
  mutate(myself = case_when(opponent == "A" & outcome == "X" ~ "Z",
                            opponent == "A" & outcome == "Y" ~ "X",
                            opponent == "A" & outcome == "Z" ~ "Y",
                            opponent == "B" & outcome == "X" ~ "X",
                            opponent == "B" & outcome == "Y" ~ "Y",
                            opponent == "B" & outcome == "Z" ~ "Z",
                            opponent == "C" & outcome == "X" ~ "Y",
                            opponent == "C" & outcome == "Y" ~ "Z",
                            opponent == "C" & outcome == "Z" ~ "X"),
         myself_new = case_when(myself == "X" ~ 1,
                                myself == "Y" ~ 2,
                                myself == "Z" ~ 3),
         outcome_new = case_when(outcome == "X" ~ 0,
                                 outcome == "Y" ~ 3,
                                 outcome == "Z" ~ 6)) %>% 
  summarise(res = sum(outcome_new + myself_new))



