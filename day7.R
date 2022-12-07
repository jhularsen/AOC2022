library(tidyverse)
input <- tibble(x = read_lines("data/day7_input.txt")) 

# star1
cd <- function(path, dir) {
  if (!is.na(dir)) {
    if (dir == "..") {
      return(head(path, -1))
    }
    return(c(path, paste0(tail(path, 1), "/", dir)))
  }
  return(path)
}

dir_sizes <- input %>% 
  extract(x, "cd_dir", "cd (.*)", remove = FALSE) %>% 
  mutate(path = c(accumulate(cd_dir, cd))) %>% 
  unnest(path) %>% 
  filter(str_detect(x, "[0-9]")) %>% 
  mutate(size = as.numeric(str_extract(x, "[0-9]*"))) %>% 
  group_by(path) %>% 
  summarise(size = sum(size))

dir_sizes %>% 
  filter(size <= 100000) %>% 
  summarise(res = sum(size))
  
# star2 
dir_sizes %>% 
  mutate(space_needed = head(dir_sizes$size, 1) + 30000000 - 70000000) %>% 
  filter(size >= space_needed) %>% 
  summarise(min = min(size))
