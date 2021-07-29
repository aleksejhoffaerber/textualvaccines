# data loading 

# combine title names in one vector
files <- paste0(c("astrazeneca", "bioNtech", "moderna", "sinovac", "general"), "_2106_0030.RData")


# load all RDS files and combine into one large tibble
tweets <- map(files, ~readRDS(as.character(.x))) %>% 
  reduce(bind_rows)

# save/read as intended
# saveRDS(tweets, "raw_dataset.RData")
readRDS("raw_dataset.RData")
