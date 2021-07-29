# libraries 

library(tidyr) # advanced data wrangling
library(stringr) # text operations
library(tidytext) # textual data wrangling
library(SnowballC) # stemming

# clean dataframe

tweets <- tweets %>% 
  mutate(time = data.created_at %>%  
           # transfer twitter time to R date time
           str_replace("T", " ") %>% 
           str_replace(":01.000Z", ":01$") %>% 
           as.POSIXct(tz = Sys.timezone()),
         # change names of columns
         likes = data.public_metrics.like_count,
         quotes = data.public_metrics.quote_count,
         replies = data.public_metrics.reply_count,
         retweets = data.public_metrics.retweet_count,
         id = data.conversation_id,
         text = data.text,
         language = data.lang,
         brand = query) %>% 
  # select only needed columns
  select(id, text, time, likes, quotes, replies, retweets, brand)

# textual cleaning, bring into tidy-format 

text <- tweets %>% select(text, brand, id) %>% 
  unnest_tokens(word, text) %>% # tokenization and tidy format
  mutate(word = str_extract(word, "[a-z']+")) %>% # if not word, delete
  mutate(word_stem = wordStem(word)) %>%  # apply stemming 
  anti_join(get_stopwords()) %>% # delete stopwords
  filter(str_detect(word, pattern = "@[a-zA-Z0-9_]{0,15}", negate = T), # delete usernames
         str_detect(word, pattern = "_", negate = T), # delete underscore things (indicating user names)
         str_count(word_stem) > 2 & str_count(word_stem) < 13) # delete words below or above word count threshold

# work with original file 
# saveRDS(text, "text.RData")
readRDS("text.Rdata")
# 
# text %>% 
#   count(word, sort = T)


  
