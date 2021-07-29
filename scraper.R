# libraries ----

library(httr) # parsing API information
library(stringr) # text operations
library(jsonlite) # JSON to data format transformation

# set-up environment and twitter credentials ------

bearer_token <- ""
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))

# define data ranges -----

data_range <- tibble(datetime = seq(as.POSIXct("2021-06-14 04:00:01"), # beggining
                                    as.POSIXct("2021-06-21 00:30:01"), # end
                                    by = "15 mins") %>% # interval 
                       as.character(),
                     tw_datetime = "") 



# the pipe operator (" %>% ", can be quickly written with Ctrl + Shift + M)
# can be interpretes as put the previous result (from previous line) in the next line and treat it as 
# input for the first object in the new function
data_range <- data_range %>% mutate(tw_datetime = datetime %>% 
                                      # create twitter specific datetime information
                                      str_replace(" ", "T") %>% 
                                      str_replace(":01$", ":01.000Z"),
                                    id = row_number()) # add id number, to ease counting

# create download function -----

twitter_download <- function(x) {
  # sleep until executing again, so that only 450 requests / 15 mins
  Sys.sleep(2) 
  
  params = list(
    `query` = '(Sinovac OR sinovac) -is.retweet lang:en', 
    # TODO: put the keywords into the query
    # AstraZeneca (AstraZeneca, astrazeneca)
    # inovac (Sinovac, sinovac)
    # BioNTech (BioNTech, biontech, biontech-pfizer, BioNTech-Pfizer)
    # Moderna (Moderna, moderna)
    # General (vaccine, COVID-vaccine, covid-vaccine, covid vaccine, sars-vaccine, SARS-vaccine)
    `max_results` = '100', # download limit
    `tweet.fields` = 'created_at,lang,conversation_id,public_metrics', # metrics, such as retweets, likes etc.
    `end_time` = as.character(data_range[x,2]) # dynamic range depending on map
  )
  
  # show progress
  print(paste(data_range[x,3], "of", nrow(data_range) ))
  
  # API-specific part 
  response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/recent', 
                        httr::add_headers(.headers = headers), query = params)
  
  
  obj <- httr::content(response, as = "text")
  
  # create data frame from JSON export 
  fromJSON(obj, flatten = TRUE) %>% 
    as.data.frame() %>% 
    # transform into better data format (faster data wrangling, important for higher data sizes)
    as_tibble()
  
}  


# intuition: map through all the rows in "data_range" (which contain the timedates)
# and execute the function above for all of them 

full_exp <- map(1:nrow(data_range), ~twitter_download(.x)) %>% 
  # reduce complexity of the results from individual tibbles into one big tibble
  reduce(bind_rows) 
# select needed data fields / data columns

full_exp <- full_exp %>% 
  # select needed columns
  select(data.text, data.lang,
         data.created_at, data.conversation_id, data.public_metrics.retweet_count,
         data.public_metrics.reply_count, data.public_metrics.like_count, data.public_metrics.quote_count,
         meta.newest_id, meta.oldest_id, meta.result_count, meta.next_token) %>% 
  mutate(query = "Sinovac")



saveRDS(full_exp, "sinovac_2106_0030.RData") 

