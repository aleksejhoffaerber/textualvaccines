# library

library(syuzhet) # sentiment analysis / dictionary

# analysis

set.seed(1224)

# 2.5 hrs to run, we recommend loading the "analysed.Rdata"
start.time <- Sys.time()
analysed <- text %>% 
  mutate(sentiment_syuzhet = get_sentiment(word, method = "syuzhet", language = "english"),
         sentiment_bing = get_sentiment(word, method = "bing", language = "english"),
         sentiment_nrc = get_sentiment(word, method = "nrc", language = "english"))

         
end.time <- Sys.time()
end.time - start.time

# 
# saveRDS(analysed, "analysed.RData") 
readRDS("analysed.RData")


# compare sentiments between each other

to_compare <- analysed %>% 
  pivot_longer(cols = c(sentiment_syuzhet, sentiment_bing, sentiment_nrc),
               names_to = "dictionary",
               values_to = "sentiment") 

to_compare <- to_compare %>% # join to add time component
  group_by(id, brand, dictionary) %>% # group by id, brand, and dictionary (distinct documents as data basis, later differences across brands & dict)
  summarise(sentiment = mean(sentiment)) %>% # mean sentiment per tweet id
  left_join(tweets %>% select(id, time)) %>% # add time component
  ungroup() %>% 
  group_by(brand, dictionary, time = lubridate::floor_date(time, "1 hour")) %>% # aggregate on hourly level
  summarise(sentiment = mean(sentiment))

to_compare %>% 
  mutate(dictionary = dictionary %>% 
           str_replace("sentiment_", "")) %>% 
  ggplot(aes(time, sentiment, colour = dictionary, linetype = dictionary)) +
  geom_line() +
  facet_wrap(~brand, nrow = 3) +
  labs(x = "Date",
       y = "Sentiment Score",
       title = "Sentiment time-series comparison between Syuzhet, Bing, and NRC-based dictionaries",
       subtitle = "Syuzhet shows a stronger accentuation for the given time-period, \nespecially due to its stronger corpus size and resolution capabilities",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  theme_bw() +
  scale_color_manual(values = c("bing" = "black", 
                                "nrc" = "black",
                                "syuzhet" = "#00BFC4")) +
  scale_linetype_manual(values = c("bing" = "dotted",
                                   "nrc" = "dashed",
                                   "syuzhet" = "solid")) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))

 ggsave("s2_sentiment across brands and dictionaries.png", path = "_plots", dpi = 300, width = 8, height = 6)



# plot: 10 most common positive and negative words, per brand
analysed %>% 
  select(brand, word, sentiment_syuzhet) %>% 
  group_by(word, sentiment_syuzhet, brand) %>% # group by word, sentiment score and brand
  count() %>% # count words per group
  mutate(type = case_when(sentiment_syuzhet < 0 ~ "negative", # create sentiment classes
                          sentiment_syuzhet > 0 ~ "positive",
                          TRUE ~ "neutral")) %>%
  filter(type != "neutral") %>% # delete neutral words, because these do not affect the scores
  arrange(-n) %>% # arrange by highest count
  ungroup() %>% 
  group_by(brand, type) %>% 
  slice_head(n = 10) %>% # pick top 10 results per brand and sentiment type
  ggplot(aes(n, reorder(word, -n), fill = type)) + 
  geom_col(show.legend = F) +
  facet_wrap(~brand + type, # seperate by brand and type
             ncol = 2, scales = "free_y") + # number of columns and type of y-axis
  labs(x = "Contribution to Sentiment",
       y = "Words",
       title = "Top 10 most appearing words by sentiment and brand",
       subtitle = "Words with a negative connotation has a lesser impact and appear less often, \n'approval', 'effective', and 'study' as positive sentiment drivers, especially for AstraZeneca and Sinovac ",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  theme_bw()

ggsave("s3_top sentiment scores per brand & sentiment type.png", path = "_plots", dpi = 300, width = 8, height = 8)
