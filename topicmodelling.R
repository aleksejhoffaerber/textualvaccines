# libraries

library(tm)
library(xlsx)
library(tidytext)
library(wordcloud)
library(topicmodels)

# count words per brand  
text_words <- text %>% 
  count(brand, word, sort = T)

total_words <- text_words %>% 
  group_by(brand) %>% 
  summarise(total = sum(n))

text_words <- text_words %>% 
  left_join(total_words)

# count words per document
text_words_id <- text %>% 
  count(id, word, sort = T)

total_words_id <- text_words_id %>% 
  group_by(id) %>% 
  summarise(total = sum(n))

text_words_id <- text_words_id %>% 
  left_join(total_words_id)


# word importance analysis (tf-idf)
text_tf_idf <- text_words %>%  
  bind_tf_idf(word, brand, n) %>% 
  arrange(-idf)


# create document-term matrix (matrix representation of tidy word format)

dtm <- text_words_id %>% 
  cast_dtm(document = id, term = word, value = n)

# LDA training ------
set.seed(1224) # setting seed because of possible changes due to unsupervised nature of the LDA algorithm

lda <- LDA(dtm, k = 18,
           method = "Gibbs",
           control = list(
             burnin = 5,  # how often sampled before estimation recorded
             iter = 75,  # number of iterations
             keep = 1,    # saves additional data per iteration (such as logLik)
             save = F,     # saves logLikelihood of all iterations
             verbose = 5  # report progress
           ))

data.frame(logLiks = lda@logLiks,
           Index = seq(1:80)) %>% 
  ggplot(aes(Index, logLiks)) +
  geom_line() +
  labs(x = "Iteration",
       y = "Loglikehood (the higher, the better)",
       title = "LDA performance over number of iterations",
       subtitle = "Stable performance after 30 Iterations") +
  theme_minimal()

# resulting topics

topic.words <- function () { 
  
  beta <- exp(lda@beta) # log of probability reported
  dim(beta)
  
  topic.terms <<- list() # empty list for topic words
  prob.top <<- list() # empty list for respective probabilities
  
  for (i in 1:lda@k) {
    
    topic.terms[[i]] <<- head(lda@terms[order(beta[i,], decreasing = T)], 60) # generating top words per topic
    prob.top[[i]] <<- head(sort(beta[i,], decreasing = T), 60)
    
  }
}

topic.words() 

topic_gamma <- lda@gamma # topic assignments per document

topic_gamma <- topic_gamma %>% as.data.frame() %>% # create df structure
  rename_with(str_replace, pattern = "V", replacement = "") %>% # change column names
  mutate(topic = colnames(.)[max.col(.,ties.method = "first")], # find winnning topic
         mak = do.call(pmax,.)) %>%  # maximizing the gamma values
  bind_cols(tibble(document = lda@documents)) # add document reference (id)



# create topic wordclouds - no need to run
extracted.topics <- function(v) { # draw the topic wordclouds
  par(mfrow=c(1,1), oma = c(0,0,0,0), mai = c(0,0,0,0)) # reset plotting space 
  for (i in 1:lda@k) {
    
    x <- topic.terms[[i]]
    y <- prob.top[[i]]
    
    
    a <- wordcloud(words = x,
                   freq = y,
                   scale = c(5, 1),
                   random.order = F, 
                   rot.per = 0.5, # 90? degree rotation amount
                   colors = brewer.pal(8, "Dark2")) # colour palette taking frequency into account
    
    dev.copy2pdf(file=paste0("Topics ",i,".pdf"), width = 7, height = 5)
    
  }
  
  par(mfrow=c(1,1), oma = c(1,1,1,1), mai = c(1,1,1,1)) # reset plotting space 
  
}
extracted.topics(lda)
# files were then transferred to png from the console and manually

# topic name assignment
topic_names <- data.frame(topic = c(1:18),
                          name = c("01 - delta variant & vaccine effectiveness",
                                   "02 - vaccine appointsments & availability",
                                   "03 - anti-vaccination stances",
                                   "04 - Sinovac positive reactions",
                                   "05 - AstraZeneca vaccine reactions",
                                   "06 - government supply & provision",
                                   "07 - general vaccine updates",
                                   "08 - first & second doses or shots",
                                   "09 - Sinovac studies & immune response",
                                   "10 - Sinovac vaccinations in Indonesia",
                                   "11 - public MRNA-vaccine awareness",
                                   "12 - FDA vaccine approval",
                                   "13 - peer-reviews, research, and studies",
                                   "14 - positive reaction to shot",
                                   "15 - getting or wanting a vaccine",
                                   "16 - BioNTech / Pfizer vaccine",
                                   "17 - AstraZeneca adverse aftereffects",
                                   "18 - vaccine trials"))

# growth in topic / tweeting behavior ------

topic_gamma %>% select(document, topic) %>% 
  left_join(tweets %>% select(id, time), by = c("document" = "id")) %>% # add time component 
  mutate(topic = as.integer(topic)) %>% # transfer to integer to enable merrging later
  left_join(topic_names) %>% # use topic names + numbering for readability
  group_by(name, time = lubridate::floor_date(time, "1 hour")) %>% 
  count() %>% # create observations based on aggregated counts
  ggplot(aes(time, n, colour = name)) +
  geom_line() +
  facet_wrap(~name, ncol = 3) +
  labs(x = "Date",
       y = "Number of Tweets",
       title = "Number of tweets classified according to a topic, aggregated on hourly level",
       subtitle = "Tweeting topics can be categorized in a) strong news-based and b) sudden continuous interest \na) includes topics 1, 6, 8, 9, 12, and 16 b) includes 10 and 13",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("top1_topic behavior.png", path = "_plots", dpi = 300, width = 8, height = 12)

# most important topics per brand ------

topic_gamma %>% select(document, topic) %>% 
  left_join(tweets %>% select(id, time, brand), by = c("document" = "id")) %>% # add time component 
  mutate(topic = as.integer(topic)) %>% # transfer to integer to enable merrging later
  left_join(topic_names) %>% # use topic names + numbering for readability
  
  group_by(name, brand) %>% 
  # distinct(document) %>% 
  count() %>% # create observations based on aggregated counts
  arrange(-n) %>% 
  ungroup() %>% 
  group_by(brand) %>% 
  mutate(sum = sum(n),
         rel = (n / sum)) %>% 
  slice_head(n = 5) %>% 
  
  ggplot(aes(rel, reorder(name, rel))) +
  geom_col(fill = "gray60") +
  facet_wrap(~brand, nrow = 5,
             scales = "free_y") +
  labs(x = "Percent of Categorized Tweets",
       y = "Topic",
       title = "Percantage of tweets categorized to the top 5 topics, per brand",
       subtitle = "Moderna and Sinovac tweets and sentiment were strongly driven by \nFDA approval, local government updates, and ongoing research",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(legend.position = "none")

ggsave("top2_important topics per brand.png", path = "_plots", dpi = 300, width = 8, height = 8)


# print sentiment by topic ----

sentiment_topic <- analysed %>% 
  select(id, brand, sentiment_syuzhet) %>% 
  rename(sentiment = sentiment_syuzhet) %>% 
  group_by(id, brand) %>% 
  summarise(sentiment = mean(sentiment)) %>% 
  left_join(tweets %>% select(id, time)) %>% # add time component
  ungroup() %>% 
  group_by(brand, id, time = lubridate::floor_date(time, "1 hour")) %>% # aggregate on hourly level
  summarise(sentiment = mean(sentiment))

sentiment_topic <- sentiment_topic %>% 
  left_join(
    topic_gamma %>% select(document, topic) %>% 
      left_join(tweets %>% select(id, time), by = c("document" = "id")) %>% # add time component 
      mutate(topic = as.integer(topic)) %>% # transfer to integer to enable merrging later
      left_join(topic_names) %>% # use topic names + numbering for readability
      group_by(document, name, time = lubridate::floor_date(time, "1 hour")) %>% 
      count(),  # create observations based on aggregated counts
  by = c("id" = "document"))
  
  
sentiment_topic <- sentiment_topic %>% 
  select(-time.x) %>% 
  rename(time = time.y) 

# sentiment by topic
sentiment_topic %>% 
  ungroup() %>% 
  group_by(name, time) %>% 
  summarise(sentiment = mean(sentiment)) %>% 
  filter(!is.na(name)) %>% 
  ggplot(aes(time, sentiment)) +
  geom_line(colour = "#00BFC4") +
  facet_wrap(~name, ncol = 3) +
  labs(x = "Date",
       y = "Sentiment Score",
       title = "Sentiment Score by classified topics",
       subtitle = "Except for topic 12 - FDA approval, other topics show a balanced sentiment \nmeaning that spikes in positive sentiment were driven by news about FDA approval",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("top3_sentiment per topic.png", path = "_plots", dpi = 300, width = 8, height = 8)

# correlation by topic -----

sentiment_topic_corr <- topic_gamma %>% select(document, topic) %>% # topic modelling data
  left_join(tweets %>% select(id, time, brand), by = c("document" = "id")) %>% # add time component 
  group_by(brand, topic, time = lubridate::floor_date(time, "1 hour")) %>% 
  count() %>% 
  # mutate(row = row_number()) %>% 
  pivot_wider(names_from = topic,
              values_from = n) %>% 
  left_join( # join with sentiment data
    to_compare %>% 
      filter(dictionary == "sentiment_syuzhet"), 
    by = c("brand" = "brand", "time" = "time")
  ) %>% 
  select(-dictionary) %>% 
  mutate_if(is.integer, as.numeric) %>% 
  replace(is.na(.), 0) %>% 
  ungroup()
  
# create crossing between brand x topic possibilities
cros <- crossing(
  sentiment_topic_corr %>% ungroup() %>% distinct(brand),
  sentiment_topic_corr %>% ungroup() %>% colnames() %>% .[3:20]
)

# correlate number of tweets per topic with sentiment on brand level 
corr <- map(1:nrow(cros), ~sentiment_topic_corr %>% 
              filter(brand == as.character(cros[.x,1])) %>% # filter on brand
              select(as.character(cros[.x,2]), sentiment) %>% # only select needed topic and brand sentiment
              corrr::correlate() %>% # correlate results
              slice(1) %>% # select one result in correlation matrix
              select(sentiment) %>% 
              as.numeric() %>% 
              # combine into a tibble
              tibble(corr = .,
                     topic = as.character(cros[.x,2]),
                     brand = as.character(cros[.x,1]))) %>% 
  reduce(bind_rows) %>% # add all individual tibbles into a bigger one
  pivot_wider(names_from = brand, # pivot for easier plotting
              values_from = corr) %>% 
  mutate(topic = as.integer(topic)) %>% 
  mutate_if(is.numeric, round, 1) %>% # round results
  arrange(topic) 

# same procedure, but to compute significance matrix for correlation results
corr_pmat <- map(1:nrow(cros), ~sentiment_topic_corr %>% 
                   filter(brand == as.character(cros[.x,1])) %>% 
                   select(as.character(cros[.x,2]), sentiment) %>% 
                   ggcorrplot::cor_pmat() %>% 
                   as_tibble() %>% 
                   slice(1) %>% 
                   select(sentiment) %>% 
                   as.numeric() %>% 
                   tibble(corr = .,
                          topic = as.character(cros[.x,2]),
                          brand = as.character(cros[.x,1]))) %>% 
  reduce(bind_rows) %>% 
  pivot_wider(names_from = brand,
              values_from = corr) %>% 
  mutate(topic = as.integer(topic)) %>% 
  mutate_if(is.numeric, round, 3) %>% # round results
  arrange(topic) 

write.xlsx(corr_pmat, file = "significance_corr.xlsx",
           sheetName="01", append=TRUE)

# plot
corr %>% 
  select(-topic) %>%
  ggcorrplot::ggcorrplot(outline.color = "white", lab = T,
                         ggtheme = ggplot2::theme_bw,
                         colors = c("#6D9EC1", "white", "#E46726")) +
  labs(x = "Topic",
       y = "Brand",
       title = "Correlation between brand sentiment and number of tweets per topic",
       subtitle = "As expected, topic 1 (delta variants), topic 6 (government supply), \nand topic 12 (FDA approval) show a high correlation with the eventual sentiment",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  scale_x_continuous(breaks = c(1:18)) +
  theme(legend.position = "bottom", legend.box = "horizontal") +
  guides(colour = guide_legend(nrow = 1))


ggsave("top4_correlation between brand sentiment and number of tweets per topic.png", path = "_plots", dpi = 300, width = 8, height = 5)

