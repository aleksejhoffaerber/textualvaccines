# minor explanatory data analysis

tweets %>% 
  # pivot data frame into a long version of itself, so that public metrics are in one column
  pivot_longer(cols = c(likes, quotes, replies, retweets),
               names_to = "metrics",
               values_to = "value") %>% 
  group_by(brand, metrics, time = lubridate::floor_date(time, "1 hour")) %>% 
  summarise(value = mean(value)) %>% 
  ggplot(aes(time, value, color = metrics)) +
  geom_point(alpha = .2) + # point aesthetic
  geom_smooth(colour = "black", linetype = "dotted", size = 0.5) + # draw smoothing line based on loess
  facet_wrap(~brand + metrics,
             ncol = 4,
             scales = "free_y") + # wrap by brand and metric
  labs(x = "Date",
       y = "Metric",
       title = "Time-series of likes, quotes, replies, and retweets among the brands",
       subtitle = "Number of retweets of AstraZeneca and Sinovac is highly affected, \nretweets for other brands and other metrics show little to no change",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  theme_bw() +  
  theme(legend.position = "none")


ggsave("eda1_metrics across brands.png", path = "_plots", dpi = 300, width = 8, height = 8)


# timeline ----

tweets %>% 
  # must be aggregated by the hours, otherwise it does not make any sense
  group_by(brand, time = lubridate::floor_date(time, "1 hour")) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(time, count, color = brand)) +
  geom_line() +
  facet_wrap(~brand) +
  labs(x = "Date",
       y = "Number of Tweets",
       title = "Number of tweets per brand available in dataset",
       subtitle = "Wide queries result in constant number of tweets per request, while BioNTech and Sinovac show variations across gathering period",
       caption = "Twitter v2 API export, from 14th of June until 21th of June 2021") +
  theme_bw() +
  theme(legend.position = "none")

# why is it centered around 400? (because 4 * 15 = 1 hours)

ggsave("eda2_timeline tweets.png", path = "_plots", dpi = 300, width = 8, height = 6)


