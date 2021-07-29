ublic Sentiment of COVID-19 Vaccines launched by Different Brands 
-------------------

Core Content
------------------

1. Download of Twitter Data using the Twitter v2 API
2. Different queries for each considered brand (Pfizer/BioNTech, Moderna, Sinovac, AstraZeneca)
3. Sentiment analysis using 3 different dictionaries
4. Topic extraction of 18 different topics using LDA
5. Correlation between brand sentiment and tweeting behavior for the respective topics

Code Architecture (also explained in main.R)
-------------------

DOWNLOAD AND ENVIRONMENT PREPERATION
* scraper.R - downloads data from Twitter using the Twitter v2 API
* dataloading.R - merges data into one data frame

PREPROCESSING
* preprocessing.R - textual data preprocessing and tidy format

ANALYSIS
* sentiment.R - sentiment analysis using syuzhet, nrc, and bing dictionaries
* topicmodeling.R - topic extraction and correlation with sentiment

PLOTTING
* plots.R" - additional plots for explanatory data analysis

Necessary Data Files 
-------------------

please use readRDS to load those files (as intended in the code)

* raw_data.Rdata - complete Twitter download wrapped into one tibble
* text.Rdata - fully processed text data 
* analysed.RData - text data with sentiment scores per term

Samples from the Analysis 
-------------------

![](https://github.com/aleksejhoffaerber/textualvaccines/blob/bc2c6f349b08d8d6a47a605f943acaec4a93def5/_plots/s2_sentiment%20across%20brands%20and%20dictionaries.png)

![](https://github.com/aleksejhoffaerber/textualvaccines/blob/988920b50b7de8227183b96e1eacf4ed2b2a351d/_plots/top1_topic%20behavior.png)

![](https://github.com/aleksejhoffaerber/textualvaccines/blob/988920b50b7de8227183b96e1eacf4ed2b2a351d/_plots/top4_correlation%20between%20brand%20sentiment%20and%20number%20of%20tweets%20per%20topic.png)
