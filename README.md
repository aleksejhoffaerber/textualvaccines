Public Sentiment of COVID-19 Vaccines launched by Different Brands 
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

Samples from the Analysis 
-------------------

![](https://github.com/Digital-Footprints/dfcp_team4-team4/blob/b1708128fdaf41ac59f5bacb2f05ee41b3e42444/_plots/s2_sentiment%20across%20brands%20and%20dictionaries.png)

![](https://github.com/Digital-Footprints/dfcp_team4-team4/blob/b1708128fdaf41ac59f5bacb2f05ee41b3e42444/_plots/top1_topic%20behavior.png)

![](https://github.com/Digital-Footprints/dfcp_team4-team4/blob/075301bbf7361c07d9f2dfe31fb4835486b52db1/_plots/top4_correlation%20between%20brand%20sentiment%20and%20number%20of%20tweets%20per%20topic.png)
