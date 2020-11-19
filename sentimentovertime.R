library(tidyverse)
library(rtweet)
library(tidytext)

platform_sentiment <- read_csv("platform_sentiment_by_tweet.csv")

str(platform_sentiment) #platform needs to be refactorised
platform_f <- factor(platform_sentiment$platform)
platform_sentiment <- platform_sentiment %>%
  mutate(platform = platform_f)

#create a new column with just date and no time
platform_sentiment <- platform_sentiment %>% 
  mutate(date = as.Date(created_at))

#create a df with the average score by date and the platform 
#that it's on 
sentiment_by_date <- platform_sentiment %>% group_by(date, platform) %>%
  summarise(avg_score = mean(score))


sentiment_by_date %>% group_by(date) %>%
  ggplot(aes(x = date, y = avg_score, color = platform)) +
  geom_line() +
  geom_point()
