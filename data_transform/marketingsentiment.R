library(tidyverse)
library(rtweet)
library(tidytext)
library(lubridate)


setwd("c://users/glumb/Documents/university/math513/coursework/presentation/csv_files/")

platform_sentiment <- read_csv("platform_sentiment_by_tweet.csv")
import <- read_csv("playstation_xbox_official_tweets_extra.csv")


#create the appropriate 6 hour breaks 
tm <- seq(as.POSIXct("2020-10-27 05:00:00"), by = "6 hours", length.out = 38)
post_date <- import %>% select(created_at, text, screen_name)
post_date <- post_date %>% filter(created_at >= as.POSIXct('2020-10-27 05:00:00') & created_at < as.POSIXct('2020-11-05 15:31:09'))
factor_screen_name <- factor(post_date$screen_name)
post_date <- post_date %>% mutate(screen_name = factor_screen_name, six_hour_intervals = cut.POSIXt(created_at, breaks = tm))



#total the amount of posts in each interval
totals <- post_date %>% group_by(screen_name, six_hour_intervals) %>% 
  summarise(total_posts = length(six_hour_intervals)) 
totals <- na.omit(totals)
totals <- totals %>% mutate(six_hour_intervals = as.POSIXct(six_hour_intervals))            


#add back in the missing aggregations with join
test <- data.frame(screen_name = "Xbox", six_hour_intervals = tm)
test_2 <- data.frame(screen_name = "PlayStation", six_hour_intervals = tm)
test <- rbind(test, test_2)
test <- test %>% mutate(six_hour_intervals = as.POSIXct(six_hour_intervals))
totals_test <- left_join(test, totals, by = c("six_hour_intervals", "screen_name"))
totals_test$total_posts <- replace_na(totals_test$total_posts, 0)
totals_test <- totals_test %>% mutate(total_posts_previous = lag(total_posts))


#refactorise platform_sentiment
platform_sentiment <- platform_sentiment %>% 
  mutate(platform = ifelse(platform == "Xbox_X", "Xbox", "PlayStation"))
platform_f <- factor(platform_sentiment$platform)
platform_sentiment <- platform_sentiment %>%
  mutate(platform = platform_f)


#6 hour intervals sentiment
sentiment_6_hour <- platform_sentiment %>% mutate(six_hour_intervals = cut.POSIXt(created_at, breaks = "6 hours")) %>% 
  group_by(platform, six_hour_intervals) %>% summarise(avg_sentiment = mean(score))
sentiment_6_hour <- sentiment_6_hour %>% mutate(six_hour_intervals = as.POSIXct(six_hour_intervals))


#combine the dataframes 
#join by date and screenname to produce correct data frame
sentiment_marketing_combined <- left_join(sentiment_6_hour, totals_test,
                                          by = c("six_hour_intervals", "platform" = "screen_name"))




#Testing for correlation for posts on previous day

sentiment_marketing_combined %>% 
  ggplot(aes(x = total_posts_previous,
             y = avg_sentiment,
             color = platform)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~platform, scales = "free")



#playstation linear model and t test 
playstation_test_previous_day <- sentiment_marketing_combined %>% filter(platform == "PlayStation") %>% na.omit()
f <- lm(avg_sentiment ~ total_posts_previous, data = sentiment_marketing_combined)
summary(f)

with(playstation_test_previous_day, cor.test(total_posts_previous, avg_sentiment, method = "pearson"))
