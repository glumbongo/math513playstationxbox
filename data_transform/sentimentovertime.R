#Posting frequency frequency off accounts
library(tidyverse)
library(rtweet)
library(tidytext)
library(lubridate)


setwd("c://users/glumb/Documents/university/math513/coursework/presentation/csv_files/")

platform_sentiment <- read_csv("platform_sentiment_by_tweet.csv")
import <- read_csv("playstation_xbox_official_tweets_extra.csv")




#strip away columns from the official accounts, summarise their
#daily post numbers by date

tm <- seq(as.POSIXct("2020-10-27 05:00:00"), by = "6 hours", length.out = 38)
tm



post_date <- import %>% select(created_at, text, screen_name)
post_date <- post_date %>% filter(created_at >= as.POSIXct('2020-10-27 05:00:00') & created_at < as.POSIXct('2020-11-05 15:31:09'))
factor_screen_name <- factor(post_date$screen_name)
post_date <- post_date %>% mutate(screen_name = factor_screen_name, six_hour_intervals = cut.POSIXt(created_at, breaks = tm))
                                  
                            




#total the amount of posts in each interval
totals <- post_date %>% group_by(screen_name, six_hour_intervals) %>% 
  summarise(total_posts = length(six_hour_intervals)) 
totals <- na.omit(totals)
totals <- totals %>% mutate(six_hour_intervals = as.POSIXct(six_hour_intervals))            
            
            # total_posts_previous = length(six_hour_intervals)) 
#move total_post rows down 1 to show previous day
# totals$total_posts_previous <- lag(totals$total_posts_previous)

#add back in the missing aggregations with join
test <- data.frame(screen_name = "Xbox", six_hour_intervals = tm)
test_2 <- data.frame(screen_name = "PlayStation", six_hour_intervals = tm)
test <- rbind(test, test_2)
test <- test %>% mutate(six_hour_intervals = as.POSIXct(six_hour_intervals))
totals_test <- left_join(test, totals, by = c("six_hour_intervals", "screen_name"))
totals_test$total_posts <- replace_na(totals_test$total_posts, 0)
totals_test <- totals_test %>% mutate(total_posts_previous = lag(total_posts))


# totals$total_posts_previous <- replace_na(totals$total_posts_previous, 0)
# totals <- na.omit(totals)
# totals <- totals %>% mutate(six_hour_intervals = as.POSIXct(six_hour_intervals))



str(platform_sentiment) #platform needs to be refactorised
platform_sentiment <- platform_sentiment %>% 
  mutate(platform = ifelse(platform == "Xbox_X", "Xbox", "PlayStation"))
platform_f <- factor(platform_sentiment$platform)
platform_sentiment <- platform_sentiment %>%
  mutate(platform = platform_f)

#create a new column with just date and no time
# platform_sentiment <- platform_sentiment %>% 
#   mutate(date = as.Date(created_at))


#create a df with the average score by date and the platform 
#that it's on 
sentiment_6_hour <- platform_sentiment %>% mutate(six_hour_intervals = cut.POSIXt(created_at, breaks = "6 hours")) %>% 
  group_by(platform, six_hour_intervals) %>% summarise(avg_sentiment = mean(score))

sentiment_6_hour <- sentiment_6_hour %>% mutate(six_hour_intervals = as.POSIXct(six_hour_intervals))



# #avg sentiment by date
# sentiment_by_date %>% group_by(date) %>%
#   ggplot(aes(x = date, y = avg_score, color = platform)) +
#   geom_line() +
#   geom_smooth(method = "lm", se = FALSE)
#   geom_point()

#plot showing the number of posts by official marketing accounts
#during the analysed period for posts on same day and posts previous day 
totals %>% ggplot(aes(x = six_hour_intervals, y = total_posts, color = screen_name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous() +
  scale_x_datetime() +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2))

totals %>% ggplot(aes(x = six_hour_intervals, y = total_posts_previous, color = screen_name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous() +
  scale_x_datetime() +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2))
  


#combine the dataframes 
#join by date and screenname to produce correct data frame
sentiment_marketing_combined <- left_join(sentiment_6_hour, totals_test,
                                          by = c("six_hour_intervals", "platform" = "screen_name"))





#-----------------------------------SAME DAY STUFF------------------------------------#
#------------------------------------++++++++++++++-----------------------------------#
#-------------------------------------++++++++++++++----------------------------------#

#check for correlation between marketing posts and sentiment on the same day
m <- lm(avg_score ~ total_posts_by_marketing_account, data = sentiment_marketing_combined)
summary(m)
#poor correlation. need to test number of posts with a day of delay instead
with(sentiment_marketing_combined, cor.test(total_posts_by_marketing_account, avg_score, method = "pearson"))

sentiment_marketing_combined %>% ggplot(aes(x = total_posts,
                                            y = avg_sentiment,
                                            color = platform)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~platform, scales = "free")
  
  

mean_sd <- sentiment_marketing_combined %>% na.omit() %>%  group_by(platform) %>% summarise(mean_marketing = mean(total_posts_by_marketing_account), sd_marketing = sd(total_posts_by_marketing_account),
                                                                                            mean_sent = mean(avg_score), sd_sent = sd(avg_score))
mean_sd
rnorm(8, 15.5, 6.82)

#just playstation
playstation_test <- sentiment_marketing_combined %>% filter(platform == "PlayStation") %>% na.omit()
q <- lm(avg_sentiment ~ total_posts, data = playstation_test)
summary(q)

with(playstation_test, cor.test(total_posts, avg_sentiment, method = "pearson"))


#just xbox
xbox_test <- sentiment_marketing_combined %>% filter(platform == "Xbox") %>% na.omit()
p <- lm(avg_score ~ total_posts_by_marketing_account, data = xbox_test)
summary(p)

with(xbox_test, cor.test(total_posts_by_marketing_account, avg_score))

#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#
#----------------------------------------------------------------------------#

#Testing for correlation for posts on previous day

sentiment_marketing_combined %>% 
  ggplot(aes(x = total_posts_previous,
             y = avg_sentiment,
             color = platform)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~platform, scales = "free")

#playstation and xbox accounts combined
sentiment_marketing_combined %>% 
  ggplot(aes(x = total_posts_previous,
             y = avg_sentiment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
  
with(sentiment_marketing_combined, cor.test(total_posts_previous, avg_sentiment, method = "pearson"))

#just playstation
playstation_test_previous_day <- sentiment_marketing_combined %>% filter(platform == "PlayStation") %>% na.omit()
f <- lm(avg_sentiment ~ total_posts_previous, data = sentiment_marketing_combined)
summary(f)

with(playstation_test_previous_day, cor.test(total_posts_previous, avg_sentiment, method = "pearson"))
