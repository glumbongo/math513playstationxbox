#Posting frequency frequency off accounts
library(tidyverse)
library(rtweet)
library(tidytext)

platform_sentiment <- read_csv("platform_sentiment_by_tweet.csv")
import <- read_csv("playstation_xbox_official_tweets_extra.csv")




#strip away columns from the official accounts, summarise their
#daily post numbers by date
post_date <- import %>% select(created_at, text, screen_name) %>%
  mutate(created_at = as.Date(created_at))
factor_screen_name <- factor(post_date$screen_name)
post_date <- post_date %>% mutate(screen_name = factor_screen_name)

totals <- post_date %>% group_by(screen_name, created_at) %>% 
  summarise(total_posts = length(created_at), 
            total_posts_previous_day = length(created_at)) 
#move total_post rows down 1 to show previous day
totals$total_posts_previous_day <- lag(totals$total_posts_previous_day)

#cut the data to to observed period (27-10 - 05-11)
totals <- totals %>% filter(created_at >= as.Date('2020-10-27') & created_at < as.Date('2020-11-06'))






str(platform_sentiment) #platform needs to be refactorised
platform_sentiment <- platform_sentiment %>% 
  mutate(platform = ifelse(platform == "Xbox_X", "Xbox", "PlayStation"))
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


#avg sentiment by date
sentiment_by_date %>% group_by(date) %>%
  ggplot(aes(x = date, y = avg_score, color = platform)) +
  geom_line() +
  geom_point()

#plot showing the number of posts by official marketing accounts
#during the analysed period for posts on same day and posts previous day 
totals %>% ggplot(aes(x = created_at, y = total_posts, color = screen_name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous()
totals %>% ggplot(aes(x = created_at, y = total_posts_previous_day, color = screen_name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous()


#combine the dataframes 
#join by date and screenname to produce correct data frame
sentiment_marketing_combined <- left_join(sentiment_by_date, totals,
                                          by = c("date" = "created_at", "platform" = "screen_name"))
#renname total_posts for clarity
sentiment_marketing_combined <- sentiment_marketing_combined %>% rename(total_posts_by_marketing_account = total_posts)




#-----------------------------------SAME DAY STUFF------------------------------------#
#------------------------------------++++++++++++++-----------------------------------#
#-------------------------------------++++++++++++++----------------------------------#

#check for correlation between marketing posts and sentiment on the same day
m <- lm(avg_score ~ total_posts_by_marketing_account, data = sentiment_marketing_combined)
summary(m)
#poor correlation. need to test number of posts with a day of delay instead
with(sentiment_marketing_combined, cor.test(total_posts_by_marketing_account, avg_score, method = "pearson"))

sentiment_marketing_combined %>% ggplot(aes(x = total_posts_by_marketing_account,
                                            y = avg_score,
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
q <- lm(avg_score ~ total_posts_by_marketing_account, data = playstation_test)
summary(q)

with(playstation_test, cor.test(total_posts_by_marketing_account, avg_score, method = "pearson"))


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
  ggplot(aes(x = total_posts_previous_day,
             y = avg_score,
             color = platform)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~platform, scales = "free")

#playstation and xbox accounts combined
sentiment_marketing_combined %>% 
  ggplot(aes(x = total_posts_previous_day,
             y = avg_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 
  
with(sentiment_marketing_combined, cor.test(total_posts_previous_day, avg_score, method = "pearson"))

#just playstation
playstation_test_previous_day <- sentiment_marketing_combined %>% filter(platform == "PlayStation") %>% na.omit()
f <- lm(avg_score ~ total_posts_previous_day, data = sentiment_marketing_combined)
summary(f)

with(playstation_test_previous_day, cor.test(total_posts_previous_day, avg_score, method = "pearson"))
