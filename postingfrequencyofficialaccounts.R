library(tidyverse)
library(rtweet)
library(tidytext)

setwd("C://users/glumb/documents/university/math513/coursework/presentation")

#CHRIS-- sentiment over time compared to posting frequency of xbox 
#and playstation marketing accounts. - Which company is having a 
#greater impact with their marketing?  LINEAR REGRESSION MODEL 




# playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
# xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)



# xbox_account_tweets <- get_timeline("Xbox", n = 3200)
# playstation_account_tweets <- get_timeline("PlayStation", n = 3200)
# playstation_xbox_tweets <- rbind(xbox_account_tweets, playstation_account_tweets)
# #add in some extra days to allow for second hypothesis testing 
# playstation_xbox_tweets_date_adjusted <- playstation_xbox_tweets %>% filter(created_at >= as.Date('2020-10-23') & created_at < as.Date('2020-11-10'))
# import <- playstation_xbox_tweets_date_adjusted
# write_as_csv(playstation_xbox_tweets_date_adjusted, "playstation_xbox_official_tweets_extra.csv")


import <- read_csv("playstation_xbox_official_tweets_extra.csv")

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



#plot showing the number of posts by official marketing accounts
#during the analysed period 
totals %>% ggplot(aes(x = created_at, y = total_posts, color = screen_name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous()
