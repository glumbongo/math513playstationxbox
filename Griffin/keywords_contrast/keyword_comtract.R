
library(tidyverse)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/MATH513/math513playstationxbox-main 2")

playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)

topic_prep <- function(dataframe, regex){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text) #strip the text of links etc etc
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text,created_at) %>% mutate(tweet_number = row_number()) #add tweet numbers to keep track
  
  #isolate the tweets containing the keyword from the dataframe, unnest the text into singular words and filter the stop words. 
  intermediate2 <- intermediate1 %>% filter(grepl(regex, stripped_text, ignore.case = TRUE)) %>% mutate(topic = regex)
}

#search each kayword
price_ps <- topic_prep(playstation_english, "price")
hardware_ps <- topic_prep(playstation_english,"hardware")  
design_ps <- topic_prep(playstation_english,"design")  
game_ps <- topic_prep(playstation_english,"game")

price_xb <- topic_prep(xbox_x_english, "price")
hardware_xb <- topic_prep(xbox_x_english,"hardware")  
design_xb <- topic_prep(xbox_x_english,"design") 
game_xb <- topic_prep(xbox_x_english,"game")

#combine data frames by console
all_ps <- rbind(price_ps, hardware_ps, design_ps, game_ps)
all_xb <- rbind(price_xb, hardware_xb, design_xb, game_xb)

#count by topics
all_ps_1 <- all_ps %>% group_by(topic) %>% count()
all_xb_1 <- all_xb %>% group_by(topic) %>% count()

#make the bar into negative section 
all <- rbind(all_ps_1, within(all_xb_1, n <- -n))
all$origin <- rep(c("all_ps_1", "all_xb_1"), each = nrow(all_ps_1))


ggplot(all, aes(n, topic, fill = origin)) +
  geom_col() +
  scale_x_continuous(labels = abs) +
  labs(x = NULL, y = NULL,
       title = "Frequency of discussed topics regards to each console",
       subtitle = "Twitter status (tweet) counts aggregated containing keywords",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  scale_fill_manual(name = "Console", values = c("#6699FF", "#FF6699")
                    , labels = c("all_ps_1" = "Playstation 5", "all_xb_1" = "Xbox series X"))





