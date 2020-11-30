
setwd("/Users/xuehanyin/Games_tweets/15games_tweets")

xbps <- rbind(playstation_english,xbox_x_english)

topic_prep <- function(dataframe, regex){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text) #strip the text of links etc etc
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text,created_at) %>% mutate(tweet_number = row_number()) #add tweet numbers to keep track
  
  #isolate the tweets containing the keyword from the dataframe, unnest the text into singular words and filter the stop words. 
  intermediate2 <- intermediate1 %>% filter(grepl(regex, stripped_text, ignore.case = TRUE)) %>% mutate(topic = regex)
 
}

#search each kayword
Watch_Dogs_Legion <- topic_prep(xbps, "Watch Dogs Legion")
Cyberpunk_2077 <- topic_prep(xbps, "Cyberpunk 2077")
Call_of_Duty <- topic_prep(xbps, "Call of Duty")
NBA_2K21 <- topic_prep(xbps, "NBA 2K21")
Assassins_Creed_Valhalla <- topic_prep(xbps, "Assassin's Creed Valhalla")
Halo_Infinite <- topic_prep(xbps, "Halo Infinite")
Demons_Souls <- topic_prep(xbps, "Demon's Souls")
Astros_Playroom <- topic_prep(xbps, "Astro's Playroom")
Fortnite <- topic_prep(xbps, "Fortnite")
Marvels_spiderman <- topic_prep(xbps, "Marvel's Spider-Man")

#combine them in the same dataframe
bind <- rbind(Watch_Dogs_Legion,Cyberpunk_2077,Call_of_Duty,NBA_2K21,Assassins_Creed_Valhalla,
              Halo_Infinite,Demons_Souls,Astros_Playroom,Fortnite,Marvels_spiderman)

#change the formula of the date and deleta the exact time
bind2 <- as.Date(as.POSIXct(bind$created_at,format='%Y%M%D'))

#combine the date column to the dataframe
bind3<- cbind(bind,bind2)

#count how many data was produced by date
bind4 <- bind3 %>% group_by(topic,bind2) %>% count()

#make the graph
ggplot(data = bind4, aes(x = bind2, y = n, color = topic)) +
  geom_line() +
  labs(x = NULL, y = NULL, 
                  title = "Frequency of Tweets of Games",
                  subtitle = "Twitter status (tweet) counts aggregated using 1-days intervals",
                  caption = "Source: Data collected from Twitter's REST API via rtweet")





