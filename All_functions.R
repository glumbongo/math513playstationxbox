library(tidyverse)
library(rtweet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(jsonlite)
library(tidytext)
library(wordcloud)
library(wordcloud2)
library(tidyr)
library(maps)
library(graphics)
library(lubridate)
library(ggallin)
library(patchwork)

setwd("C://Users//ELISW//Documents//UNI//Data")

playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)

##____________________________(tv) Tweet Volume___________________________##

tv_ps <- as.Date(as.POSIXct(playstation_english$created_at,format='%Y%M%D'))
tv_xb <- as.Date(as.POSIXct(xbox_x_english$created_at,format='%Y%M%D'))

playstation_english <- cbind(playstation_english,tv_ps)
xbox_x_english <- cbind(xbox_x_english,tv_xb)

tv_ctx <- playstation_english %>% group_by(tv_ps) %>% count()

tv_ctp <- xbox_x_english %>% group_by(tv_xb) %>% count()


ggplot() +
  geom_area(data = tv_ctp,aes(x = tv_xb, y = n, color = "#E7B800", fill = "#E7B800"),
            alpha = 0.5, position = position_dodge(0.8)) +
  geom_area(data = tv_ctx,aes(x = tv_ps, y = n,color = "#00AFBB", fill = "#00AFBB"),
            alpha = 0.5, position = position_dodge(0.8)) +
  labs(x = NULL, y = NULL,title = "Frequency of Tweets From #Xbox and #Playstation",
       subtitle = "Twitter status (tweet) counts aggregated using 1-days intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet")+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"),
                    name  ="Console",
                    labels=c("Playstation", "Xbox")) +
  guides(color=FALSE)
##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##________________________(csa)Sentiment Analysis for consoles + t test____________##

console_my_stop_words <- data.frame(word = c("playstation","ps5","PS5","xbox","gaming","games","game"))


#creates new data frames with the tweet number, platform, sentiment score, date of tweet
console_sentiment_analysis_prep <- function(dataframe, inputtopic){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text)
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text, created_at) %>% mutate(tweet_number = row_number()) %>%
    unnest_tokens(word, stripped_text)
  
  intermediate2 <- intermediate1 %>% anti_join(stop_words) %>% anti_join(console_my_stop_words)
  
  dataframe_sentiment <- intermediate2 %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(sentiment = ifelse(word == "hype", "positive", sentiment))
  
  created_list <- dataframe_sentiment %>% select(created_at, tweet_number) %>% distinct() %>% mutate(created_at = as.POSIXct(created_at))
  
  newname <- dataframe_sentiment %>% count(tweet_number, sentiment) %>%
    spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
    mutate(score = positive - negative) # score = net sentiment (positive - negative)
  
  
  
  newname <- newname %>% mutate(platform = inputtopic)
  newname <- left_join(newname, created_list, by = "tweet_number")
  
}


xbox_sentiment <- console_sentiment_analysis_prep(xbox_x_english, "Xbox_X")
playstation_sentiment <- console_sentiment_analysis_prep(playstation_english, "Playstation")

#bind dataframes, factorise platform
platform_sentiment <- rbind(xbox_sentiment, playstation_sentiment)
platform_f <- factor(platform_sentiment$platform)
platform_sentiment <- platform_sentiment %>% mutate(platform = platform_f, hour_of_day = hour(created_at))


#average sentiment aggregated every 6 hours

sentiment_6_hour <- platform_sentiment %>% mutate(six_hour_intervals = cut.POSIXt(created_at, breaks = "6 hours")) %>% 
  group_by(platform, six_hour_intervals) %>% summarise(avg_sentiment = mean(score))

sentiment_6_hour  %>% group_by(platform) %>% ggplot(aes(x = six_hour_intervals, y = avg_sentiment, color = platform,
                                                        group = platform)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2))


#average sentiment by hour of the day

sentiment_by_hour <- platform_sentiment %>% group_by(platform, hour_of_day) %>% 
  summarise(mean_sentiment = mean(score) )

sentiment_by_hour %>% group_by(platform) %>% ggplot(aes(x = hour_of_day, y = mean_sentiment, color = platform)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(0:23))



#sentiment using NRC library (emotions)

console_sentiment_analysis_prep_nrc <- function(dataframe, inputtopic){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text)
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text, created_at) %>% mutate(tweet_number = row_number()) %>%
    unnest_tokens(word, stripped_text)
  
  intermediate2 <- intermediate1 %>% anti_join(stop_words) %>% anti_join(console_my_stop_words)
  
  dataframe_sentiment <- intermediate2 %>%
    inner_join(get_sentiments("nrc"))
  
  output <- dataframe_sentiment %>% mutate(platform = inputtopic)
  
  
}

nrc_test_xbox <- console_sentiment_analysis_prep_nrc(xbox_x_english, "Xbox_X")
nrc_test_playstation <- console_sentiment_analysis_prep_nrc(playstation_english, "Playstation")
nrc_sentiment <- rbind(nrc_test_xbox, nrc_test_playstation)
platform_f <- factor(nrc_sentiment$platform)
nrc_sentiment <- nrc_sentiment %>% mutate(platform = platform_f)

#nrc sentiment grouped in 6 hour intervals 
nrc_sentiment_6_hour <- nrc_sentiment %>% mutate(six_hour_intervals = cut.POSIXt(created_at, breaks = "6 hours")) %>%
  count(platform, sentiment, six_hour_intervals)

#too many data points tightly packed
nrc_sentiment_6_hour %>% group_by(sentiment) %>% ggplot(aes(x = six_hour_intervals, y = n, color = sentiment, group = sentiment)) +
  geom_point() +
  
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2)) +
  facet_wrap(~ platform)

#1 day intervals 

nrc_sentiment_1_day <- nrc_sentiment %>% mutate(one_day_intervals = cut.POSIXt(created_at, breaks = "1 day")) %>%
  count(platform, sentiment, one_day_intervals)

nrc_sentiment_1_day %>% group_by(sentiment) %>% ggplot(aes(x = one_day_intervals, y = n, color = sentiment, group = sentiment)) +
  geom_point() +
  geom_line() +
  scale_y_log10() +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2)) +
  facet_wrap(platform ~sentiment)



# write_csv(platform_sentiment, file = "platform_sentiment_by_tweet.csv")


#checking row numbers for platforms. read online that sample size doesnt
#actually matter for t.test but not 100% sure
#platform_sentiment %>% group_by(platform) %>% summarise(rows = n())


#test for statistical significance between opinions of platforms
t.test(score ~platform, data = platform_sentiment, var.equal = TRUE)


#ttest is the mean of playstation less than the mean of xbox
t.test(score ~platform, data = platform_sentiment, var.equal = TRUE,
       alternative = "less")



#one sample t.test for each platform. compare sentiment distribution
#with a dist of 0 
Xbox_scores <- platform_sentiment %>% filter(platform == "Xbox_X") %>%
  select(score)

playstation_scores <- platform_sentiment %>% filter(platform == "Playstation") %>%
  select(score)

#both are obviously not actually neutral in their sentiments and
#are statistically significant 
t.test(Xbox_scores$score, mu = 0)
t.test(playstation_scores, mu = 0)
##++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##_________________________________________(tl)Tweet Location_____________________________________##
playstation_english
playstation_english$location[playstation_english$location==""] <- NA
#Transforms the empty values into NA's                    

re_playstation <- playstation_english %>% mutate(location_rec =
                                           recode(location, "United Kingdom" = "UK","London, England" = "UK",
                                                  "Colorado, USA" = "United States", "New Jersey USA" = "United States",
                                                  "San Francisco, CA" = "United States", "England, United Kingdom" = "UK",
                                                  "Los Angeles, CA" = "United States", "California, USA" = "United States",
                                                  "USA" = "United States", "Canada <U+0001F1E8><U+0001F1E6>" = "Canada",
                                                  "Toronto, Ontario" = "Canada", "New York, USA" = "United States",
                                                  "New York, NY" = "United States", "Bronx, NY" = "United States",
                                                  "Texas, USA" = "United States" , "Big Apple" = "United States", "Toledo/Orlando" = "United States",
                                                  "Calgary, Alberta" = "Canada", "Florida, USA" = "United States", "Ontario Canada" = "Canada",
                                                  "Melbourne, Victoria" = "Australia", "Chicago, IL" = "United States", "London" = "UK", "Lafayette, LA"
                                                  = "United States", "Sheffield, England" = "UK", "Scotland" = "UK", "North Pole, AK" = "United States",
                                                  "London" = "UK", "Chicago, IL"= "United States", "Córdoba/Spain" = "Spain",
                                                  "Florida, USA" = "United States", "London, UK" = "UK", "Sweden/Stockholm" = "Sweden",
                                                  "Caerphilly, South Wales, UK" = "UK", "Dallas, TX" = "United States", "Virginia, USA"
                                                  = "United States", "Deutschland" = "Germany", "NJ" = "United States", "Seattle, WA" = 
                                                    "United States", "Bari, Puglia" = "Italy", "New York" = "United States", "Ohio, USA"=
                                                    "United States", "England" = "UK", "Atlanta, GA" = "United States", "Washington, DC"
                                                  = "United States", "New Jersey, USA" = "United States", "Manchester, England" = "UK",
                                                  "Scotland, United Kingdom" = "UK", "Nordrhein-West., DE, Iserlohn" = "Germany", "British Columbia, Canada"
                                                  = "Canada", "Brooklyn, NY" = "United States", "Nashville, TN" = "United States",
                                                  "Pennsylvania, USA" = "United States", "Perth, Western Australia" = "Australia", "Sydney, Australia" = "Australia",
                                                  "Wales, United Kingdom" = "UK", "Arizona, USA" = "United States", "East, England" = "UK"
                                                  , "San Antonio, TX" = "United States", "Toronto" = "Canada", "Glasgow, Scotland" = "UK",
                                                  "Washington D.C." = "United States", "Moscow" = "Russia", "North Carolina, USA" = "United States",
                                                  "Inglewood,Ca" = "United States", "Melbourne" = "Australia", "Melbourne, Australia" = "Australia",
                                                  "Melbourne. Australia" = "Australia", "Nova Scotia" = "Canada", "Melbourne " = "Australia", 
                                                  "Sverige" = "Sweden", "Sydney, New South Wales" = "Australia", "Garden City, KS" = "United States"))
#This renames the locations from their cities or regions and renames them to the country to get a clearer understanding of where they are all
#coming from


#
ps_loc_graph <- re_playstation %>%
  filter(!location_rec %in% c("Worldwide", "A PLANET N OUTER SPACE <U+0001F30D><U+0001F30D>", "Born in Night City ",
                              "patreon.com/germanstrands", "Mistake Island", "Ragnarok", "Nirvana, Outer Space", "twitch.tv/xbmnetwork", "Earth", "/usr/optimus_code",
                              "Everywhere & Nowhere", "Interwebs", "XBL", "Bad Vibes Forever")) %>%
  #This line filters out any joke locations or locations with strange values
  count(location_rec, sort = TRUE) %>%  #Orders from most to least
  mutate(location_rec = reorder(location_rec,n)) %>%
  na.omit() %>% # remove NAs
  head(10) %>% #Shows 10 results
  ggplot(aes(x = location_rec,y = n)) +
  geom_col(fill = "dark blue") + 
  #mutate(location_rec = reorder(location_rec,n)) %>%
  coord_flip() +
  labs(x = "Top Locations",
       y = "Number of Tweets",
       title = "Locations of tweets mentioning the PlayStation 5") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))
#                          

#_____________________________________________________________________#
## XBOX SERIES X ##


xbox_x_english$location[xbox_x_english$location==""] <- NA

#Transforms the empty values into NA's    

re_xboxX <- xbox_x_english %>% mutate(location_rec =
                               recode(location, "United Kingdom" = "UK","London, England" = "UK",
                                      "Colorado, USA" = "United States", "New Jersey USA" = "United States",
                                      "San Francisco, CA" = "United States", "England, United Kingdom" = "UK",
                                      "Los Angeles, CA" = "United States", "California, USA" = "United States",
                                      "USA" = "United States", "Canada <U+0001F1E8><U+0001F1E6>" = "Canada",
                                      "Toronto, Ontario" = "Canada", "New York, USA" = "United States",
                                      "New York, NY" = "United States", "Bronx, NY" = "United States",
                                      "Texas, USA" = "United States" , "Big Apple" = "United States", "Toledo/Orlando" = "United States",
                                      "Calgary, Alberta" = "Canada", "Florida, USA" = "United States", "Ontario Canada" = "Canada",
                                      "Melbourne, Victoria" = "Australia", "Chicago, IL" = "United States", "London" = "UK", "Lafayette, LA"
                                      = "United States", "Sheffield, England" = "UK", "Scotland" = "UK", "North Pole, AK" = "United States",
                                      "London" = "UK", "Chicago, IL"= "United States", "Córdoba/Spain" = "Spain",
                                      "Florida, USA" = "United States", "London, UK" = "UK", "Sweden/Stockholm" = "Sweden",
                                      "Caerphilly, South Wales, UK" = "UK", "Dallas, TX" = "United States", "Virginia, USA"
                                      = "United States", "Deutschland" = "Germany", "NJ" = "United States", "Seattle, WA" = 
                                        "United States", "Bari, Puglia" = "Italy", "New York" = "United States", "Ohio, USA"=
                                        "United States", "England" = "UK", "Atlanta, GA" = "United States", "Washington, DC"
                                      = "United States", "New Jersey, USA" = "United States", "Manchester, England" = "UK",
                                      "Scotland, United Kingdom" = "UK", "Nordrhein-West., DE, Iserlohn" = "Germany", "British Columbia, Canada"
                                      = "Canada", "Brooklyn, NY" = "United States", "Nashville, TN" = "United States",
                                      "Pennsylvania, USA" = "United States", "Perth, Western Australia" = "Australia", "Sydney, Australia" = "Australia",
                                      "Wales, United Kingdom" = "UK", "Arizona, USA" = "United States", "East, England" = "UK"
                                      , "San Antonio, TX" = "United States", "Toronto" = "Canada", "Glasgow, Scotland" = "UK",
                                      "Washington D.C." = "United States", "Moscow" = "Russia", "North Carolina, USA" = "United States",
                                      "Inglewood,Ca" = "United States", "Melbourne" = "Australia", "Melbourne, Australia" = "Australia",
                                      "Melbourne. Australia" = "Australia", "Nova Scotia" = "Canada", "Melbourne " = "Australia", 
                                      "Sverige" = "Sweden", "Sydney, New South Wales" = "Australia", "Garden City, KS" = "United States"))



#This renames the locations from their cities or regions and renames them to the country to get a clearer understanding of where they are all
#coming from



xbox_loc_graph <- re_xboxX %>%
  filter(!location_rec %in% c("Worldwide", "A PLANET N OUTER SPACE <U+0001F30D><U+0001F30D>", "Born in Night City ",
                              "patreon.com/germanstrands", "Mistake Island", "Ragnarok", "Nirvana, Outer Space", "twitch.tv/xbmnetwork", "Earth", "/usr/optimus_code",
                              "Everywhere & Nowhere", "Interwebs", "XBL", "Bad Vibes Forever")) %>%
  #This line filters out any joke locations or locations with strange values
  count(location_rec, sort = TRUE) %>%  #Order from highest to lowest
  mutate(location_rec = reorder(location_rec,n)) %>%
  na.omit() %>% # remove NAs
  head(10) %>% #Shows 10 results
  ggplot(aes(x = location_rec,y = n)) +
  geom_col(fill = "Green") +
  coord_flip() +
  labs(x = "Top Locations",
       y = "Number of Tweets",
       title = "Locations of tweets mentioning the Xbox Series X") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))


xbox_loc_graph + ps_loc_graph #Patchwork Lib lets you save graphs to variables and print them out right next too eachother

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
#____________________________Word Cloud (wc)_____________________________#
####_________________________Xbox___________________###

xbox_x_english$stripped_text <- gsub("https\\S*", "", xbox_x_english$text) 
xbox_x_english$stripped_text <-gsub("@\\S*", "", xbox_x_english$stripped_text) 
xbox_x_english$stripped_text <-gsub("amp", "", xbox_x_english$stripped_text) 


xbox_clean <- xbox_x_english %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% 
  unnest_tokens(word, stripped_text)
data("stop_words")


xbox_clean_words <- xbox_clean %>%
  anti_join(stop_words)


wc_my_stop_words <- data.frame(word = c("dont", "wanna", "people", "ps5","playstation","xbox","sony","xboxseriesx",
                                     "5", "ps4", "im","xboxseriess", "xbox", "series","time","microsoft","week","playstation5","day","days"))
xbox_clean_words_2 <- xbox_clean_words %>%
  anti_join(wc_my_stop_words) 


xbox_clean_words_3 <- xbox_clean_words_2 %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))

with(xbox_clean_words_3, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 200,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2")))
####_________________________PlayStation___________________###

playstation_english$stripped_text <- gsub("https\\S*", "", playstation_english$text) 
playstation_english$stripped_text <-gsub("@\\S*", "", playstation_english$stripped_text) 
playstation_english$stripped_text <-gsub("amp", "", playstation_english$stripped_text) 
playstation_english$stripped_text <-gsub("[\r\n]", "", playstation_english$text)
playstation_english$stripped_text <-gsub("[[:punct:]]", "", playstation_english$text)

ps_clean <- playstation_english %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% 
  unnest_tokens(word, stripped_text)

data("stop_words")

ps_clean_words <- ps_clean %>%
  anti_join(stop_words)


ps_clean_words_2 <- ps_clean_words %>%
  anti_join(wc_my_stop_words) 

ps_clean_words_3 <- ps_clean_words_2 %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))

with(ps_clean_words_3, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 500,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2")))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##__________________________________Bar charts that shows tweet volume of price and hardware_________________________________#####


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##_________________________________Amount of tweets from offical xbox and playstation accounts________________________##

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
  scale_y_continuous() +   
  labs(x = "Date", y = "Number of Tweets", color = "Console Twitter Account",
      title = "Number of tweets the official Xbox and Playstation twitters posted") 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
##__________________________________________________Top ten most mentioned games(mg)_______________________________________________________##
##____________________________________________________________Fix Graph____________________________________________________________________##
mg_playstation_english <- playstation_english %>% select(text, created_at) %>% mutate(platform = "Playstation")
mg_xbox_x_english <- xbox_x_english %>% select(text, created_at) %>% mutate(platform = "Xbox")

mg_playstation_xbox_text <- rbind(mg_playstation_english, mg_xbox_x_english)


game_name <- c("Astro's Playroom","Demon's Souls",
               "Marvel's Spider-Man: Miles Morales","Sackboy: A Big Adventure",
               "Bugsnax","Godfall","The Pathless","Forza Horizon 4","Gears 5",
               "Gears Tactics","Grounded","Ori and the Will of the Wisps",
               "Bright Memory 1.0","Dead by Daylight","Enlisted",
               "Evergate","Manifold Garden","Tetris Effect: Connected",
               "The Touryst","The Falconeer","War Thunder",
               "Yes, Your Grace",  "Halo: Infinite", "Assassin's Creed Valhalla	",
               "Borderlands 3", "Call of Duty: Black Ops Cold War", "Cyberpunk 2077",
               "Destiny 2: Beyond Light", "Devil May Cry 5: Special Edition", "DiRT 5",
               "For Honor", "Fortnite", "Just Dance 2021", "Maneater", "Mortal Kombat 11 Ultimate",
               "NBA 2K21", "Observer: System Redux", "Planet Coaster", "Warhammer: Chaosbane - Slayer Edition",
               "Watch Dogs Legion", "Yakuza: Like a Dragon")

platform <- c("Playstation 5","Playstation 5","Playstation 5",
              "Playstation 5","Playstation 5","Playstation 5","Playstation 5",
              "Xbox Series X","Xbox Series X","Xbox Series X","Xbox Series X",
              "Xbox Series X","Xbox Series X","Xbox Series X",
              "Xbox Series X","Xbox Series X","Xbox Series X","Xbox Series X",
              "Xbox Series X","Xbox Series X","Xbox Series X","Xbox Series X",
              "Xbox Series X", "Multiplatform","Multiplatform", "Multiplatform",
              "Multiplatform", "Multiplatform", "Multiplatform", "Multiplatform",
              "Multiplatform", "Multiplatform", "Multiplatform", "Multiplatform",
              "Multiplatform", "Multiplatform", "Multiplatform", "Multiplatform",
              "Multiplatform", "Multiplatform", "Xbox Series X")

keyword <- c("astro","Souls","spiderman","sackboy","bugsnax","godfall",
             "pathless","forza","gears","tactics","grounded","wisps","bright memory",
             "daylight","enlisted","evergate","manifold","tetris",
             "touryst","falconeer","thunder","grace", "halo", "valhalla",
             "borderlands", "cod", "cyberpunk", "destiny", "devil", "dirt",
             "for honor", "fortnite", "just dance", "maneater", "mortal kombat", "nba", "observer",
             "coaster", "warhammer", "watchdogs", "yakuza")

genre <- c("Platformer","Action RPG","Action-Adventure","Platformer","Adventure","Action RPG",
           "Action-Adventure","Racing","Third-person Shooter","Strategy","Action Adventure","Platformer","First-person Shooter",
           "Survival Horror","First-person Shooter","Puzzle","Puzzle","Puzzle",
           "Action-Adventure","Action RPG","Simulator","Strategy", "First-person Shooter",
           "Action RPG", "First-person Shooter", "Action RPG", "First-person Shooter", "Action-Adventure",
           "Racing", "Fighting", "Third-person Shooter", "Musical", "Action-Adventure", "Fighting",
           "Sport", "Horror", "Sim", "Hack and Slash", "Action-Adventure", "RPG", "n/a")



games <- data.frame(name = game_name, genre = genre, keyword = keyword, platform = platform, stringsAsFactors = FALSE)


#returns the filtered tweets based on games in a dataframe
discussed_games_prep <- function(dataframe, regex, game_name, output_frame){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text) #strip the text of links etc etc
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text, platform, created_at) %>% mutate(tweet_number = row_number()) #add tweet numbers to keep track
  
  #isolate the tweets containing the keyword from the dataframe, unnest the text into singular words and filter the stop words. 
  intermediate2 <- intermediate1 %>% filter(grepl(regex, stripped_text, ignore.case = TRUE)) %>% mutate(game = game_name)
}


#search for tweets concerning all 41 games in the dataframe and return each as a list 
games_by_volume_list <- list()
for (i in 1:length(games$name)) {
  games_by_volume_list[[i]] <- discussed_games_prep(mg_playstation_xbox_text, games$keyword[i], games$name[i])
}

launch_games_tweets <- do.call(rbind, games_by_volume_list)
launch_games_tweets <- launch_games_tweets %>% group_by(game) %>%
  mutate(tweet_count = n())

#dataframe now ready to use for further analysis 


#most discussed games within the entire dataset, ordered from ost to least
launch_games_tweets %>% 
  ggplot(aes(x = reorder(game, -tweet_count))) +
  geom_bar(stat="count") +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2)) +
  labs(x = "Game", y = "Number of Tweets",
       title = "Number of times a certain game was mentioned in a tweet",
       subtitle = "Twitter status (tweet) counts aggregated containing keywords",
       caption = "Source: Data collected from Twitter's REST API via rtweet")




#---------------------------TOP 10 GAME SENTIMENT--------------------------------#

#establish the top 10 discussed games by n- of tweets
top10_filter <- launch_games_tweets %>% count(game) %>% arrange(desc(n)) %>% head(10)

#filter tweet list to just have the top 10 games 
top10_tweets <- launch_games_tweets %>% filter(game %in% top10_filter$game)
console_my_stop_words <- data.frame(word = c("playstation","ps5","PS5","xbox","gaming","games","game"))

#unnest text to analyse 
top10games_unnested <- top10_tweets %>% select(stripped_text, tweet_number, created_at) %>% unnest_tokens(word, stripped_text)

#introduce stop words 
top10games_sentiment <- top10games_unnested %>% anti_join(stop_words, by = c("word" = "word"))
top10games_sentiment_2 <- top10games_sentiment %>%  anti_join(console_my_stop_words, by = c("word" = "word"))

#using binary sentiment library
game_sentiment <- top10games_sentiment_2 %>%
  inner_join(get_sentiments("bing")) %>%
  mutate(sentiment = ifelse(word == "hype", "positive", sentiment))

#overall score per tweet with time included for more detailed analysis 
game_sentiment_score <- game_sentiment %>% count(tweet_number, created_at, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% mutate(score = positive - negative)


#mean sentiment for each game
mean_sentiment_game <- game_sentiment_score %>% group_by(game) %>% summarise(avg_sentiment = mean(score))
#cyberpunk held down by delay news that came out october 27th

#multi variable t-test for the sentiments of each game 


#sentiment of games every 6 hours
game_sentiment_6_hour <- game_sentiment_score %>% mutate(six_hour_intervals = cut.POSIXt(created_at, breaks = "6 hours")) %>% 
  group_by(game, six_hour_intervals) %>% summarise(avg_sentiment = mean(score))

game_sentiment_6_hour  %>% group_by(game) %>% ggplot(aes(x = six_hour_intervals, y = avg_sentiment, color = game,
                                                         group = game)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2))

#sentiment of games every day
game_sentiment_one_day <- game_sentiment_score %>% mutate(one_day_intervals = cut.POSIXt(created_at, breaks = "1 day")) %>% 
  group_by(game, one_day_intervals) %>% summarise(avg_sentiment = mean(score))

game_sentiment_one_day  %>% group_by(game) %>% ggplot(aes(x = one_day_intervals, y = avg_sentiment, color = game,
                                                          group = game)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2))





















# volume of tweets over time for the top 10 games



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
###### Top 10 Games Keyword Contrast###
launch_games_tweets <- do.call(rbind, games_by_volume_list)

#change date formula as a variable
date <- as.Date(as.POSIXct(launch_games_tweets$created_at, format = '%Y%M%D'))

#ass the variable into the previous data frame
add_date <- cbind(launch_games_tweets,date)

#group_by games by date
group <- add_date %>% group_by(game,date) %>% count()

#only group_by games
group2 <- add_date %>% group_by(game) %>% count()

#get top 10 games with the most tweests
get_top_10 <- group2 %>% arrange(desc(n)) %>% head(10)
get_top_10

#filter the df with the date by the list of top 10 games
get_top_102 <- filter(group,game %in% get_top_10$game)

ggplot(get_top_102, aes(x = date, y = n, color = game)) +
  geom_line()+
  labs(x = NULL, y = NULL, 
       title = "Frequency of Tweets of Games",
       subtitle = "Twitter status (tweet) counts aggregated using 1-days intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
####### Frequency of topics reagrds to console###

topic_prep <- function(dataframe, regex){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text) #strip the text of links etc etc
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text,created_at) %>% mutate(tweet_number = row_number()) #add tweet numbers to keep track
  
  #isolate the tweets containing the keyword from the dataframe, unnest the text into singular words and filter the stop words. 
  intermediate2 <- intermediate1 %>% filter(grepl(regex, stripped_text, ignore.case = TRUE)) %>% mutate(topic = regex)
}

#search each keyword
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


#psuedo log10
ggplot(all, aes(n, topic, fill = origin)) +
  geom_col() +
  scale_x_continuous(trans = pseudolog10_trans, labels = abs, breaks = waiver()) + 
  labs(x = NULL, y = NULL,
       title = "Frequency of discussed topics regards to each console",
       subtitle = "Twitter status (tweet) counts aggregated containing keywords",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  scale_fill_manual(name = "Console", values = c("#6699FF", "#FF6699"), 
                    labels = c("all_ps_1" = "Playstation 5", "all_xb_1" = "Xbox series X")) 


#continous scaling
ggplot(all, aes(n, topic, fill = origin)) +
  geom_col() +
  scale_x_continuous(labels = abs) +
  labs(x = NULL, y = NULL,
       title = "Frequency of discussed topics regards to each console",
       subtitle = "Twitter status (tweet) counts aggregated containing keywords",
       caption = "Source: Data collected from Twitter's REST API via rtweet") +
  scale_fill_manual(name = "Console", values = c("#6699FF", "#FF6699"), 
                    labels = c("all_ps_1" = "Playstation 5", "all_xb_1" = "Xbox series X"))
#topics as a proportion of tweets 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#


















