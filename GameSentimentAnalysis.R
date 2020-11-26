library(tidyverse)
library(tidytext)



setwd("C://Users/glumb/documents/university/math513/coursework/presentation")

playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)


#import generic stop words. others can be included by editing the function 
data("stop_words")

##Getting tweets only regarding 3 exclusives of each console

halo <- data.frame(tweet = xbox_x_english$text[grep("Halo|halo|Infinite|infinite", xbox_x_english$text) ],game = "Halo: Infinite")
yakuza <- data.frame(tweet = xbox_x_english$text[grep("Yakuza|yakuza|like a dragon| Like a dragon", xbox_x_english$text) ],game = "Yakuza: Like A Dragon")
forza <- data.frame(tweet = xbox_x_english$text[grep("forza|Forza", xbox_x_english$text) ],game = "Forza: Motorsports")


spiderman <- data.frame(tweet = playstation_english$text[grep("Spiderman|spiderman|spider man|Spider Man|Spider man| Miles Morales", playstation_english$text) ],game = "Spiderman: Miles Morales")
demonsouls<- data.frame(tweet = playstation_english$text[grep("Demonsouls|demonsouls|Demon Souls|demon souls|Demon souls|Demon's Souls|demon's souls", playstation_english$text) ],game = "Demon's Souls")
sackboy<- data.frame(tweet = playstation_english$text[grep("Sackboy|sackboy", playstation_english$text) ],game = "Sackboy: A Big Adventure")




#function takes the search_tweets dataframe and a keyword/regex query to search by. | operator is used as 'or' in regex and may be useful
genre_analysis_prep <- function(dataframe, regex, dataname){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text) #strip the text of links etc etc
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text) %>% mutate(tweet_number = row_number()) #add tweet numbers to keep track
  
  #isolate the tweets containing the keyword from the dataframe, unnest the text into singular words and filter the stop words. 
  intermediate2 <- intermediate1 %>% filter(grepl(regex, stripped_text, ignore.case = TRUE)) %>%
    unnest_tokens(word, stripped_text) %>% anti_join(stop_words)
  
  #apply the sentiment analysis library, make hype positive
  dataframe_sentiment <- intermediate2 %>% 
    inner_join(get_sentiments("bing")) %>% 
    mutate(sentiment = ifelse(word == "hype", "positive", sentiment))
  
  #score each tweet for sentiment and add the keyword to each column so that dataframes can later be joined
  newname <- dataframe_sentiment %>% count(tweet_number, sentiment, Game = dataname) %>%
    spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
    mutate(score = positive - negative, keyword = regex)
}

yakuza_prep <- genre_analysis_prep(xbox_x_english, "Yakuza|yakuza|like a dragon|Like a dragon", "Yakuza: Like a Dragon")
halo_prep <- genre_analysis_prep(xbox_x_english, "Halo|halo|infinite|Infinite", "Halo: Infinite")
forza_prep <- genre_analysis_prep(xbox_x_english, "forza|Forza", "Forza: Motorsports")
gears_prep <- genre_analysis_prep(xbox_x_english, "gears", "gears of war")


spiderman_prep <- genre_analysis_prep(playstation_english, "Spiderman|spiderman|spider man|Spider Man|Spider man| Miles Morales", "Spiderman: Miles Morales")
demonsouls_prep <- genre_analysis_prep(playstation_english, "Demonsouls|demonsouls|Demon Souls|demon souls|Demon souls|Demon's Souls|demon's souls", "Demon's Souls")
godfall_prep <- genre_analysis_prep(playstation_english, "Godfall|godfall", "Godfall")
sackboy_prep <- genre_analysis_prep(playstation_english, "Sackboy|sackboy|lbp|littlebigplanet", "Sackboy: A Big Adventure")


##Combinging the exclusives for each game into one dataset for manipulation if needed later on
xbox_game_sentiment <- rbind(yakuza_prep,halo_prep,forza_prep,gears_prep)
xbox_game_sentiment_f <- factor(xbox_game_sentiment$Game)
xbox_game_sentiment<- xbox_game_sentiment %>% mutate(Game = xbox_game_sentiment_f)

ps_game_sentiment <- rbind(spiderman_prep,demonsouls_prep,godfall_prep, sackboy_prep)
ps_game_sentiment_f <- factor(ps_game_sentiment$Game)
ps_game_sentiment<- ps_game_sentiment %>% mutate(Game = ps_game_sentiment_f)

#write the data to a csv file
# write_csv(xbox_game_sentiment, file = "xbox_game_sentiment_by_tweet.csv")
# write_csv(ps_game_sentiment, file = "ps_game_sentiment_by_tweet.csv")

#check row for each game
xbox_game_sentiment %>% group_by(Game) %>% summarise(rows = n(), score = mean(score))
ps_game_sentiment %>% group_by(Game) %>% summarise(rows = n(), score = mean(score))

##We might just want to do two rather than three exclusives due to the limiting amount of data and the high P value we are getting from the ones without much data??
##t-Test Per Game (xbox)
t.test(yakuza_prep$score)
t.test(halo_prep$score) 
t.test(forza_prep$score)

##t-Test Per Game (ps)
t.test(spiderman_prep$score)
t.test(demonsouls_prep$score)
#t.test(sackboy_prep$score) #Got rid of sackboy as it had a pvalue of a whole !!!!??
t.test(godfall_prep$score)


#combined exclusvies test
t.test(xbox_game_sentiment$score)
t.test(ps_game_sentiment$score)

xbox_game_sentiment %>% ggplot(aes(x=score)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "xbox")

ps_game_sentiment %>% ggplot(aes(x=score)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "playstation")

#xbox no halo 
xbox_no_halo <- xbox_game_sentiment %>% filter(Game != "Halo: Infinite")

xbox_no_halo %>% ggplot(aes(x=score)) +
  geom_histogram() +
  scale_y_log10() +
  labs(title = "xbox no halo")

mean(xbox_no_halo$score)
t.test(xbox_no_halo$score)

#conclusions:
# xbox has a statistically significant positive sentiment in regards
# to its launch games despite having it's biggest launch game delayed
#playstation game sentiment doesnt pass null hypothesis and is neutral