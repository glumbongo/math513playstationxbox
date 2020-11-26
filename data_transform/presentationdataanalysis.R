library(tidyverse)
library(rtweet)
library(tidytext)

setwd("C://users/glumb/documents/university/math513/coursework/presentation")

playstation_all_lang <- read_csv("playstation_tweets_alllang.csv", col_names = TRUE)
playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_all_lang <-  read_csv("xboxseriesx_tweets_alllang.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)
xbox_s_english <-  read_csv("xboxseriess_tweets_english.csv", col_names = TRUE)

data("stop_words")
my_stop_words <- data.frame(word = c("playstation","ps5","PS5","xbox","gaming","games","game"))

sentiment_analysis_prep <- function(dataframe, inputtopic){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text)
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text, created_at) %>% mutate(tweet_number = row_number()) %>%
    unnest_tokens(word, stripped_text)
  
  intermediate2 <- intermediate1 %>% anti_join(stop_words) %>% anti_join(my_stop_words)

  dataframe_sentiment <- intermediate2 %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(sentiment = ifelse(word == "hype", "positive", sentiment))

  created_list <- dataframe_sentiment %>% select(created_at, tweet_number) %>% distinct() %>% mutate(created_at = as.POSIXct(created_at))

  newname <- dataframe_sentiment %>% count(tweet_number, sentiment) %>%
     spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
     mutate(score = positive - negative) # score = net sentiment (positive - negative)



  newname <- newname %>% mutate(topic = inputtopic)
  newname <- left_join(newname, created_list, by = "tweet_number")
  
}

genre_analysis_prep <- function(dataframe){
  
  dataframe$stripped_text <- gsub("http.*","", dataframe$text)
  dataframe$stripped_text <- gsub("https.*","", dataframe$stripped_text)
  dataframe$stripped_text <- gsub("amp","", dataframe$stripped_text)
  
  intermediate1 <- dataframe %>% select(stripped_text) %>% mutate(tweet_number = row_number()) %>%
    unnest_tokens(word, stripped_text)
  
  # intermediate2 <- intermediate1 %>% anti_join(stop_words) %>% anti_join(my_stop_words)
  # 
  # dataframe_sentiment <- intermediate2 %>% 
  #   inner_join(get_sentiments("bing")) %>% 
  #   mutate(sentiment = ifelse(word == "hype", "positive", sentiment))
}


xbox_sentiment <- sentiment_analysis_prep(xbox_x_english, "Xbox_X")
playstation_sentiment <- sentiment_analysis_prep(playstation_english, "Playstation")

platform_sentiment <- rbind(xbox_sentiment, playstation_sentiment)
platform_f <- factor(platform_sentiment$topic)
platform_sentiment <- platform_sentiment %>% mutate(topic = platform_f)

colnames(platform_sentiment)

platform_sentiment <- platform_sentiment %>% rename(platform = topic)
write_csv(platform_sentiment, file = "platform_sentiment_by_tweet.csv")

platform_sentiment <- read_csv("platform_sentiment_by_tweet.csv")
platform_f <- factor(platform_sentiment$platform)
platform_sentiment <- platform_sentiment %>% mutate(platform = platform_f)


platform_sentiment %>% group_by(platform) %>% summarise(rows = n())

t.test(score ~platform, data = platform_sentiment, var.equal = TRUE)






xbox_genre_analysis <- genre_analysis_prep(xbox_x_english)  #this gives us the words seperated with a tweet number ID so 

sum(str_count(xbox_genre_analysis$word, "ori")) #returns count of mentions of game keywords


#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------#

semi_join() # return all rows in x with a match in y




playstation_english$stripped_text <- gsub("http.*","", playstation_english$text)
playstation_english$stripped_text <- gsub("https.*","", playstation_english$stripped_text)
playstation_english$stripped_text <- gsub("amp","", playstation_english$stripped_text)
head(playstation_english$stripped_text)

playstation_english_clean <- playstation_english %>% select(stripped_text) %>%  #clean up each tweet, strip it of punctuation etc and organise individual words by tweet 
  mutate(tweet_number = row_number()) %>% unnest_tokens(word, stripped_text)

#remove stop words
data("stop_words")

playstation_english_clean_nostop <- playstation_english_clean %>% anti_join(stop_words) #return all rows that arent stop words

playstation_english_clean_nostop %>% count(word, sort = TRUE) %>%  #useless
  head(30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  labs(X = "Unique Words",
       y = "Frequency",
       title = "Top 10 most popular words in discussion of the Playstation 5")

playstation_english_clean_nostop %>% count(word, sort = TRUE) %>%  #useless
  head(30)

tweet_topics <- c("price","exclusives", "specs","design")  #CREATE SUB LISTS THAT CAPTURE MORE DATA HERE

#list the number of tweets in playstation based on our topics in tweet topics

playstation_english_clean_nostop %>% count(word, sort = TRUE) %>% 
  mutate(reorder(word, n)) %>% 
  filter(word %in% tweet_topics) %>% ggplot(aes(x = word, y = n)) +
  geom_col() +
  labs(x = "Topics", y = "Frequency", title = "Number of tweets relating to vital topics for analysis")

#wordclouds

my_stop_words <- data.frame(word = c("playstation","ps5","PS5","xbox","gaming","games","game"))
playstation_english_clean_nostop_custom <- playstation_english_clean_nostop %>% anti_join(my_stop_words)

#CREATE WORDCLOUDS HERE LATER


#sentiment analysis 

sentiments

bing_playstation <- playstation_english_clean_nostop_custom %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  mutate(word = reorder(word, n)) 
head(bing_playstation)

giveaway_words <- c("win","free")

bing_playstation_filtered <- bing_playstation %>% filter(!word %in% giveaway_words)
head(bing_playstation_filtered)

bing_playstation_filtered %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip()      #shows most popular positive and negative words for tweets about playstation. minimal usage


#sentiment scores 

get_sentiments("bing") 

playstation_sentiment <- playstation_english_clean_nostop_custom %>% 
  inner_join(get_sentiments("bing"))

playstation_sentiment <- playstation_sentiment %>% mutate(sentiment = ifelse(word == "hype", "positive", sentiment))



playstation_sentiment <- playstation_sentiment %>%
  count(tweet_number, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
  mutate(score = positive - negative) # score = net sentiment (positive - negative)
head(playstation_sentiment)

playstation_sentiment <- playstation_sentiment %>% mutate(topic = "playstation")

playstation_sentiment %>% count(score)
playstation_sentiment_mean <- playstation_sentiment %>% summarise(mean_score = mean(score))


#GRAPHS SENTIMENT ANALYSIS

playstation_sentiment %>%
  ggplot(aes(x = score)) +
  geom_bar() +
  labs() +
  scale_x_continuous(breaks = -5:6, 
                     minor_breaks = NULL) +
  scale_y_log10()

xbox_sentiment %>%
  ggplot(aes(x = score)) +
  geom_bar() +
  labs() +
  scale_x_continuous(breaks = -5:6, 
                     minor_breaks = NULL) +
  scale_y_log10()

xbox_sentiment %>% summarise(mean_score = mean(score), sd_score = sd(score))
playstation_sentiment %>% summarise(mean_score = mean(score), sd_score = sd(score))
xbox_sentiment %>% count(score)

#checking sd calculation is correct
sqrt( sum(( xbox_sentiment$score - (sum(xbox_sentiment$score)/nrow(xbox_sentiment)))^2/nrow(xbox_sentiment)  ))
sqrt( sum(( playstation_sentiment$score - (sum(playstation_sentiment$score)/nrow(playstation_sentiment)))^2/nrow(playstation_sentiment)  ))
