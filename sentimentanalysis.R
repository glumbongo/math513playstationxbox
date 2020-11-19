library(tidyverse)
library(rtweet)
library(tidytext)

setwd("C://users/glumb/documents/university/math513/coursework/presentation")

playstation_all_lang <- read_csv("playstation_tweets_alllang.csv", col_names = TRUE)
playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_all_lang <-  read_csv("xboxseriesx_tweets_alllang.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)
xbox_s_english <-  read_csv("xboxseriess_tweets_english.csv", col_names = TRUE)

data("stop_words") #generic stopwords from library

#custom stop word list. could be improved/changed for final analysis.
#'games' being filtered may be an issue when it comes to determining issues that best inform 
#sentiment
my_stop_words <- data.frame(word = c("playstation","ps5","PS5","xbox","gaming","games","game"))




#creates new data frames with the tweet number, platform, sentiment score, date of tweet
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
  
  
  
  newname <- newname %>% mutate(platform = inputtopic)
  newname <- left_join(newname, created_list, by = "tweet_number")
  
}


xbox_sentiment <- sentiment_analysis_prep(xbox_x_english, "Xbox_X")
playstation_sentiment <- sentiment_analysis_prep(playstation_english, "Playstation")

#bind dataframes, factorise platform
platform_sentiment <- rbind(xbox_sentiment, playstation_sentiment)
platform_f <- factor(platform_sentiment$platform)
platform_sentiment <- platform_sentiment %>% mutate(platform = platform_f)



# write_csv(platform_sentiment, file = "platform_sentiment_by_tweet.csv")


#checking row numbers for platforms. read online that sample size doesnt
#actually matter for t.test but not 100% sure
platform_sentiment %>% group_by(platform) %>% summarise(rows = n())


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

