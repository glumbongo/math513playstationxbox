library(tidyverse)
library(tidytext)

playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)


#import generic stop words. others can be included by editing the function 
data("stop_words")

#function takes the search_tweets dataframe and a keyword/regex query to search by. | operator is used as 'or' in regex and may be useful
genre_analysis_prep <- function(dataframe, regex){
  
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
  newname <- dataframe_sentiment %>% count(tweet_number, sentiment) %>%
    spread(sentiment, n, fill = 0) %>% # negative and positive sentiment in separate columns
    mutate(score = positive - negative, keyword = regex)
}





