library(wordcloud)
library(tidytext)
xbox_x_english$stripped_text <- gsub("https\\S*", "", xbox_x_english$text) 
xbox_x_english$stripped_text <-gsub("@\\S*", "", xbox_x_english$stripped_text) 
xbox_x_english$stripped_text <-gsub("amp", "", xbox_x_english$stripped_text) 

head(xbox_x_english$stripped_text)

xbox_clean <- xbox_x_english %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% 
  unnest_tokens(word, stripped_text)
head(xbox_clean)

data("stop_words")
head(stop_words)

nrow(xbox_clean)

xbox_clean_words <- xbox_clean %>%
  anti_join(stop_words)

nrow(xbox_clean_words)

my_stop_words <- data.frame(word = c("dont", "wanna", "people", "ps5","playstation","xbox","sony","xboxseriesx",
                                     "5", "ps4", "im","xboxseriess", "xbox", "series","time","microsoft","week","playstation5","day","days"))
xbox_clean_words_2 <- xbox_clean_words %>%
  anti_join(my_stop_words) 

nrow(xbox_clean_words_2)

xbox_clean_words_3 <- xbox_clean_words_2 %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))
head(xbox_clean_words_3)


with(xbox_clean_words_3, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 200,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2")))

