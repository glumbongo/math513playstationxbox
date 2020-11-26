# word cloud for playstation
playstation_english$stripped_text <- gsub("https\\S*", "", playstation_english$text) 
playstation_english$stripped_text <-gsub("@\\S*", "", playstation_english$stripped_text) 
playstation_english$stripped_text <-gsub("amp", "", playstation_english$stripped_text) 
playstation_english$stripped_text <-gsub("[\r\n]", "", playstation_english$text)
playstation_english$stripped_text <-gsub("[[:punct:]]", "", playstation_english$text)
head(playstation_english$stripped_text)

ps_clean <- playstation_english %>%
  select(stripped_text) %>% 
  mutate(tweetnumber = row_number()) %>% 
  unnest_tokens(word, stripped_text)
head(ps_clean)

data("stop_words")
head(stop_words)

nrow(ps_clean)

ps_clean_words <- ps_clean %>%
  anti_join(stop_words)

nrow(ps_clean_words)

my_stop_words <- data.frame(word = c("dont", "wanna", "people", "ps5","playstation","xbox","sony","xboxseriesx",
                                     "5", "ps4", "im","xboxseriess", "xbox", "series","time","microsoft","week","playstation5","day","days"))

ps_clean_words_2 <- ps_clean_words %>%
  anti_join(my_stop_words) 

nrow(ps_clean_words_2)

ps_clean_words_3 <- ps_clean_words_2 %>%
  count(word, sort = TRUE) %>% 
  mutate(freq = n / sum(n))
head(ps_clean_words_3)

with(ps_clean_words_3, 
     wordcloud(word, freq, 
               min.freq = 1, 
               max.words = 500,
               random.order = FALSE, 
               colors = brewer.pal(8, "Dark2")))

