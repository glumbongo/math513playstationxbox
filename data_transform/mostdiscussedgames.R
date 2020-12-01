library(tidyverse)
library(tidytext)

setwd("university/math513/coursework/presentation/csv_files")



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
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2))




#need to load in launch_games_tweets

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
