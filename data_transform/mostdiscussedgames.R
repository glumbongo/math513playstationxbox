library(tidyverse)
library(tidytext)

playstation_english <-  read_csv("playstation_tweets_english.csv", col_names = TRUE)
xbox_x_english <-  read_csv("xboxseriesx_tweets_english.csv", col_names = TRUE)

playstation_english <- playstation_english %>% select(text) %>% mutate(platform = "Playstation")
xbox_x_english <- xbox_x_english %>% select(text) %>% mutate(platform = "Xbox")

playstation_xbox_text <- rbind(playstation_english, xbox_x_english)


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
  
  intermediate1 <- dataframe %>% select(stripped_text, platform) %>% mutate(tweet_number = row_number()) #add tweet numbers to keep track
  
  #isolate the tweets containing the keyword from the dataframe, unnest the text into singular words and filter the stop words. 
  intermediate2 <- intermediate1 %>% filter(grepl(regex, stripped_text, ignore.case = TRUE)) %>% mutate(game = game_name)
}


#search for tweets concerning all 41 games in the dataframe and return each as a list 
games_by_volume_list <- list()
for (i in 1:length(games$name)) {
  games_by_volume_list[[i]] <- discussed_games_prep(playstation_xbox_text, games$keyword[i], games$name[i])
}

launch_games_tweets <- do.call(rbind, games_by_volume_list)
launch_games_tweets <- launch_games_tweets %>% group_by(game) %>%
  mutate(tweet_count = n())

#dataframe now ready to use for further analysis 



launch_games_tweets %>% ggplot(aes(x = reorder(game, -tweet_count))) +
  geom_bar(stat="count") +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2)
  )
