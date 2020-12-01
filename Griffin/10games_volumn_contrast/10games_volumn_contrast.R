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


#combine all the list
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



