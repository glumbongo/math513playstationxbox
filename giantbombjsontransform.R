library(jsonlite)
library(tidyverse)

setwd("C://users/glumb/documents/university/math513/coursework/presentation")

games <- fromJSON("giantbombxboxps5games.json", flatten = TRUE)

colnames(games)

games_2 <- games$results

games_3 <- games_2$platforms




test <- do.call("rbind", games_3) # this moves all of the variables into a dataframe but gives us 4 times too many results
test_2 <- test %>% filter(id == 176 | id == 179) #this  cuts to down but still cant be mjoined to df because no foreign key
games_2_fixed <- games_2 %>% mutate(platforms = test)

#add a unique id to each DF in games_3 then rbind and join based on foreign key



game_name <- c("Astro's Playroom","Demon's Souls",
               "Marvel's Spider-Man: Miles Morales","Sackboy: A Big Adventure",
               "Bugsnax","Godfall","The Pathless","Forza Horizon 4","Gears 5",
               "Gears Tactics","Grounded","Ori and the Will of the Wisps",
               "Bright Memory 1.0","Dead by Daylight","Enlisted",
               "Evergate","Manifold Garden","Tetris Effect: Connected",
               "The Touryst","The Falconeer","War Thunder",
               "Yes, Your Grace")

platform <- c("Playstation 5","Playstation 5","Playstation 5",
              "Playstation 5","Playstation 5","Playstation 5","Playstation 5",
              "Xbox Series X","Xbox Series X","Xbox Series X","Xbox Series X",
              "Xbox Series X","Xbox Series X","Xbox Series X",
              "Xbox Series X","Xbox Series X","Xbox Series X","Xbox Series X",
              "Xbox Series X","Xbox Series X","Xbox Series X","Xbox Series X")

keyword <- c("astro","Souls","spiderman","sackboy","bugsnax","godfall",
             "pathless","forza","gears","tactics","grounded","ori","bright memory",
             "daylight","enlisted","evergate","manifold","tetris",
             "touryst","falconeer","thunder","grace")

genre <- c("Platformer","Action RPG","Action-Adventure","Platformer","Adventure","Action RPG",
           "Action-Adventure","Racing","Third-person Shooter","Strategy","Action Adventure","Platformer","First-person Shooter",
           "Survival Horror","First-person Shooter","Puzzle","Puzzle","Puzzle",
           "Action-Adventure","Action RPG","Simulator","Strategy")

platform <- factor(platform)
genre <- factor(genre)

games <- data.frame(name = game_name, genre = genre, keyword = keyword, platform = platform)

write_csv(games, "")

games %>% group_by(genre) %>%
  ggplot(aes(x=genre, fill = platform)) +
  geom_bar() +
  facet_wrap(platform) +
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2),
        legend.position = "none")



games_test_2 <- read_csv("xboxgames.csv")
xbox_games <- games_test_2 %>% select(Title, `Genre(s)`, `Developer(s)`, `Publisher(s)`)


games_test_3 <- read_csv("ps5games.csv")
ps5_games <- games_test_3 %>% select(Title, `Genre(s)`, `Developer(s)`, `Publisher(s)`)
ps5_games[152,1] <- "The Witcher 3: Wild Hunt"
ps5_games[132,1] <- "The Stone of Madness"

ps5_games <- ps5_games %>% mutate(Title = ifelse(grepl("<span", Title), str_extract(Title, "(?<=>)(.*)"), Title)) 
xbox_games <- xbox_games %>% mutate(Title = ifelse(grepl("<span", Title), str_extract(Title, "(?<=>)(.*)"), Title)) 

third_party_games <- semi_join(ps5_games, xbox_games, by = "Title")
third_party_games <- third_party_games %>% mutate(Platform = "Multi-platform")

ps5_games <- ps5_games %>% mutate(Platform = "Playstation 5")
xbox_games <- xbox_games %>% mutate(Platform = "Xbox Series X")

psxbox_com <- rbind(ps5_games,xbox_games)

psxbox_com_test <- psxbox_com[!duplicated(psxbox_com$Title), ]
test_combined <- rbind(third_party_games, psxbox_com_test)


test_combined_finished <- test_combined[!duplicated(test_combined$Title),]


metroidvania <- "Platform-adventure\r\nMetroidvania"
survival_horror <- c("Survival horror","Stealth\r\nSurvival horror", "First-person shooter\r\nSurvival horror","Action-adventure\r\nSurvival horror",
                     "Puzzle-platformer\r\nsurvival horror")
horror_list <- c("Psychological horror")
action_adventure_list <- c("Action-adventure\r\nstealth","Action-adventure\r\nhack and slash","Action-adventure\r\nsurvival","Role-playing\r\nfarming simulation","3D platformer\r\nAction-adventure",
                           "Action-adventure\r\nfirst-person shooter")
simulation_list <- c("Fishing\r\nsimulation","Space station simulation","Amateur flight simulation")
racing <- c("Racing\r\nsimulation")
action_roleplaying <- c("Action role-playing souls-like", "Role-playing","Action role-playing\r\nhack and slash", "Japanese role-playing",
                        "Role-playing adventure","Role-playing simulation")
action <- c("Vehicular combat","Action\r\nvehicular combat\r\ncombat flight simulator")
fighting <- c("Fighting party","Action fighting")
strategy <- c("Strategy\r\nstealth","Artillery\r\nbattle royale\r\nstrategy","Turn-based role-playing","Turn-based strategy")
adventure <- c("Adventure puzzle","	Adventure creature collection")
third_person <- c("Loot shooter", "Battle royale\r\nsandbox survival")
platformer <- c("Adventure platform","3D platformer","Action-adventure platform")
tacticalshooter <- c("Tactical shooter")

test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% tacticalshooter , "First-person Shooter", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% metroidvania , "Metroidvania", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% survival_horror , "Survival", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% action_adventure_list , "Action adventure", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% simulation_list , "Simulation", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% racing , "Racing", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% action_roleplaying , "Action role-playing", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% action , "Action", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% fighting , "Fighting", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% strategy , "Strategy", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% adventure , "Adventure", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% third_person , "Third-person shooter", `Genre(s)`))
test_combined_finished <- test_combined_finished %>% mutate(`Genre(s)` = ifelse(`Genre(s)` %in% platformer , "Platformer", `Genre(s)`))




test_combined_finished[58,2] <- "Simulation"
test_combined_finished[5,2] <- "Action role-playing"
test_combined_finished[11,2] <- "Action-adventure"
test_combined_finished[4,2] <- "Strategy"
test_combined_finished[13,2] <- "Survival"
test_combined_finished[6,2] <- "Puzzle"
test_combined_finished[18,2] <- "Action-adventure"
test_combined_finished[27,2] <- "Battle Royale"
test_combined_finished[28,2] <- "Platformer"
test_combined_finished[31,2] <- "First-person shooter"
test_combined_finished[36,2] <- "Stealth"
test_combined_finished[43,2] <- "Survival"
test_combined_finished[48,2] <- "Platformer"
test_combined_finished[50,2] <- "Action role-playing"
test_combined_finished[52,2] <- "Action-adventure"
test_combined_finished[55,2] <- "Action"
test_combined_finished[70,2] <- "Action role-playing"
test_combined_finished[72,2] <- "Roguelike"
test_combined_finished[82,2] <- "Action role-playing"
test_combined_finished[84,2] <- "Action"
test_combined_finished[88,2] <- "Action role-playing"
test_combined_finished[89,2] <- "Stealth"
test_combined_finished[91,2] <- "Action role-playing"
test_combined_finished[93,2] <- "Platformer"
test_combined_finished[96,2] <- "Action role-playing"
test_combined_finished[106,2] <- "Action"
test_combined_finished[109,2] <- "Action role-playing"
test_combined_finished[111,2] <- "Action-adventure"
test_combined_finished[112,2] <- "Fighting"
test_combined_finished[115,2] <- "Racing"
test_combined_finished[128,2] <- "Action-adventure"
test_combined_finished[131,2] <- "Action role-playing"
test_combined_finished[132,2] <- "Action role-playing"
test_combined_finished[137,2] <- "Platformer"
test_combined_finished[139,2] <- "Action-adventure"
test_combined_finished[142,2] <- "Platformer"
test_combined_finished[144,2] <- "Metroidvania"
test_combined_finished[145,2] <- "Simulation"
test_combined_finished[150,2] <- "Stealth"
test_combined_finished[151,2] <- "Puzzle"
test_combined_finished[155,2] <- "Battle Royale"
test_combined_finished[157,2] <- "Action-adventure"
test_combined_finished[181,2] <- "Survival horror"
test_combined_finished[187,2] <- "Metroidvania"
test_combined_finished[191,2] <- "Platformer"
test_combined_finished[196,2] <- "Survival"
test_combined_finished[200,2] <- "Survival"
test_combined_finished[201,2] <- "Survival"
test_combined_finished[147,2] <- "Hack and slash"
test_combined_finished[41,2] <- "Role-playing"
test_combined_finished[73,2] <- "Adventure"
test_combined_finished[177,2] <- "Action-adventure"
test_combined_finished[205,2] <- "Survival"
test_combined_finished[206,2] <- "Role-playing"
test_combined_finished[95,2] <- "Adventure"
test_combined_finished[141,2] <- "Shoot 'em up"
test_combined_finished[38,2] <- "Psychological horror"
test_combined_finished[121,2] <- "Platformer"
test_combined_finished[20,2] <- "First-person shooter"
test_combined_finished[163,2] <- "Puzzle"
test_combined_finished[168,2] <- "Platformer"
test_combined_finished[10,2] <- "Adventure"
test_combined_finished[32,2] <- "Role-playing"
test_combined_finished[134,2] <- "Platformer"
test_combined_finished[97,2] <- "Shoot 'em up"
test_combined_finished[83,2] <- "Strategy"






test_combined_finished$`Genre(s)` %in% metroidvania
test_combined_finished$`Genre(s)` == survival_horror

test_combined_finished[1,3]



genre_factor <- factor(test_combined_finished$`Genre(s)`)
platform_factor <- factor(test_combined_finished$Platform)


test_combined_finished<- test_combined_finished %>% mutate(Genre = genre_factor)
test_combined_finished<- test_combined_finished %>% mutate(Platform = platform_factor)

write_csv(test_combined_finished, "releasegames.csv")



test_combined_finished %>% group_by(Genre) %>% ggplot(aes(x = Genre, fill = Platform)) +
  geom_bar() +
  
  theme(axis.text.x = element_text(angle = 90, size = 7, hjust = 1, vjust = 0.2)
        )
