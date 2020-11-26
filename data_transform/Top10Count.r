
library(rtweet)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(readr)
library(tidytext)
library(tidyr)
library(maps)
#

#This part of the code will gather the data from the english speaking data sets of each console, and show the top 10 countries
#that have tweeted about each console

##__________________________________________________________________________##
#Playstation

playStation <- read.csv("C://Users//ELISW//Desktop//math513playstationxbox-main//playstation_tweets_english.csv")

#Reads the CVS into a dataframe

playStation$location[playStation$location==""] <- NA
#Transforms the empty values into NA's                    
                       
re_playstation <- playStation %>% mutate(location_rec =
                                      recode(location, "United Kingdom" = "UK","London, England" = "UK",
                                             "Colorado, USA" = "United States", "New Jersey USA" = "United States",
                                             "San Francisco, CA" = "United States", "England, United Kingdom" = "UK",
                                             "Los Angeles, CA" = "United States", "California, USA" = "United States",
                                             "USA" = "United States", "Canada <U+0001F1E8><U+0001F1E6>" = "Canada",
                                             "Toronto, Ontario" = "Canada", "New York, USA" = "United States",
                                             "New York, NY" = "United States", "Bronx, NY" = "United States",
                                             "Texas, USA" = "United States" , "Big Apple" = "United States", "Toledo/Orlando" = "United States",
                                             "Calgary, Alberta" = "Canada", "Florida, USA" = "United States", "Ontario Canada" = "Canada",
                                             "Melbourne, Victoria" = "Australia", "Chicago, IL" = "United States", "London" = "UK", "Lafayette, LA"
                                             = "United States", "Sheffield, England" = "UK", "Scotland" = "UK", "North Pole, AK" = "United States",
                                             "London" = "UK", "Chicago, IL"= "United States", "Córdoba/Spain" = "Spain",
                                             "Florida, USA" = "United States", "London, UK" = "UK", "Sweden/Stockholm" = "Sweden",
                                             "Caerphilly, South Wales, UK" = "UK", "Dallas, TX" = "United States", "Virginia, USA"
                                             = "United States", "Deutschland" = "Germany", "NJ" = "United States", "Seattle, WA" = 
                                               "United States", "Bari, Puglia" = "Italy", "New York" = "United States", "Ohio, USA"=
                                               "United States", "England" = "UK", "Atlanta, GA" = "United States", "Washington, DC"
                                             = "United States", "New Jersey, USA" = "United States", "Manchester, England" = "UK",
                                             "Scotland, United Kingdom" = "UK", "Nordrhein-West., DE, Iserlohn" = "Germany", "British Columbia, Canada"
                                             = "Canada", "Brooklyn, NY" = "United States", "Nashville, TN" = "United States",
                                             "Pennsylvania, USA" = "United States", "Perth, Western Australia" = "Australia", "Sydney, Australia" = "Australia",
                                             "Wales, United Kingdom" = "UK", "Arizona, USA" = "United States", "East, England" = "UK"
                                             , "San Antonio, TX" = "United States", "Toronto" = "Canada", "Glasgow, Scotland" = "UK",
                                             "Washington D.C." = "United States", "Moscow" = "Russia", "North Carolina, USA" = "United States",
                                             "Inglewood,Ca" = "United States", "Melbourne" = "Australia", "Melbourne, Australia" = "Australia",
                                             "Melbourne. Australia" = "Australia", "Nova Scotia" = "Canada", "Melbourne " = "Australia", 
                                             "Sverige" = "Sweden", "Sydney, New South Wales" = "Australia", "Garden City, KS" = "United States"))
#This renames the locations from their cities or regions and renames them to the country to get a clearer understanding of where they are all
#coming from
                       
                       
#
re_playstation %>%
filter(!location_rec %in% c("Worldwide", "A PLANET N OUTER SPACE <U+0001F30D><U+0001F30D>", "Born in Night City ",
                            "patreon.com/germanstrands", "Mistake Island", "Ragnarok", "Nirvana, Outer Space", "twitch.tv/xbmnetwork", "Earth", "/usr/optimus_code",
                            "Everywhere & Nowhere", "Interwebs", "XBL", "Bad Vibes Forever")) %>%
  #This line filters out any joke locations or locations with strange values
count(location_rec, sort = TRUE) %>%  #Orders from most to least
mutate(location_rec = reorder(location_rec,n)) %>%
na.omit() %>% # remove NAs
head(10) %>% #Shows 10 results
ggplot(aes(x = location_rec,y = n)) +
geom_col(fill = "dark blue") + 
#mutate(location_rec = reorder(location_rec,n)) %>%
coord_flip() +
labs(x = "Top Locations",
y = "Number of Tweets",
title = "Locations of tweets mentioning the PlayStation 5") + 
theme(axis.text = element_text(size = 16, color = "black"), 
axis.title = element_text(size = 16, color = "black"),
title = element_text(size = 18))
#                          

#_____________________________________________________________________#
## XBOX SERIES X ##
xboxX <- read.csv("C://Users//ELISW//Desktop//math513playstationxbox-main//xboxseriesx_tweets_english.csv")

#Reads the CVS into a dataframe



xboxX$location[xboxX$location==""] <- NA

#Transforms the empty values into NA's    

re_xboxX <- xboxX %>% mutate(location_rec =
                                           recode(location, "United Kingdom" = "UK","London, England" = "UK",
                                                  "Colorado, USA" = "United States", "New Jersey USA" = "United States",
                                                  "San Francisco, CA" = "United States", "England, United Kingdom" = "UK",
                                                  "Los Angeles, CA" = "United States", "California, USA" = "United States",
                                                  "USA" = "United States", "Canada <U+0001F1E8><U+0001F1E6>" = "Canada",
                                                  "Toronto, Ontario" = "Canada", "New York, USA" = "United States",
                                                  "New York, NY" = "United States", "Bronx, NY" = "United States",
                                                  "Texas, USA" = "United States" , "Big Apple" = "United States", "Toledo/Orlando" = "United States",
                                                  "Calgary, Alberta" = "Canada", "Florida, USA" = "United States", "Ontario Canada" = "Canada",
                                                  "Melbourne, Victoria" = "Australia", "Chicago, IL" = "United States", "London" = "UK", "Lafayette, LA"
                                                  = "United States", "Sheffield, England" = "UK", "Scotland" = "UK", "North Pole, AK" = "United States",
                                                  "London" = "UK", "Chicago, IL"= "United States", "Córdoba/Spain" = "Spain",
                                                  "Florida, USA" = "United States", "London, UK" = "UK", "Sweden/Stockholm" = "Sweden",
                                                  "Caerphilly, South Wales, UK" = "UK", "Dallas, TX" = "United States", "Virginia, USA"
                                                  = "United States", "Deutschland" = "Germany", "NJ" = "United States", "Seattle, WA" = 
                                                    "United States", "Bari, Puglia" = "Italy", "New York" = "United States", "Ohio, USA"=
                                                    "United States", "England" = "UK", "Atlanta, GA" = "United States", "Washington, DC"
                                                  = "United States", "New Jersey, USA" = "United States", "Manchester, England" = "UK",
                                                  "Scotland, United Kingdom" = "UK", "Nordrhein-West., DE, Iserlohn" = "Germany", "British Columbia, Canada"
                                                  = "Canada", "Brooklyn, NY" = "United States", "Nashville, TN" = "United States",
                                                  "Pennsylvania, USA" = "United States", "Perth, Western Australia" = "Australia", "Sydney, Australia" = "Australia",
                                                  "Wales, United Kingdom" = "UK", "Arizona, USA" = "United States", "East, England" = "UK"
                                                  , "San Antonio, TX" = "United States", "Toronto" = "Canada", "Glasgow, Scotland" = "UK",
                                                  "Washington D.C." = "United States", "Moscow" = "Russia", "North Carolina, USA" = "United States",
                                                  "Inglewood,Ca" = "United States", "Melbourne" = "Australia", "Melbourne, Australia" = "Australia",
                                                  "Melbourne. Australia" = "Australia", "Nova Scotia" = "Canada", "Melbourne " = "Australia", 
                                                  "Sverige" = "Sweden", "Sydney, New South Wales" = "Australia", "Garden City, KS" = "United States"))



#This renames the locations from their cities or regions and renames them to the country to get a clearer understanding of where they are all
#coming from



re_xboxX %>%
  filter(!location_rec %in% c("Worldwide", "A PLANET N OUTER SPACE <U+0001F30D><U+0001F30D>", "Born in Night City ",
                              "patreon.com/germanstrands", "Mistake Island", "Ragnarok", "Nirvana, Outer Space", "twitch.tv/xbmnetwork", "Earth", "/usr/optimus_code",
                              "Everywhere & Nowhere", "Interwebs", "XBL", "Bad Vibes Forever")) %>%
  #This line filters out any joke locations or locations with strange values
  count(location_rec, sort = TRUE) %>%  #Order from highest to lowest
  mutate(location_rec = reorder(location_rec,n)) %>%
  na.omit() %>% # remove NAs
  head(10) %>% #Shows 10 results
  ggplot(aes(x = location_rec,y = n)) +
  geom_col(fill = "Green") +
  coord_flip() +
  labs(x = "Top Locations",
       y = "Number of Tweets",
       title = "Locations of tweets mentioning the Xbox Series X") + 
  theme(axis.text = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, color = "black"),
        title = element_text(size = 18))

##___________________________________________________________________________________##
##Xbox series S

#xboxS <- read.csv("C://Users//ELISW//Desktop//math513playstationxbox-main//xboxseriess_tweets_english.csv")


#Reads the CVS into a dataframe


#xboxS$location[xboxS$location==""] <- NA

#Transforms the empty values into NA's   

#re_xboxS <- xboxS %>% mutate(location_rec =
#                               recode(location, "United Kingdom" = "UK","London, England" = "UK",
#                                      "Colorado, USA" = "United States", "New Jersey USA" = "United States",
#                                      "San Francisco, CA" = "United States", "England, United Kingdom" = "UK",
#                                      "Los Angeles, CA" = "United States", "California, USA" = "United States",
#                                      "USA" = "United States", "Canada <U+0001F1E8><U+0001F1E6>" = "Canada",
#                                      "Toronto, Ontario" = "Canada", "New York, USA" = "United States",
#                                      "New York, NY" = "United States", "Bronx, NY" = "United States",
#                                      "Texas, USA" = "United States" , "Big Apple" = "United States", "Toledo/Orlando" = "United States",
#                                      "Calgary, Alberta" = "Canada", "Florida, USA" = "United States", "Ontario Canada" = "Canada",
#                                       "Melbourne, Victoria" = "Australia", "Chicago, IL" = "United States", "London" = "UK", "Lafayette, LA"
#                                       = "United States", "Sheffield, England" = "UK", "Scotland" = "UK", "North Pole, AK" = "United States",
#                                       "London" = "UK", "Chicago, IL"= "United States", "Córdoba/Spain" = "Spain",
#                                       "Florida, USA" = "United States", "London, UK" = "UK", "Sweden/Stockholm" = "Sweden",
#                                       "Caerphilly, South Wales, UK" = "UK", "Dallas, TX" = "United States", "Virginia, USA"
#                                       = "United States", "Deutschland" = "Germany", "NJ" = "United States", "Seattle, WA" = 
#                                         "United States", "Bari, Puglia" = "Italy", "New York" = "United States", "Ohio, USA"=
#                                         "United States", "England" = "UK", "Atlanta, GA" = "United States", "Washington, DC"
#                                       = "United States", "New Jersey, USA" = "United States", "Manchester, England" = "UK",
#                                       "Scotland, United Kingdom" = "UK", "Nordrhein-West., DE, Iserlohn" = "Germany", "British Columbia, Canada"
#                                       = "Canada", "Brooklyn, NY" = "United States", "Nashville, TN" = "United States",
#                                       "Pennsylvania, USA" = "United States", "Perth, Western Australia" = "Australia", "Sydney, Australia" = "Australia",
#                                       "Wales, United Kingdom" = "UK", "Arizona, USA" = "United States", "East, England" = "UK"
#                                       , "San Antonio, TX" = "United States", "Toronto" = "Canada", "Glasgow, Scotland" = "UK",
#                                       "Washington D.C." = "United States", "Moscow" = "Russia", "North Carolina, USA" = "United States",
#                                       "Inglewood,Ca" = "United States", "Melbourne" = "Australia", "Melbourne, Australia" = "Australia",
#                                       "Melbourne. Australia" = "Australia", "Nova Scotia" = "Canada", "Melbourne " = "Australia", 
#                                       "Sverige" = "Sweden", "Sydney, New South Wales" = "Australia", "Garden City, KS" = "United States"))
# 
# #This renames the locations from their cities or regions and renames them to the country to get a clearer understanding of where they are all
# #coming from
# 
# 
# re_xboxS %>%
#   filter(!location_rec %in% c("Worldwide", "A PLANET N OUTER SPACE <U+0001F30D><U+0001F30D>", "Born in Night City ",
#                               "patreon.com/germanstrands", "Mistake Island", "Ragnarok", "Nirvana, Outer Space", "twitch.tv/xbmnetwork", "Earth", "/usr/optimus_code",
#                               "Everywhere & Nowhere", "Interwebs", "XBL", "Bad Vibes Forever")) %>%
#   #This line filters out any joke locations or locations with strange values
#   count(location_rec, sort = TRUE) %>% #Order from highest to lowest
#   mutate(location_rec = reorder(location_rec,n)) %>%
#   na.omit() %>% # remove NAs
#   head(10) %>% # Show 10 results
#   ggplot(aes(x = location_rec,y = n)) +
#   geom_col(fill = "Dark Green") +
#   coord_flip() +
#   labs(x = "Top Locations",
#        y = "Number of Tweets",
#        title = "Locations of tweets mentioning the Xbox Series S") + 
#   theme(axis.text = element_text(size = 16, color = "black"), 
#         axis.title = element_text(size = 16, color = "black"),
#         title = element_text(size = 18))
# 
