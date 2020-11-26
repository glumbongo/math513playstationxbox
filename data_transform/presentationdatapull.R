#DONTNEED
library(tidyverse)
library(rtweet)
library(readr)

setwd("C://users/glumb/documents/university/math513/coursework")

token <- create_token(app = "math513study",
                      consumer_key = "wmG4paDWEtwIqH2NDfzIsekBM",
                      consumer_secret = "pyFyfg4WBTbzMRBH5qSdriM3l51CkFYEoc9UOZzAQyEP531sz8",
                      access_token = "1316056167353909255-LHy2H7emnj0C8JQVH65bauWHWyD6Mf",
                      access_secret = "MfDRf2CbAK4cu5fRunDR8UGdw1VJTJrvYoQkGGa6K2in0")

token


playstation <- search_tweets("#playstation5",
                             n = 20000,
                             include_rts = FALSE,
                             retryonratelimit = TRUE,
                             verbose = TRUE)

playstation_eng <- search_tweets("#playstation5",
                                 n = 20000,
                                 include_rts = FALSE,
                                 retryonratelimit = TRUE,
                                 verbose = TRUE,
                                 lang = "en")


xbox_eng <- search_tweets("#xboxseriesx",
                          n = 20000,
                          include_rts = FALSE,
                          retryonratelimit = TRUE,
                          verbose = TRUE,
                          lang = "en")


xboxseriess_eng <- search_tweets("#xboxseriess",
                                 n = 20000,
                                 include_rts = FALSE,
                                 retryonratelimit = TRUE,
                                 verbose = TRUE,
                                 lang = "en")

xbox <- search_tweets("#xboxseriesx",
                      n = 20000,
                      include_rts = FALSE,
                      retryonratelimit = TRUE,
                      verbose = TRUE)


write_as_csv(playstation_eng, "playstation_tweets_english.csv")
write_as_csv(playstation, "playstation_tweets_alllang.csv")
write_as_csv(xbox_eng, "xboxseriesx_tweets_english.csv")
write_as_csv(xboxseriess_eng, "xboxseriess_tweets_english.csv")


write_as_csv(xbox, "xboxseriesx_tweets_alllang.csv")


