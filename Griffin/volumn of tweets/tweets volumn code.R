ps <- as.Date(as.POSIXct(playstation_english$created_at,format='%Y%M%D'))
xb <- as.Date(as.POSIXct(xbox_x_english$created_at,format='%Y%M%D'))

playstation_english <- cbind(playstation_english,ps)
xbox_x_english <- cbind(xbox_x_english,xb)

ctx <- playstation_english %>% group_by(ps) %>% count()

ctp <- xbox_x_english %>% group_by(xb) %>% count()

ggplot() +
  geom_line(data = ctx, aes(x = ps, y = n))+
  geom_line(data = ctp, aes(x = xb, y = n))


ggplot() + 
  geom_area(data = ctp,aes(x = xb, y = n, color = "#E7B800", fill = "#E7B800"), 
            alpha = 0.5, position = position_dodge(0.8)) +
  geom_area(data = ctx,aes(x = ps, y = n,color = "#00AFBB", fill = "#00AFBB"), 
            alpha = 0.5, position = position_dodge(0.8)) +
  labs(x = NULL, y = NULL,title = "Frequency of Tweets From #Xbox and #Playstation",
       subtitle = "Twitter status (tweet) counts aggregated using 1-days intervals",
       caption = "Source: Data collected from Twitter's REST API via rtweet")+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"),
                    name  ="Console",
                    labels=c("Playstation", "Xbox")) +
  guides(color=FALSE) 
