#This counts the amout of tweets the offical playstation and xbox twitter ccounts have made and makes a graph comparing them
X_P <- get_timeline(
  c("Xbox","PlayStation"),
  n = 18000)

X_P %>% filter(created_at >= as.Date('2020-10-01') & created_at <= as.Date('2020-11-22')) %>%
  group_by(screen_name,is_retweet) %>%
  ts_plot("days", lwd = 1) +
  labs( x = "Time", 
        y = "Number of tweets",
        title = "Frequency of Twitter statuses posted by Xbox and Playstation",
        subtitle = "Tweet counts aggregated every day",
        colour = "Console") +
  scale_colour_manual(values = c("Xbox" = "blue", "PlayStation" = "red")) +
  theme_minimal() +
  theme(plot.title = element_text(size = 16),
        plot.caption = element_text(size = 14),
        axis.text.x = element_text(size = 14), 
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "bottom") 
