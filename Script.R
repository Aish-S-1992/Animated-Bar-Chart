rm(list =ls())

cat("\014")

library(dplyr)

library(scales)

library(gganimate)

library(colorspace)

library(ggplot2)

library(gganimate)

library(gifski)

df <- read.csv("C:/Users/aishwarya.sharma/OneDrive - insidemedia.net/Dell Lattitude - 5300/Aishwarya/Kaggle/International FIFA Results/results.csv",stringsAsFactors = F)

df$date <-  format(as.Date(df$date,, format = "%Y-%m-%d"),"%Y")

home.result <- df %>%
  group_by(home_team,date)%>%
  summarise(Matches_Played = n())
colnames(home.result)[1] <- "Team"

away.result <- df %>%    
  group_by(away_team,date)%>%
  summarise(Matches_Played = n())
colnames(away.result)[1] <- "Team"

total <- rbind.data.frame(home.result,away.result)

total <- total %>%
  group_by(Team,date)%>%
  summarise(Total_Matches = sum(Matches_Played))

colnames(total) <- c("Team","Year","Total_Played")

temp <- total %>%
  group_by(Year) %>%
  mutate(rank = rank(-Total_Played, ties.method = "random"),
         Total_Played_rel = Total_Played/Total_Played[rank==1],
         Total_Played_lbl = paste0(" ",round(Total_Played))) %>%
  group_by(Team) %>% 
  filter(rank <=10) %>%
  ungroup()

staticplot = ggplot(temp, aes(rank, group = Team, 
                              fill = as.factor(Team), color = as.factor(Team))) +
  geom_tile(aes(y = Total_Played/2,
                height = Total_Played,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Team, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y= Total_Played,label = Total_Played_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(Year, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Football Matches Through the Years : {closest_state}',  
       subtitle  =  "Top 10 Countries",
       caption  = "Top 10 Countries with Most Played Football Matches")



animate(anim, 1000, fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"))

