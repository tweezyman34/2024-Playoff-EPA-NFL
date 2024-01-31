#Install and Load Pakcages
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("nflplotR")
library(tidyverse)
library(nflfastR)
library(ggimage)
library(nflplotR)

#List playoff teams for alpha
playoff_team <- c('KC', 'SF')

eliminated <- c('DAL', 'LA', 'MIA', 'CLE', 'PIT', 'PHI', 'HOU', 'BUF', 'TB',
                'GB', 'BAL', 'DET')

missed <- c('ATL', 'ARI', 'CAR', 'CHI', 'MIN', 'NO', 'NYG', 'SEA', 'WAS',
            'CIN', 'DEN', 'IND', 'JAX', 'LV', 'LAC', 'NE', 'NYJ', 'TEN')

#Load 2023 play-by-play
data <- load_pbp(2023)

#Filter for first half data
firsthalf <- data %>%
  filter(qtr < 3) 

#Drop NA's and get Offensive EPA
offdata <- firsthalf %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(posteam) %>%
  summarise(total_epaoff = sum(epa),
            offplays = n(),
            epa_play_off = total_epaoff/offplays)

#Drop NA's and get Defensive EPA
defdata <- firsthalf %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(defteam) %>%
  summarise(total_epadef = sum(epa),
            defplays = n(),
            epa_play_def = total_epadef/defplays)

#Rename columns
colnames(offdata)[1] = "team"
colnames(defdata)[1] = "team"

#Join offense and defense
final <- left_join(offdata, defdata, by = "team")

#Add alpha and color values for playoff teams
final <- final %>%
  mutate(
    colour = ifelse(final$team %in% playoff_team, NA, 'b/w'),
    alpha = ifelse(final$team %in% playoff_team, .9, .1)
  )

#Get mean EPA for the plot
meanepa<-mean(final$epa_play_off)

#add alpha and colour indicators
final <- final %>%
  mutate(colour = case_when(final$team %in% playoff_team ~ NA,
                            final$team %in% eliminated ~ NA,
                            final$team %in% missed ~ 'b/w'),
         alpha = case_when(final$team %in% playoff_team ~ 1,
                           final$team %in% eliminated ~ .5,
                           final$team %in% missed ~ .1))

#Create plot
firsthalfplot <- ggplot(final, aes(epa_play_off, epa_play_def)) +
  geom_nfl_logos(aes(team_abbr = team, alpha = alpha, colour = colour), width = .06) +
  geom_hline(yintercept = meanepa) +
  geom_vline(xintercept = meanepa) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "EPA Earned/Allowed in the First Half",
       subtitle = "Created by Dylan Wilkerson/@wilkersonadylan",
       x = "Total EPA Gained on Offense",
       y = "Total EPA Allowed on Defense") +
  theme_light() +
  geom_text(aes(x=-.15, y=.075, label="Bad Offense, Bad Defense"),
            size = 5) +
  geom_text(aes(x=-.15, y=-.075, label="Bad Offense, Good Defense"),
            size = 5) +
  geom_text(aes(x=.1, y=-.1, label="Good Offense, Good Defense"),
            size = 5) +
  geom_text(aes(x= .1, y=.075, label="Good Offense, Bad Defense"),
            size = 5) +
  xlim(-.225, .2) +
  ylim(-.15, .15) +
  scale_alpha_identity() +
  scale_color_identity() +
  scale_y_reverse() + 
  theme(plot.title = element_text(hjust = .5, face = "bold"),
                             plot.subtitle = element_text(hjust = .5))

#Print plot
firsthalfplot




