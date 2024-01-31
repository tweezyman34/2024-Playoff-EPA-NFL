#Install and Load Packages
install.packages("tidyverse")
install.packages("nflfastR")
install.packages("ggimage")
install.packages("nflplotR")
library(tidyverse)
library(nflfastR)
library(ggimage)
library(nflplotR)

#Pull Data
data <- load_pbp(2023)

#Name Playoff Teams
playoff_team <- c('KC', 'SF')

#EDIT
eliminated <- c('CLE', 'LA', 'DAL', 'MIA', 'PHI', 'PIT', 'HOU', 'BUF', 'GB',
                'TB', 'BAL', 'DET')
missed <- c('ATL', 'ARI', 'CAR', 'CHI', 'MIN', 'NO', 'NYG', 'SEA', 'WAS',
            'CIN', 'DEN', 'IND', 'JAX', 'LV', 'LAC', 'NE', 'NYJ', 'TEN')

#Drop NA's and get Offensive EPA
offdata <- data %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(posteam) %>%
  summarise(total_epaoff = sum(epa),
            offplays = n(),
            epa_play_off = total_epaoff/offplays)

#Drop NA's and get Defensive EPA
defdata <- data %>%
  drop_na(epa) %>%
  drop_na(posteam) %>%
  group_by(defteam) %>%
  summarise(total_epadef = sum(epa),
            defplays = n(),
            epa_play_def = total_epadef/defplays)

#Rename Columns
colnames(offdata)[1] = "team"
colnames(defdata)[1] = "team"

#Merge Offensive and Defensive Data
mergedata <- left_join(offdata, defdata, by = "team")

#Merge Complete Data with Logos
finaldata <- left_join(mergedata, nfllogos, by = "team")

#Add alpha and color
finaldata <- finaldata %>%
  mutate(
    colour = ifelse(finaldata$team %in% playoff_team, NA, 'b/w'),
    alpha = ifelse(finaldata$team %in% playoff_team, .9, .1)
  )
#EDITS
finaldata <- finaldata %>%
  mutate(colour = case_when(finaldata$team %in% playoff_team ~ NA,
                            finaldata$team %in% eliminated ~ NA,
                            finaldata$team %in% missed ~ 'b/w'),
         alpha = case_when(finaldata$team %in% playoff_team ~ 1,
                           finaldata$team %in% eliminated ~ .3,
                           finaldata$team %in% missed ~ .1))

#Calculate Mean EPA for Graph Line
meanepa<-mean(finaldata$epa_play_off)

#Create Plot- Adjust geom_text Items Accordingly
plot <- ggplot(finaldata, aes(epa_play_off, epa_play_def)) +
  geom_nfl_logos(aes(team_abbr = team, alpha = alpha, colour = colour), width = .06) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "EPA Earned/Allowed per Play",
       subtitle = "Created by Dylan Wilkerson/@wilkersonadylan",
       x = "Total EPA Gained on Offense",
       y = "Total EPA Allowed on Defense",
       caption = "Teams faded out are eliminated from playoff contention") +
  geom_text(aes(x=-.15, y=.05, label="Bad Offense, Bad Defense"),
            size = 6) +
  geom_text(aes(x=-.15, y=-.085, label="Bad Offense, Good Defense"),
            size = 6) +
  geom_text(aes(x= .05, y=-.09, label="Good Offense, Good Defense"),
            size = 6) +
  geom_text(aes(x= .05, y=.065, label="Good Offense, Bad Defense"),
            size = 6) +
  theme_light() +
  geom_vline(xintercept = meanepa) +
  geom_hline(yintercept = meanepa) +
  theme(plot.title = element_text(hjust = .5, face = "bold"),
        plot.subtitle = element_text(hjust = .5)) +
  scale_y_reverse() +
  scale_alpha_identity() +
  scale_color_identity()


#Print Plot
plot
