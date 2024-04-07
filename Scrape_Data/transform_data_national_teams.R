library(rvest)
library(stringr)
library(XML)
library(readr)
library(dplyr)
library(maps)
library(countrycode)

setwd("C:/Users/simon/OneDrive/Fussballdaten/national_teams")


#Load Data World Cup
world_cup_data <- readRDS(file="./Data/euro_data_clean.rds")

#Add 2 and 3 Letter code
world_cup_data$three_letter_code_home <- countrycode(world_cup_data$team_home,origin='country.name',destination = 'iso3c',
                                                     custom_match = c('England'='ENG','Kosovo' = "XXK",
                                                                      'Netherlands Antilles'='ANT', 
                                                                      'Northern Ireland'='NIR', 
                                                                      'Scotland'='SCO', 
                                                                      'Serbia and Montenegro'='SRB', 
                                                                      'Türkiye'='TUR', 
                                                                      'Wales'='WLS', 
                                                                      'Yugoslavia'='YUG'))
world_cup_data$three_letter_code_away <- countrycode(world_cup_data$team_away,origin='country.name',destination = 'iso3c',
                                                     custom_match = c('England'='ENG','Kosovo' = "XXK",
                                                                      'Netherlands Antilles'='ANT', 
                                                                      'Northern Ireland'='NIR', 
                                                                      'Scotland'='SCO', 
                                                                      'Serbia and Montenegro'='SRB', 
                                                                      'Türkiye'='TUR', 
                                                                      'Wales'='WLS', 
                                                                      'Yugoslavia'='YUG'))
world_cup_data$two_letter_code_home <- countrycode(world_cup_data$team_home,origin='country.name',destination = 'iso2c',
                                                   custom_match = c('England'='EN','Kosovo' = "XK",
                                                                    'Netherlands Antilles'='AN', 
                                                                    'Northern Ireland'='EI', 
                                                                    'Scotland'='SQ', 
                                                                    'Serbia and Montenegro'='CS', 
                                                                    'Türkiye'='TR', 
                                                                    'Wales'='WA', 
                                                                    'Yugoslavia'='YU'))
world_cup_data$two_letter_code_away <- countrycode(world_cup_data$team_away,origin='country.name',destination = 'iso2c',
                                                   custom_match = c('England'='EN','Kosovo' = "XK",
                                                                    'Netherlands Antilles'='AN', 
                                                                    'Northern Ireland'='EI', 
                                                                    'Scotland'='SQ', 
                                                                    'Serbia and Montenegro'='CS', 
                                                                    'Türkiye'='TR', 
                                                                    'Wales'='WA', 
                                                                    'Yugoslavia'='YU'))
             
#Load Ranking Data
fifa_ranking <- read_csv("data/fifa_ranking-2023-07-20.csv")

#Get Elo Data
source("get_elo_values_history.R")


world_cup_data$confederation_home <- NA
world_cup_data$confederation_away <- NA
world_cup_data$fifa_ranking_score_home <- NA
world_cup_data$fifa_ranking_score_away <- NA
world_cup_data$fifa_ranking_rank_home <- NA
world_cup_data$fifa_ranking_rank_away <- NA
world_cup_data$elo_ranking_score_home <- NA
world_cup_data$elo_ranking_score_away <- NA
world_cup_data$elo_ranking_rank_home <- NA
world_cup_data$elo_ranking_rank_away <- NA


for (w in 1:nrow(world_cup_data)) {

#Home Team
selection_fifa <- fifa_ranking %>%
    filter(country_full == world_cup_data$team_home[w],
           rank_date < world_cup_data$date[w])


if (nrow(selection_fifa) > 0) {
world_cup_data$confederation_home[w] <- selection_fifa$confederation[nrow(selection_fifa)]
world_cup_data$fifa_ranking_score_home[w] <- selection_fifa$total_points[nrow(selection_fifa)]
world_cup_data$fifa_ranking_rank_home[w] <- selection_fifa$rank[nrow(selection_fifa)]
}

selection_elo <- elo_ratings %>%
  filter(two_letter_code == world_cup_data$two_letter_code_home[w],
         date < world_cup_data$date[w])

if (nrow(selection_elo) > 0) {
  world_cup_data$elo_ranking_score_home[w] <- selection_elo$score[nrow(selection_elo)]
  world_cup_data$elo_ranking_rank_home[w] <- selection_elo$rank[nrow(selection_elo)]
}

#Away Team
selection_fifa <- fifa_ranking %>%
  filter(country_full == world_cup_data$team_away[w],
         rank_date < world_cup_data$date[w])

if (nrow(selection_fifa) > 0) {
world_cup_data$confederation_away[w] <- selection_fifa$confederation[nrow(selection_fifa)]
world_cup_data$fifa_ranking_score_away[w] <- selection_fifa$total_points[nrow(selection_fifa)]
world_cup_data$fifa_ranking_rank_away[w] <- selection_fifa$rank[nrow(selection_fifa)]
}

selection_elo <- elo_ratings %>%
  filter(two_letter_code == world_cup_data$two_letter_code_away[w],
         date < world_cup_data$date[w])

if (nrow(selection_elo) > 0) {
  world_cup_data$elo_ranking_score_away[w] <- selection_elo$score[nrow(selection_elo)]
  world_cup_data$elo_ranking_rank_away[w] <- selection_elo$rank[nrow(selection_elo)]
}


}    


#Clean Up
world_cup_data <- world_cup_data %>%
  filter(is.na(fifa_ranking_score_home) == FALSE,
         is.na(fifa_ranking_score_away) == FALSE,
         is.na(fifa_ranking_rank_home) == FALSE,
         is.na(fifa_ranking_rank_away) == FALSE,
         is.na(elo_ranking_score_home) == FALSE,
         is.na(elo_ranking_score_away) == FALSE,
         is.na(elo_ranking_rank_home) == FALSE,
         is.na(elo_ranking_rank_away) == FALSE)





###Add Form Data###
world_cup_data$performance_home_last_five_matches <- NA
world_cup_data$performance_away_last_five_matches <- NA

for (w in 1:nrow(world_cup_data)) {

#Form Home Team
selection_form <- world_cup_data %>%
    filter(team_home == world_cup_data$team_home[w] |
           team_away == world_cup_data$team_home[w],
           date < world_cup_data$date[w]) %>%
  arrange(desc(date))

if (nrow(selection_form) > 4) {
points_scored <- 0

for (s in 1:5) {
if (selection_form$team_home[s] == world_cup_data$team_home[w]) {
points_scored <- points_scored + selection_form$points_home[s] 
} else {
points_scored <- points_scored + selection_form$points_away[s]
}  
}  
world_cup_data$performance_home_last_five_matches[w] <- points_scored/5
}  

#Form Away Team
selection_form <- world_cup_data %>%
  filter(team_home == world_cup_data$team_away[w] |
           team_away == world_cup_data$team_away[w],
         date < world_cup_data$date[w]) %>%
  arrange(desc(date))

if (nrow(selection_form) > 4) {
  points_scored <- 0
  
  for (s in 1:5) {
    if (selection_form$team_home[s] == world_cup_data$team_away[w]) {
      points_scored <- points_scored + selection_form$points_home[s] 
    } else {
      points_scored <- points_scored + selection_form$points_away[s]
    }  
  }  
  world_cup_data$performance_away_last_five_matches[w] <- points_scored/5
}  

}  

world_cup_data <- world_cup_data %>%
  filter(is.na(performance_home_last_five_matches) == FALSE,
         is.na(performance_away_last_five_matches) == FALSE) %>%
  unique()

saveRDS(world_cup_data,file="./Data/euro_data_complete.rds")

