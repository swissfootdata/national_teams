library(rvest)
library(stringr)
library(XML)
library(readr)
library(dplyr)
library(maps)
library(countrycode)
library(httr)

world_cup_data <- readRDS("./Data/world_cup_data_complete.rds")

#Get IDs
url <- "https://www.transfermarkt.ch/weltmeisterschaft-2018/gesamtspielplan/pokalwettbewerb/WM22/saison_id/2021"
webpage <- read_html(url)

ids_id <- webpage %>%
  html_nodes(xpath = "//td/a") %>%
  html_attr("href")

ids <- data.frame(ids_id)
ids$check <- grepl("spielbericht",ids$ids_id)

ids <- na.omit(ids[ids$check == TRUE,])

new_matches <- as.integer(gsub(".*spielbericht/", "",ids$ids_id))

print(paste0(length(new_matches)," matches found"))

#Get match data
data_wc2022 <- data.frame(9999999,"team_home","team_away","tournament","stage","01.01.1900","99:99")
colnames(data_wc2022) <- c("ID","team_home","team_away","tournament","stage",
                                      "date","time")

for (i in new_matches) {

url <- paste0("https://www.transfermarkt.us/frankreich_kroatien/statistik/spielbericht/",i)
webpage <- read_html(url)
ID <- i

team_home <- html_text(html_nodes(webpage,".sb-vereinslink"))[1]
team_away <- html_text(html_nodes(webpage,".sb-vereinslink"))[2] #3

tournament <- trimws(gsub("\n","",gsub("\t","",html_text(html_nodes(webpage,"h2"))[1])))
stage <- trimws(gsub("\n","",gsub("[|].*","",html_text(html_nodes(webpage,".sb-datum"))[1])))

datum <- gsub( ".*(\\d{2}.\\d{1,2}.\\d{2,4}).*", "\\1", html_text(html_nodes(webpage,".sb-datum"))[1])
zeit <- gsub( ".*(\\d{1,2}:\\d{1,2}).*", "\\1", html_text(html_nodes(webpage,".sb-datum"))[1]) 

new_data <- data.frame(ID,team_home,team_away,tournament,stage,datum,zeit)
colnames(new_data) <- c("ID","team_home","team_away","tournament","stage",
                           "date","time")
data_wc2022 <- rbind(data_wc2022,new_data)
}
data_wc2022 <- data_wc2022[-1,]
data_wc2022$date <- paste0("bla",data_wc2022$date)
data_wc2022$date <- gsub("bla0[/]","10/",data_wc2022$date)
data_wc2022$date <- gsub("bla","",data_wc2022$date)
data_wc2022$date <- as.Date(data_wc2022$date,format="%m/%d/%y")

#Add 2 and 3 Letter code
data_wc2022$three_letter_code_home <- countrycode(data_wc2022$team_home,origin='country.name',destination = 'iso3c',
                                                     custom_match = c('England'='ENG','Kosovo' = "XXK",
                                                                      'Netherlands Antilles'='ANT', 
                                                                      'Northern Ireland'='NIR', 
                                                                      'Scotland'='SCO', 
                                                                      'Serbia and Montenegro'='SRB', 
                                                                      'Türkiye'='TUR', 
                                                                      'Wales'='WLS', 
                                                                      'Yugoslavia'='YUG'))
data_wc2022$three_letter_code_away <- countrycode(data_wc2022$team_away,origin='country.name',destination = 'iso3c',
                                                     custom_match = c('England'='ENG','Kosovo' = "XXK",
                                                                      'Netherlands Antilles'='ANT', 
                                                                      'Northern Ireland'='NIR', 
                                                                      'Scotland'='SCO', 
                                                                      'Serbia and Montenegro'='SRB', 
                                                                      'Türkiye'='TUR', 
                                                                      'Wales'='WLS', 
                                                                      'Yugoslavia'='YUG'))
data_wc2022$two_letter_code_home <- countrycode(data_wc2022$team_home,origin='country.name',destination = 'iso2c',
                                                   custom_match = c('England'='EN','Kosovo' = "XK",
                                                                    'Netherlands Antilles'='AN', 
                                                                    'Northern Ireland'='EI', 
                                                                    'Scotland'='SQ', 
                                                                    'Serbia and Montenegro'='CS', 
                                                                    'Türkiye'='TR', 
                                                                    'Wales'='WA', 
                                                                    'Yugoslavia'='YU'))
data_wc2022$two_letter_code_away <- countrycode(data_wc2022$team_away,origin='country.name',destination = 'iso2c',
                                                   custom_match = c('England'='EN','Kosovo' = "XK",
                                                                    'Netherlands Antilles'='AN', 
                                                                    'Northern Ireland'='EI', 
                                                                    'Scotland'='SQ', 
                                                                    'Serbia and Montenegro'='CS', 
                                                                    'Türkiye'='TR', 
                                                                    'Wales'='WA', 
                                                                    'Yugoslavia'='YU'))

#Load Ranking Data
fifa_ranking <- read_csv("data/fifa_ranking-2022-08-25_new.csv")

#Get ELO Data
url <- "https://www.eloratings.net/2022.tsv"

all_content <- GET(url)
elo_ratings <- content(all_content, col_names = FALSE)


elo_ratings <- elo_ratings[,c(3,2,4)]
colnames(elo_ratings) <- c("two_letter_code","rank","score")
elo_ratings$date <- as.Date("2022-10-15")

data_wc2022$confederation_home <- NA
data_wc2022$confederation_away <- NA
data_wc2022$fifa_ranking_score_home <- NA
data_wc2022$fifa_ranking_score_away <- NA
data_wc2022$fifa_ranking_rank_home <- NA
data_wc2022$fifa_ranking_rank_away <- NA
data_wc2022$elo_ranking_score_home <- NA
data_wc2022$elo_ranking_score_away <- NA
data_wc2022$elo_ranking_rank_home <- NA
data_wc2022$elo_ranking_rank_away <- NA


for (w in 1:nrow(data_wc2022)) {
  
  #Home Team
  selection_fifa <- fifa_ranking %>%
    filter(country_full == data_wc2022$team_home[w],
           rank_date < data_wc2022$date[w])
  
  
  if (nrow(selection_fifa) > 0) {
    data_wc2022$confederation_home[w] <- selection_fifa$confederation[nrow(selection_fifa)]
    data_wc2022$fifa_ranking_score_home[w] <- selection_fifa$total_points[nrow(selection_fifa)]
    data_wc2022$fifa_ranking_rank_home[w] <- selection_fifa$rank[nrow(selection_fifa)]
  }
  
  selection_elo <- elo_ratings %>%
    filter(two_letter_code == data_wc2022$two_letter_code_home[w],
           date < data_wc2022$date[w])
  
  if (nrow(selection_elo) > 0) {
    data_wc2022$elo_ranking_score_home[w] <- selection_elo$score[nrow(selection_elo)]
    data_wc2022$elo_ranking_rank_home[w] <- selection_elo$rank[nrow(selection_elo)]
  }
  
  #Away Team
  selection_fifa <- fifa_ranking %>%
    filter(country_full == data_wc2022$team_away[w],
           rank_date < data_wc2022$date[w])
  
  if (nrow(selection_fifa) > 0) {
    data_wc2022$confederation_away[w] <- selection_fifa$confederation[nrow(selection_fifa)]
    data_wc2022$fifa_ranking_score_away[w] <- selection_fifa$total_points[nrow(selection_fifa)]
    data_wc2022$fifa_ranking_rank_away[w] <- selection_fifa$rank[nrow(selection_fifa)]
  }
  
  selection_elo <- elo_ratings %>%
    filter(two_letter_code == data_wc2022$two_letter_code_away[w],
           date < data_wc2022$date[w])
  
  if (nrow(selection_elo) > 0) {
    data_wc2022$elo_ranking_score_away[w] <- selection_elo$score[nrow(selection_elo)]
    data_wc2022$elo_ranking_rank_away[w] <- selection_elo$rank[nrow(selection_elo)]
  }
  
  
}    



###Add Form Data###
data_wc2022$performance_home_last_five_matches <- NA
data_wc2022$performance_away_last_five_matches <- NA

for (w in 1:nrow(data_wc2022)) {
 
  #Form Home Team
  selection_form <- world_cup_data %>%
    filter(team_home == data_wc2022$team_home[w] |
             team_away == data_wc2022$team_home[w],
           date < data_wc2022$date[w]) %>%
    arrange(desc(date))
  
  if (nrow(selection_form) > 4) {
    points_scored <- 0
    
    for (s in 1:5) {
      if (selection_form$team_home[s] == data_wc2022$team_home[w]) {
        points_scored <- points_scored + selection_form$points_home[s] 
      } else {
        points_scored <- points_scored + selection_form$points_away[s]
      }  
    }  
    data_wc2022$performance_home_last_five_matches[w] <- points_scored/5
  }  
  
  #Form Away Team
  selection_form <- world_cup_data %>%
    filter(team_home == data_wc2022$team_away[w] |
             team_away == data_wc2022$team_away[w],
           date < data_wc2022$date[w]) %>%
    arrange(desc(date))
  
  if (nrow(selection_form) > 4) {
    points_scored <- 0
    
    for (s in 1:5) {
      if (selection_form$team_home[s] == data_wc2022$team_away[w]) {
        points_scored <- points_scored + selection_form$points_home[s] 
      } else {
        points_scored <- points_scored + selection_form$points_away[s]
      }  
    }  
    data_wc2022$performance_away_last_five_matches[w] <- points_scored/5
  }  
  
}  


