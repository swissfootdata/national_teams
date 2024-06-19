library(rvest)
library(stringr)
library(XML)
library(readr)
library(dplyr)
library(maps)
library(countrycode)
library(httr)
library(gsubfn)


euro_data <- readRDS("./Data/euro_data_complete.rds")
euro_data$team_home <- gsub("Turkey","Türkiye",euro_data$team_home)
euro_data$team_away <- gsub("Turkey","Türkiye",euro_data$team_away)

#Get IDs
url <- "https://www.transfermarkt.com/europameisterschaft-2024/gesamtspielplan/pokalwettbewerb/EM24/saison_id/2023"
webpage <- read_html(url)

ids_id <- webpage %>%
  html_nodes(xpath = "//td/a") %>%
  html_attr("href")

ids <- data.frame(ids_id)
ids$check <- grepl("spielbericht",ids$ids_id)

ids <- na.omit(ids[ids$check == TRUE,])

#Select Group Matches (optional)
ids <- ids[1:36,]

new_matches <- as.integer(gsub(".*spielbericht/", "",ids$ids_id))

print(paste0(length(new_matches)," matches found"))

#Get match data
data_ec2024 <- data.frame(9999999,"team_home","team_away","tournament","stage","01.01.1900","99:99")
colnames(data_ec2024) <- c("ID","team_home","team_away","tournament","stage",
                                      "date","time")

for (i in new_matches) {

url <- paste0("https://www.transfermarkt.us/frankreich_kroatien/statistik/spielbericht/",i)
webpage <- read_html(url)
ID <- i
url
team_home <- html_text(html_nodes(webpage,".sb-vereinslink"))[1]
team_away <- html_text(html_nodes(webpage,".sb-vereinslink"))[2] #3

tournament <- trimws(gsub("\n","",gsub("\t","",html_text(html_nodes(webpage,"h2"))[1])))
stage <- trimws(gsub("\n","",gsub("[|].*","",html_text(html_nodes(webpage,".sb-datum"))[1])))

datum <- gsub( ".*(\\d{2}.\\d{1,2}.\\d{2,4}).*", "\\1", html_text(html_nodes(webpage,".sb-datum"))[1])
zeit <- gsub( ".*(\\d{1,2}:\\d{1,2}).*", "\\1", html_text(html_nodes(webpage,".sb-datum"))[1]) 

new_data <- data.frame(ID,team_home,team_away,tournament,stage,datum,zeit)
colnames(new_data) <- c("ID","team_home","team_away","tournament","stage",
                           "date","time")
data_ec2024 <- rbind(data_ec2024,new_data)
}
data_ec2024 <- data_ec2024[-1,]
data_ec2024$date <- as.Date(strapplyc(data_ec2024$date, "\\d+/\\d+/\\d+", simplify = TRUE),format="%m/%d/%y")

#Add 2 and 3 Letter code
data_ec2024$three_letter_code_home <- countrycode(data_ec2024$team_home,origin='country.name',destination = 'iso3c',
                                                     custom_match = c('England'='ENG','Kosovo' = "XXK",
                                                                      'Netherlands Antilles'='ANT', 
                                                                      'Northern Ireland'='NIR', 
                                                                      'Scotland'='SCO', 
                                                                      'Serbia and Montenegro'='SRB', 
                                                                      'Türkiye'='TUR', 
                                                                      'Wales'='WLS', 
                                                                      'Yugoslavia'='YUG'))
data_ec2024$three_letter_code_away <- countrycode(data_ec2024$team_away,origin='country.name',destination = 'iso3c',
                                                     custom_match = c('England'='ENG','Kosovo' = "XXK",
                                                                      'Netherlands Antilles'='ANT', 
                                                                      'Northern Ireland'='NIR', 
                                                                      'Scotland'='SCO', 
                                                                      'Serbia and Montenegro'='SRB', 
                                                                      'Türkiye'='TUR', 
                                                                      'Wales'='WLS', 
                                                                      'Yugoslavia'='YUG'))
data_ec2024$two_letter_code_home <- countrycode(data_ec2024$team_home,origin='country.name',destination = 'iso2c',
                                                   custom_match = c('England'='EN','Kosovo' = "XK",
                                                                    'Netherlands Antilles'='AN', 
                                                                    'Northern Ireland'='EI', 
                                                                    'Scotland'='SQ', 
                                                                    'Serbia and Montenegro'='CS', 
                                                                    'Türkiye'='TR', 
                                                                    'Wales'='WA', 
                                                                    'Yugoslavia'='YU'))
data_ec2024$two_letter_code_away <- countrycode(data_ec2024$team_away,origin='country.name',destination = 'iso2c',
                                                   custom_match = c('England'='EN','Kosovo' = "XK",
                                                                    'Netherlands Antilles'='AN', 
                                                                    'Northern Ireland'='EI', 
                                                                    'Scotland'='SQ', 
                                                                    'Serbia and Montenegro'='CS', 
                                                                    'Türkiye'='TR', 
                                                                    'Wales'='WA', 
                                                                    'Yugoslavia'='YU'))

#Load Ranking Data
fifa_ranking <- read_csv("data/fifa_ranking-2023-07-20_new.csv")


#Get ELO Data
url <- "https://www.eloratings.net/2024.tsv"

all_content <- GET(url)
elo_ratings <- content(all_content, col_names = FALSE)

elo_ratings <- elo_ratings[,c(3,2,4)]
colnames(elo_ratings) <- c("two_letter_code","rank","score")


elo_ratings$date <- as.Date("2024-06-01")

data_ec2024$confederation_home <- NA
data_ec2024$confederation_away <- NA
data_ec2024$fifa_ranking_score_home <- NA
data_ec2024$fifa_ranking_score_away <- NA
data_ec2024$fifa_ranking_rank_home <- NA
data_ec2024$fifa_ranking_rank_away <- NA
data_ec2024$elo_ranking_score_home <- NA
data_ec2024$elo_ranking_score_away <- NA
data_ec2024$elo_ranking_rank_home <- NA
data_ec2024$elo_ranking_rank_away <- NA


for (w in 1:nrow(data_ec2024)) {
  
  #Home Team
  selection_fifa <- fifa_ranking %>%
    filter(country_full == data_ec2024$team_home[w],
           rank_date < data_ec2024$date[w])
  
  
  if (nrow(selection_fifa) > 0) {
    data_ec2024$confederation_home[w] <- selection_fifa$confederation[nrow(selection_fifa)]
    data_ec2024$fifa_ranking_score_home[w] <- selection_fifa$total_points[nrow(selection_fifa)]
    data_ec2024$fifa_ranking_rank_home[w] <- selection_fifa$rank[nrow(selection_fifa)]
  }
  
  selection_elo <- elo_ratings %>%
    filter(two_letter_code == data_ec2024$two_letter_code_home[w],
           date < data_ec2024$date[w])
  
  if (nrow(selection_elo) > 0) {
    data_ec2024$elo_ranking_score_home[w] <- selection_elo$score[nrow(selection_elo)]
    data_ec2024$elo_ranking_rank_home[w] <- selection_elo$rank[nrow(selection_elo)]
  }

  #Away Team
  selection_fifa <- fifa_ranking %>%
    filter(country_full == data_ec2024$team_away[w],
           rank_date < data_ec2024$date[w])
  
  if (nrow(selection_fifa) > 0) {
    data_ec2024$confederation_away[w] <- selection_fifa$confederation[nrow(selection_fifa)]
    data_ec2024$fifa_ranking_score_away[w] <- selection_fifa$total_points[nrow(selection_fifa)]
    data_ec2024$fifa_ranking_rank_away[w] <- selection_fifa$rank[nrow(selection_fifa)]
  }
  
  selection_elo <- elo_ratings %>%
    filter(two_letter_code == data_ec2024$two_letter_code_away[w],
           date < data_ec2024$date[w])
  
  if (nrow(selection_elo) > 0) {
    data_ec2024$elo_ranking_score_away[w] <- selection_elo$score[nrow(selection_elo)]
    data_ec2024$elo_ranking_rank_away[w] <- selection_elo$rank[nrow(selection_elo)]
  }
  
  
}    



###Add Form Data###
data_ec2024$performance_home_last_five_matches <- NA
data_ec2024$performance_away_last_five_matches <- NA

for (w in 1:nrow(data_ec2024)) {
 
  #Form Home Team
  selection_form <- euro_data %>%
    filter(team_home == data_ec2024$team_home[w] |
             team_away == data_ec2024$team_home[w],
           date < data_ec2024$date[w]) %>%
    arrange(desc(date))
  
  if (nrow(selection_form) > 4) {
    points_scored <- 0
    
    for (s in 1:5) {
      if (selection_form$team_home[s] == data_ec2024$team_home[w]) {
        points_scored <- points_scored + selection_form$points_home[s] 
      } else {
        points_scored <- points_scored + selection_form$points_away[s]
      }  
    }  
    data_ec2024$performance_home_last_five_matches[w] <- points_scored/5
  }  
  
  #Form Away Team
  selection_form <- euro_data %>%
    filter(team_home == data_ec2024$team_away[w] |
             team_away == data_ec2024$team_away[w],
           date < data_ec2024$date[w]) %>%
    arrange(desc(date))
  
  if (nrow(selection_form) > 4) {
    points_scored <- 0
    
    for (s in 1:5) {
      if (selection_form$team_home[s] == data_ec2024$team_away[w]) {
        points_scored <- points_scored + selection_form$points_home[s] 
      } else {
        points_scored <- points_scored + selection_form$points_away[s]
      }  
    }  
    data_ec2024$performance_away_last_five_matches[w] <- points_scored/5
  }  
  
}  

saveRDS(data_ec2024,file="./Data/euro_matches.rds")

