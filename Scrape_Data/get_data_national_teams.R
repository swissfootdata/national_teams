library(rvest)
library(stringr)
library(XML)
library(readr)
library(dplyr)

setwd("C:/Users/simon/OneDrive/Fussballdaten/national_teams")

#New data frame
data_transfermarkt_new <- data.frame(9999999,"team_home","team_away","tournament","stage","01.01.1900","99:99","result","result_halftime",99999,"Schiedsrichter",1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
colnames(data_transfermarkt_new) <- c("ID","team_home","team_away","tournament","stage",
                                      "date","time","result","result_halftime","crowd","referee",
                                      "shots_overall_home","shots_overall_away","shots_target_home","shots_target_away",
                                      "shots_missed_home","shots_missed_away","shots_hold_home","shots_hold_away",
                                      "corner_home","corner_away","freekicks_home","freekicks_away",
                                      "fouls_home","fouls_away","offside_home","offside_away")

links <- c(#"https://www.transfermarkt.ch/europameisterschaft-2020/gesamtspielplan/pokalwettbewerb/EMQ/saison_id/2022",
           #"https://www.transfermarkt.ch/europameisterschaft-2020/gesamtspielplan/pokalwettbewerb/EMQ/saison_id/2018",
           #"https://www.transfermarkt.ch/europameisterschaft-2020/gesamtspielplan/pokalwettbewerb/EMQ/saison_id/2014",
           #"https://www.transfermarkt.ch/europameisterschaft-2020/gesamtspielplan/pokalwettbewerb/EMQ/saison_id/2010",
           "https://www.transfermarkt.ch/europameisterschaft-2020/gesamtspielplan/pokalwettbewerb/EMQ/saison_id/2007",
           "https://www.transfermarkt.ch/europameisterschaft-2020/gesamtspielplan/pokalwettbewerb/EMQ/saison_id/2002",
           "https://www.transfermarkt.ch/europameisterschaft-2020/gesamtspielplan/pokalwettbewerb/EM20/saison_id/2020",
           "https://www.transfermarkt.ch/europameisterschaft-2016/gesamtspielplan/pokalwettbewerb/EM16/saison_id/2016",
           "https://www.transfermarkt.ch/europameisterschaft-2012/gesamtspielplan/pokalwettbewerb/EM12/saison_id/2012",
           "https://www.transfermarkt.ch/europameisterschaft-2008/gesamtspielplan/pokalwettbewerb/EM08/saison_id/2008",
           "https://www.transfermarkt.ch/europameisterschaft-2004/gesamtspielplan/pokalwettbewerb/EM04/saison_id/2004",
           "https://www.transfermarkt.ch/europameisterschaft-2004/gesamtspielplan/pokalwettbewerb/EM04/saison_id/2000"
           )


#for (a in 1:6) {

#links <- c(paste0("https://www.transfermarkt.ch/wm-qualifikation-europa/gesamtspielplan/pokalwettbewerb/EMQ",a,"/saison_id/2020"),
#           paste0("https://www.transfermarkt.ch/wm-qualifikation-europa/gesamtspielplan/pokalwettbewerb/EMQ",a,"/saison_id/2016"),
#           paste0("https://www.transfermarkt.ch/wm-qualifikation-europa/gesamtspielplan/pokalwettbewerb/EMQ",a,"/saison_id/2012"),
#           paste0("https://www.transfermarkt.ch/wm-qualifikation-europa/gesamtspielplan/pokalwettbewerb/EMQ",a,"/saison_id/2008"),
#           paste0("https://www.transfermarkt.ch/wm-qualifikation-europa/gesamtspielplan/pokalwettbewerb/EMQ",a,"/saison_id/2004"),
#           paste0("https://www.transfermarkt.ch/wm-qualifikation-europa/gesamtspielplan/pokalwettbewerb/EMQ",a,"/saison_id/2000")
#           )

for (link in links) {
  
#Get IDs
url <- link
webpage <- read_html(url)

ids_id <- webpage %>%
  html_nodes(xpath = "//td/a") %>%
  html_attr("href")

ids <- data.frame(ids_id)
ids$check <- grepl("spielbericht",ids$ids_id)

ids <- na.omit(ids[ids$check == TRUE,])

new_matches <- as.integer(gsub(".*spielbericht/", "",ids$ids_id))

print(paste0(length(new_matches)," matches found"))

for (i in new_matches) {
url <- paste0("https://www.transfermarkt.us/frankreich_kroatien/statistik/spielbericht/",i)
webpage <- read_html(url)
ID <- i
url
team_home <- html_text(html_nodes(webpage,".sb-vereinslink"))[1]
team_away <- html_text(html_nodes(webpage,".sb-vereinslink"))[2] #3

tournament <- trimws(gsub("\n","",gsub("\t","",html_text(html_nodes(webpage,"h2"))[1])))
stage <- trimws(gsub("\n","",gsub("[|].*","",html_text(html_nodes(webpage,".sb-datum"))[1])))

datum <- gsub( ".*(\\d{1,2}.\\d{1,2}.\\d{2,4}).*", "\\1", html_text(html_nodes(webpage,".sb-datum"))[1]) #2
zeit <- gsub( ".*(\\d{1,2}:\\d{1,2}).*", "\\1", html_text(html_nodes(webpage,".sb-datum"))[1]) #2


result_fulltime <- gsub("[(].*","",html_text(html_nodes(webpage,".sb-endstand")))[1]
result_fulltime <- gsub( ".*(\\d{1}:\\d{1}).*", "\\1",result_fulltime)

result_halbzeit <- gsub( ".*(\\d{1}:\\d{1}).*", "\\1", html_text(html_nodes(webpage,".sb-halbzeit")))[1]

zuschauer <- gsub( "[.]", "", html_text(html_nodes(webpage,".sb-zusatzinfos")))
zuschauer <- as.numeric(gsub("\\D", "", zuschauer))

schiedsrichter <- str_split(html_text(html_nodes(webpage,".sb-zusatzinfos")),"Referee:")[[1]][2]
schiedsrichter <- trimws(gsub("\t","",schiedsrichter))

shots_overall_home <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[1])
shots_overall_away <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[2])
shots_missed_home <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[3])
shots_missed_away <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[4])
shots_target_home <- shots_overall_home - shots_missed_home
shots_target_away <- shots_overall_away - shots_missed_away
shots_hold_home <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[5])
shots_hold_away <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[6])
corner_home <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[7])
corner_away <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[8])
freekicks_home <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[9])
freekicks_away <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[10])
fouls_home <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[11])
fouls_away <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[12])
offside_home <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[13])
offside_away <- as.numeric(html_text(html_nodes(webpage,".sb-statistik-zahl"))[14])

#Write in dataframe
new_data <- data.frame(ID,team_home,team_away,tournament,stage,
                       datum,zeit,result_fulltime,result_halbzeit,zuschauer,schiedsrichter,
                       shots_overall_home,shots_overall_away,shots_target_home,shots_target_away,
                       shots_missed_home,shots_missed_away,shots_hold_home,shots_hold_away,
                       corner_home,corner_away,freekicks_home,freekicks_away,
                       fouls_home,fouls_away,offside_home,offside_away)



colnames(new_data) <- c("ID","team_home","team_away","tournament","stage",
                        "date","time","result","result_halftime","crowd","referee",
                        "shots_overall_home","shots_overall_away","shots_target_home","shots_target_away",
                        "shots_missed_home","shots_missed_away","shots_hold_home","shots_hold_away",
                        "corner_home","corner_away","freekicks_home","freekicks_away",
                        "fouls_home","fouls_away","offside_home","offside_away")


data_transfermarkt_new <- rbind(data_transfermarkt_new,new_data)

print("scraping completed")
print(new_data)
}
}
#}
saveRDS(data_transfermarkt_new,file="./Data/euro_data_raw.rds")
data_transfermarkt_new <- readRDS("./Data/euro_data_raw.rds")


###Clean Data
clean_data <- data_transfermarkt_new %>%
  filter(nchar(date) < 10,
         grepl("-",result) == FALSE)

clean_data$date <- paste0("bla",clean_data$date)
clean_data$date <- gsub("bla0[/]","10/",clean_data$date)
clean_data$date <- gsub("bla1[/]","11/",clean_data$date)
clean_data$date <- gsub("bla","",clean_data$date)


#clean_data <- clean_data[-1,]
clean_data$date <- as.Date(clean_data$date,format="%m/%d/%y")
clean_data$goals_home <- NA
clean_data$goals_away <- NA
clean_data$points_home <- NA
clean_data$points_away <- NA

#Transformations
clean_data$goals_home <- parse_number(gsub(":.*","",clean_data$result))
clean_data$goals_away <- parse_number(gsub(".*:","",clean_data$result))

for (i in 1:nrow(clean_data)) {
  if (clean_data$goals_home[i] > clean_data$goals_away[i]) {
    clean_data$points_home[i] <- 3
    clean_data$points_away[i] <- 0
  } else if (clean_data$goals_home[i] < clean_data$goals_away[i]) {
    clean_data$points_home[i] <- 0
    clean_data$points_away[i] <- 3
  } else {
    clean_data$points_home[i] <- 1
    clean_data$points_away[i] <- 1
  }
}

saveRDS(clean_data,file="./Data/euro_data_clean.rds")

