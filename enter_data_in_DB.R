library(DBI) 
library(RMySQL)
library(stringr)

source("C:/Users/simon/OneDrive/Fussballdaten/Tools/Funktionen/Utils.R")

#Auf Datenbank zugreifen
#mydb <- connectDB(db_name="football_data")

world_cup_data$team_home <- str_replace_all(world_cup_data$team_home,"'","\\\\'")
world_cup_data$team_away <- str_replace_all(world_cup_data$team_away,"'","\\\\'")
world_cup_data$tournament <- str_replace_all(world_cup_data$tournament,"'","\\\\'")
world_cup_data$referee <- str_replace_all(world_cup_data$referee,"'","\\\\'")

mydb <- dbConnect(MySQL(), user='Administrator', password='tqYYDcqx43', dbname='football_data', host='33796.hostserv.eu', encoding="utf8")

#DE-Datenbank einlesen
sql_qry <- paste0("INSERT IGNORE INTO matches_national_teams(ID,team_home,team_away,tournament,stage,date,time,result,result_halftime,crowd,referee,goals_home,goals_away,points_home,points_away,
shots_overall_home,shots_overall_away,shots_target_home,shots_target_away,shots_missed_home,shots_missed_away,shots_hold_home,shots_hold_away,
corner_home,corner_away,freekicks_home,freekicks_away,offside_home,offside_away,fouls_home,fouls_away,
three_letter_code_home,three_letter_code_away,two_letter_code_home,two_letter_code_away,
confederation_home,confederation_away,
fifa_ranking_score_home,fifa_ranking_score_away,fifa_ranking_rank_home,fifa_ranking_rank_away,
elo_ranking_score_home,elo_ranking_score_away,elo_ranking_rank_home,elo_ranking_rank_away,
performance_home_last_five_matches,performance_away_last_five_matches
) VALUES")

sql_qry <- paste0(sql_qry, paste(sprintf("('%s', '%s', '%s', '%s' , '%s', '%s', '%s', '%s' , '%s', '%s',
                                         '%s', '%s', '%s', '%s' , '%s', '%s', '%s', '%s' , '%s', '%s',
                                         '%s', '%s', '%s', '%s' , '%s', '%s', '%s', '%s' , '%s', '%s',
                                         '%s', '%s', '%s', '%s' , '%s', '%s', '%s', '%s', '%s', '%s',
                                         '%s', '%s', '%s', '%s' , '%s', '%s', '%s')",
                                         world_cup_data$ID,
                                         world_cup_data$team_home,
                                         world_cup_data$team_away,
                                         world_cup_data$tournament,
                                         world_cup_data$stage,
                                         world_cup_data$date,
                                         world_cup_data$time,
                                         world_cup_data$result,
                                         world_cup_data$result_halftime,
                                         world_cup_data$crowd,
                                         world_cup_data$referee,
                                         world_cup_data$goals_home,
                                         world_cup_data$goals_away,
                                         world_cup_data$points_home,
                                         world_cup_data$points_away,
                                         world_cup_data$shots_overall_home,
                                         world_cup_data$shots_overall_away,
                                         world_cup_data$shots_target_home,
                                         world_cup_data$shots_target_away,
                                         world_cup_data$shots_missed_home,
                                         world_cup_data$shots_missed_away,
                                         world_cup_data$shots_hold_home,
                                         world_cup_data$shots_hold_away,
                                         world_cup_data$corner_home,
                                         world_cup_data$corner_away,
                                         world_cup_data$freekicks_home,
                                         world_cup_data$freekicks_away,
                                         world_cup_data$offside_home,
                                         world_cup_data$offside_away,
                                         world_cup_data$fouls_home,
                                         world_cup_data$fouls_away,
                                         world_cup_data$three_letter_code_home,
                                         world_cup_data$three_letter_code_away,
                                         world_cup_data$two_letter_code_home,
                                         world_cup_data$two_letter_code_away,
                                         world_cup_data$confederation_home,
                                         world_cup_data$confederation_away,
                                         world_cup_data$fifa_ranking_score_home,
                                         world_cup_data$fifa_ranking_score_away,
                                         world_cup_data$fifa_ranking_rank_home,
                                         world_cup_data$fifa_ranking_rank_away,
                                         world_cup_data$elo_ranking_score_home,
                                         world_cup_data$elo_ranking_score_away,
                                         world_cup_data$elo_ranking_rank_home,
                                         world_cup_data$elo_ranking_rank_away,
                                         world_cup_data$performance_home_last_five_matches,
                                         world_cup_data$performance_away_last_five_matches
                                         
), collapse = ","))

dbGetQuery(mydb, "SET NAMES 'utf8'")
rs <- dbSendQuery(mydb, sql_qry)

#Datenbankverbindungen schliessen
dbDisconnectAll()

print("New data written in Database")