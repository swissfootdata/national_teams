library(dplyr)
library(lubridate)

#Datawrapper Output
teams_flags <- data_ec2024 %>%
  distinct(team_home,.keep_all=TRUE) %>%
  select(team_home,two_letter_code_home) %>%
  rename(team = team_home)

teams_flags$two_letter_code_home <- paste0(":",tolower(teams_flags$two_letter_code_home),":")
teams_flags$two_letter_code_home <- gsub("en","gb-eng",teams_flags$two_letter_code_home)
teams_flags$two_letter_code_home <- gsub("wa","gb-wls",teams_flags$two_letter_code_home)
teams_flags$two_letter_code_home <- gsub("wa","gb-wls",teams_flags$two_letter_code_home)
teams_flags$two_letter_code_home <- gsub("sq","gb-sct",teams_flags$two_letter_code_home)

#World Cup Winner
prediction_winner <- data.frame(table(BigFinal$winner)/10000)
colnames(prediction_winner) <- c("team","probability")
prediction_winner <- merge(teams_flags,prediction_winner,all.x = TRUE)
prediction_winner[is.na(prediction_winner)] <- 0

prediction_winner$team <- paste0(prediction_winner$two_letter_code_home,prediction_winner$team)
colnames(prediction_winner) <- c("team","two_letter_code_home","probability_winner")

#World Cup 2nd
prediction_finalist <- data.frame(table(BigFinal$loser)/10000)
colnames(prediction_finalist) <- c("team","probability")
prediction_finalist <- merge(teams_flags,prediction_finalist,all.x = TRUE)
prediction_finalist[is.na(prediction_finalist)] <- 0

prediction_finalist$team <- paste0(prediction_finalist$two_letter_code_home,prediction_finalist$team)
colnames(prediction_finalist) <- c("team","two_letter_code_home","probability_finalist")

#World Cup SF
prediction_SF <- data.frame(table(SF_Matches$loser)/10000)
colnames(prediction_SF) <- c("team","probability")
prediction_SF <- merge(teams_flags,prediction_SF,all.x = TRUE)
prediction_SF[is.na(prediction_SF)] <- 0

prediction_SF$team <- paste0(prediction_SF$two_letter_code_home,prediction_SF$team)
colnames(prediction_SF) <- c("team","two_letter_code_home","probability_SF")

#World Cup Quarters
prediction_QF <- data.frame(table(QF_matches$loser)/10000)
colnames(prediction_QF) <- c("team","probability")
prediction_QF <- merge(teams_flags,prediction_QF,all.x = TRUE)
prediction_QF[is.na(prediction_QF)] <- 0

prediction_QF$team <- paste0(prediction_QF$two_letter_code_home,prediction_QF$team)
colnames(prediction_QF) <- c("team","two_letter_code_home","probability_QF")

#World Cup R16
prediction_R16 <- data.frame(table(R16_matches$loser)/10000)
colnames(prediction_R16) <- c("team","probability")
prediction_R16 <- merge(teams_flags,prediction_R16,all.x = TRUE)
prediction_R16[is.na(prediction_R16)] <- 0

prediction_R16$team <- paste0(prediction_R16$two_letter_code_home,prediction_R16$team)
colnames(prediction_R16) <- c("team","two_letter_code_home","probability_R16")


#World Cup Out Group Stage
group_stage_summary$prob_out <- group_stage_summary$prob_rank3*(2/6) + group_stage_summary$prob_rank4
prediction_Group <- group_stage_summary[,c(1,8)]
colnames(prediction_Group) <- c("team","probability")
prediction_Group <- merge(teams_flags,prediction_Group,all.x = TRUE)
prediction_Group[is.na(prediction_Group)] <- 0

prediction_Group$team <- paste0(prediction_Group$two_letter_code_home,prediction_Group$team)
colnames(prediction_Group) <- c("team","two_letter_code_home","probability_Group")

#Gather data
prediction_euro <- merge(prediction_Group,prediction_R16)
prediction_euro <- merge(prediction_euro,prediction_QF)
prediction_euro <- merge(prediction_euro,prediction_SF)
prediction_euro <- merge(prediction_euro,prediction_finalist)
prediction_euro <- merge(prediction_euro,prediction_winner)

prediction_euro <- prediction_euro %>%
  arrange(desc(probability_winner),
          probability_Group)

write.csv(prediction_euro,"Output/prediction_euro2024.csv",row.names = FALSE)


###Prediction Winner
prediction_winner_old <- read.csv("https://raw.githubusercontent.com/swissfootdata/national_teams/master/Output/prediction_winner_euro2024.csv")[,1:2]
colnames(prediction_winner_old) <- c("team","probability_winner_old")

prediction_winner <- merge(prediction_euro[,c(1,8)],prediction_winner_old)
prediction_winner$probability_change <- prediction_winner$probability_winner-prediction_winner$probability_winner_old

write.csv(prediction_winner[,c(1,2,4)],"Output/prediction_winner_euro2024.csv",row.names = FALSE)

#Group Stages
group_stage_prediction <- merge(teams_flags,group_stage_summary,by.x="team",by.y="Team")
group_stage_prediction <- group_stage_prediction[order(group_stage_prediction$Group,-group_stage_prediction$prob_rank1),]
group_stage_prediction$team <- paste0(group_stage_prediction$two_letter_code_home,group_stage_prediction$team)
group_stage_prediction <- group_stage_prediction[,-2]
group_stage_prediction <- group_stage_prediction[,c(1,3:6)]



group_stage_prediction <- cbind(group_stage_prediction[1:12,],group_stage_prediction[13:24,])
group_stage_prediction <- rbind(c("Group A",NA,NA,NA,NA,"Group D",NA,NA,NA,NA),group_stage_prediction)
group_stage_prediction <- rbind(group_stage_prediction[1:5,],c("Group B",NA,NA,NA,NA,"Group E",NA,NA,NA,NA),group_stage_prediction[-(1:5),])
group_stage_prediction <- rbind(group_stage_prediction[1:10,],c("Group C",NA,NA,NA,NA,"Group F",NA,NA,NA,NA),group_stage_prediction[-(1:10),])

write.csv(group_stage_prediction,"Output/prediction_group_stages_euro2024.csv",row.names = FALSE)

#Prediction all group stage matches
group_stage_matches_prediction <- data_ec2024
group_stage_matches_prediction$match <- paste0(group_stage_matches_prediction$team_home,"-",group_stage_matches_prediction$team_away)

group_stage_matches_prediction <- group_stage_matches_prediction %>%
  arrange(date)

group_stage_matches_prediction$date_name <- paste0(day(group_stage_matches_prediction$date)," ",
                                                   month(group_stage_matches_prediction$date, label=TRUE, abbr=FALSE)," ",
                                                   year(group_stage_matches_prediction$date))

group_stage_matches_prediction$date_name <- gsub("Dezember","December",group_stage_matches_prediction$date_name)

group_stage_matches_prediction <- group_stage_matches_prediction %>%
  filter(match_finished==FALSE)
write.csv(group_stage_matches_prediction[,c(31,32,24,26,25)],"Output/prediction_group_stages_matches_euro2024.csv",row.names = FALSE)

#Predictions R16
R16_predictions <- R16_matches %>%
  group_by(match) %>%
  summarise(winning_prob_home = sum(prediction == "win home")/10000,
            winning_prob_away = sum(prediction == "win away")/10000,
            match = paste0(team_home[1],"-",team_away[1])
  )


R16_predictions$date_name <- c("29 June 2024","29 June 2024",
                               "30 June 2024","30 June 2024",
                               "1 July 2024","1 July 2024",
                               "2 July 2024","2 July 2024")

write.csv(R16_predictions[,c(1,4,2,3)],"Output/prediction_R16_euro2024.csv",row.names = FALSE)

#Predictions QF
QF_predictions <- QF_matches %>%
  group_by(match) %>%
  summarise(winning_prob_home = sum(prediction == "win home")/10000,
            winning_prob_away = sum(prediction == "win away")/10000,
            match = paste0(team_home[1],"-",team_away[1])
  )


QF_predictions$date_name <- c("5 July 2024","5 July 2024",
                              "6 July 2024","6 July 2024")

write.csv(QF_predictions[,c(1,4,2,3)],"Output/prediction_QF_euro2024.csv",row.names = FALSE)

#Predictions SF
SF_predictions <- SF_Matches %>%
  group_by(match) %>%
  summarise(winning_prob_home = sum(prediction == "win home")/10000,
            winning_prob_away = sum(prediction == "win away")/10000,
            match = paste0(team_home[1],"-",team_away[1])
  )


SF_predictions$date_name <- c("9 July 2024","9 July 2024")

write.csv(SF_predictions[,c(1,4,2,3)],"Output/prediction_SF_euro2024.csv",row.names = FALSE)


#Predictions Final
Final_predictions <- Final_Matches %>%
  group_by(match) %>%
  summarise(winning_prob_home = sum(prediction == "win home")/10000,
            winning_prob_away = sum(prediction == "win away")/10000,
            match = paste0(team_home[1],"-",team_away[1])
  )


Final_predictions$date_name <- c("14 July 2024")

write.csv(Final_predictions[,c(1,4,2,3)],"Output/prediction_Finals_euro2024.csv",row.names = FALSE)

