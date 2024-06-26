library(mgcv)
library(randomForest)
library(dplyr)

source("./2024_Euro/get_data_ec2024.R", encoding = "latin1")
#data_ec2024 <- data_ec2024[(nrow(data_ec2024)-47):nrow(data_ec2024),]

source("./2024_Euro/create_prediction_model_ec2024.R", encoding = "latin1")

#Data Frame Group Stage Simulation
group_stage_simulation <- data.frame("Group","Team",0,0)
colnames(group_stage_simulation) <- c("Group","Team","Score","Rank")

# Train the model 
regr <- randomForest(x = X, y = y, maxnodes = 250, ntree = 1100, type="prob")

n <- 1

while (n < 1001) { #10001

new_games_home <- data_ec2024[,c(16:19,22:23)]
new_games_away <- data_ec2024[,c(16:19,22:23)]
colnames(new_games_away) <- colnames(new_games_away)[c(2,1,4,3,6,5)]

data_ec2024$winning_prob_home <- NA
data_ec2024$losing_prob_home <- NA
data_ec2024$draw_prob <- NA
data_ec2024$prediction_home <- NA
data_ec2024$prediction_away <- NA
data_ec2024$match_finished <- c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                                TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                                TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                                TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                                TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,
                                TRUE,TRUE,TRUE,TRUE,TRUE,TRUE
                                )
data_ec2024$score_home <- c(3,0,3,1,1,0,
                            3,3,1,3,0,1,
                            1,0,1,1,1,1,
                            0,0,0,1,1,0,
                            3,0,0,3,1,1,
                            3,3,1,0,1,3)

#Predict all Group matches
prediction_group_home <- predict(regr, new_games_home, type="prob")
prediction_group_away <- predict(regr, new_games_away, type="prob")


for (m in 1:nrow(data_ec2024)) {

if (data_ec2024$team_home[m] == "Germany") { 
  data_ec2024$winning_prob_home[m] <- prediction_group_home[m,3]
  data_ec2024$losing_prob_home[m] <- prediction_group_home[m,2]
  data_ec2024$draw_prob[m] <- prediction_group_home[m,1]  
} else if (data_ec2024$team_away[m] == "Germany"){
  data_ec2024$winning_prob_home[m] <- prediction_group_away[m,2]
  data_ec2024$losing_prob_home[m] <- prediction_group_away[m,3]
  data_ec2024$draw_prob[m] <- prediction_group_away[m,1] 
}  else {
data_ec2024$winning_prob_home[m] <- (prediction_group_home[m,3]+prediction_group_away[m,2])/2
data_ec2024$losing_prob_home[m] <- (prediction_group_home[m,2]+prediction_group_away[m,3])/2
data_ec2024$draw_prob[m] <- (prediction_group_home[m,1]+prediction_group_away[m,1])/2
}

if (data_ec2024$match_finished[m] == FALSE) {
data_ec2024$prediction_home[m] <- sample(c(3,1,0),prob=c(as.numeric(data_ec2024$winning_prob_home[m]),as.numeric(data_ec2024$draw_prob[m]),as.numeric(data_ec2024$losing_prob_home[m])), size=1)
} else {
data_ec2024$prediction_home[m] <- data_ec2024$score_home[m]  
}  


if (data_ec2024$prediction_home[m] == 3) {
  data_ec2024$prediction_away[m] <- 0
} else if (data_ec2024$prediction_home[m] == 1) {
  data_ec2024$prediction_away[m] <- 1
} else {
  data_ec2024$prediction_away[m] <- 3
}  

}

groups <- unique(data_ec2024$stage)

for (group in groups) {

#Simulate Group outcome
group_outcome_home <- data_ec2024 %>%
  filter(stage == group) %>%
  group_by(team_home) %>%
  summarise(overall_score_home=sum(prediction_home))

group_outcome_away <- data_ec2024 %>%
  filter(stage == group) %>%
  group_by(team_away) %>%
  summarise(overall_score_away=sum(prediction_away))

group_outcome <- merge(group_outcome_home,group_outcome_away,by.x ="team_home",by.y="team_away")
group_outcome$score_overall <- group_outcome$overall_score_home + group_outcome$overall_score_away
group_outcome$rank <- 5-rank(group_outcome$score_overall,ties.method = "random")

group_outcome <- group_outcome %>%
  arrange(team_home)

#Adaptions
if (group == "Group C") {
group_outcome$rank[1] <- 2
group_outcome$rank[4] <- 3
} else if (group == "Group E") {
group_outcome$rank[1] <- 2
group_outcome$rank[2] <- 1  
group_outcome$rank[3] <- 3
group_outcome$rank[4] <- 4  
} else if (group == "Group F") {
  group_outcome$rank[2] <- 3
  group_outcome$rank[4] <- 2 
#} else if (group == "Group H") {
  #  group_outcome$rank[2] <- 3
  #  group_outcome$rank[4] <- 2  
#} else if (group == "Group G") {
#  group_outcome$rank[1] <- 1
#  group_outcome$rank[4] <- 2  
}  

#print(group_outcome)

for (g in 1:nrow(group_outcome)) {
new_entry <- data.frame(group,group_outcome$team_home[g],group_outcome$score_overall[g],group_outcome$rank[g])
colnames(new_entry) <- c("Group","Team","Score","Rank")
group_stage_simulation <- rbind(group_stage_simulation,new_entry) 
}
}
print(n)
n <- n+1
}
n <- n-1
group_stage_simulation <- group_stage_simulation[-1,]


group_stage_summary <- group_stage_simulation %>%
  group_by(Team) %>%
  summarise(average_score = mean(Score)) %>%
  arrange(Team)

group_stage_probabilities <- group_stage_simulation %>%
  group_by(Team, Rank) %>%
  summarise(prob_rank = length(Rank)/n) %>%
  arrange(Team)

group_stage_summary$prob_rank1 <- 0
group_stage_summary$prob_rank2 <- 0
group_stage_summary$prob_rank3 <- 0
group_stage_summary$prob_rank4 <- 0
 
for (g in 1:nrow(group_stage_summary)) {

  team_selection <- group_stage_probabilities %>%
    filter(Team == group_stage_summary$Team[g])
  
  for (t in 1:nrow(team_selection)) {
  
   if (team_selection$Rank[t] == 1) { 
  group_stage_summary$prob_rank1[g] <- team_selection$prob_rank[t]
   }
    if (team_selection$Rank[t] == 2) { 
      group_stage_summary$prob_rank2[g] <- team_selection$prob_rank[t]
    }
    if (team_selection$Rank[t] == 3) { 
      group_stage_summary$prob_rank3[g] <- team_selection$prob_rank[t]
    }
    if (team_selection$Rank[t] == 4) { 
      group_stage_summary$prob_rank4[g] <- team_selection$prob_rank[t]
    }

  }
  
}  

team_groups <- data_ec2024 %>%
  distinct(team_home,.keep_all=TRUE) %>%
  arrange(team_home)

group_stage_summary$Group <- team_groups$stage

group_stage_summary <- group_stage_summary %>%
  arrange(Group,desc(prob_rank1))

print(group_stage_summary)
saveRDS(group_stage_summary,file="./Data/group_stage_prediction.rds")
write.csv(group_stage_summary,file="./Data/group_stage_prediction.csv",row.names = FALSE)


#group_stage_simulation_old <- group_stage_simulation
group_stage_simulation <- rbind(group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation,
                                    group_stage_simulation)
###Data Frame Round of 16
winner_a <- group_stage_simulation %>%
  filter(Group == "Group A",
         Rank == 1) %>%
  select(Team)
winner_b <- group_stage_simulation %>%
  filter(Group == "Group B",
         Rank == 1) %>%
  select(Team)
winner_c <- group_stage_simulation %>%
  filter(Group == "Group C",
         Rank == 1) %>%
  select(Team)
winner_d <- group_stage_simulation %>%
  filter(Group == "Group D",
         Rank == 1) %>%
  select(Team)
winner_e <- group_stage_simulation %>%
  filter(Group == "Group E",
         Rank == 1) %>%
  select(Team)
winner_f <- group_stage_simulation %>%
  filter(Group == "Group F",
         Rank == 1) %>%
  select(Team)

runner_up_a <- group_stage_simulation %>%
  filter(Group == "Group A",
         Rank == 2) %>%
  select(Team)
runner_up_b <- group_stage_simulation %>%
  filter(Group == "Group B",
         Rank == 2) %>%
  select(Team)
runner_up_c <- group_stage_simulation %>%
  filter(Group == "Group C",
         Rank == 2) %>%
  select(Team)
runner_up_d <- group_stage_simulation %>%
  filter(Group == "Group D",
         Rank == 2) %>%
  select(Team)
runner_up_e <- group_stage_simulation %>%
  filter(Group == "Group E",
         Rank == 2) %>%
  select(Team)
runner_up_f <- group_stage_simulation %>%
  filter(Group == "Group F",
         Rank == 2) %>%
  select(Team)

third_placed_teams <- group_stage_simulation %>%
  filter(Rank == 3)
#third_placed_teams$rank <- NA
#for (i in seq(1,nrow(third_placed_teams),6)) {
#third_placed_teams$rank[i:(i+5)] <- 7-rank(third_placed_teams$Score[i:(i+5)],ties.method = "random")
#}  

#third_place_qualified <- third_placed_teams %>%
#  filter(rank < 5)

third_place_qualified <- third_placed_teams %>%
  filter(grepl("Netherlands|Slovenia|Slovakia|Georgia",Team) == TRUE)

third_place_qualified$opponent_R16 <- NA

for(i in seq(1,nrow(third_place_qualified),4)) {
if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group B","Group C","Group D")) == 4) {
  third_place_qualified$opponent_R16[i:(i+3)]<- c("1B","1E","1F","1C")  
}
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group B","Group C","Group E")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1B","1E","1F","1C")  
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group B","Group C","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1B","1E","1F","1C")  
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group B","Group D","Group E")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1E","1F","1B","1C")  
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group B","Group D","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1E","1F","1B","1C")  
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group B","Group E","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1B","1C") 
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group C","Group D","Group E")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1C","1B")   
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group C","Group D","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1C","1B")   
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group C","Group E","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1B","1C")  
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group A","Group D","Group E","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1B","1C")  
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group B","Group C","Group D","Group E")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1E","1F","1C","1B")  
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group B","Group C","Group D","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1C","1B") 
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group B","Group C","Group E","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1C","1B") 
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group B","Group D","Group E","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1F","1E","1C","1B") 
  }
  if (sum(third_place_qualified$Group[i:(i+3)] == c("Group C","Group D","Group E","Group F")) == 4) {
    third_place_qualified$opponent_R16[i:(i+3)]<- c("1E","1F","1C","1B") 
  }
}


third_1C <- third_place_qualified %>%
  filter(opponent_R16 == "1C")
third_1B <- third_place_qualified %>%
  filter(opponent_R16 == "1B")
third_1E <- third_place_qualified %>%
  filter(opponent_R16 == "1E")
third_1F <- third_place_qualified %>%
  filter(opponent_R16 == "1F")

R16_matches <- data.frame(c(runner_up_a$Team,winner_a$Team,
                            winner_c$Team,winner_b$Team,
                            runner_up_d$Team,winner_f$Team,
                            winner_e$Team,winner_d$Team),
                          c(runner_up_b$Team,runner_up_c$Team,
                            third_1C$Team,third_1B$Team,
                            runner_up_e$Team,third_1F$Team,
                            third_1E$Team,runner_up_f$Team))
colnames(R16_matches) <- c("team_home","team_away")
R16_matches$match <- c(rep("Match 37",nrow(winner_a)),
                       rep("Match 38",nrow(winner_a)),
                       rep("Match 39",nrow(winner_a)),
                       rep("Match 40",nrow(winner_a)),
                       rep("Match 41",nrow(winner_a)),
                       rep("Match 42",nrow(winner_a)),
                       rep("Match 43",nrow(winner_a)),
                       rep("Match 44",nrow(winner_a)))

data_team_home <- data_ec2024 %>%
  distinct(team_home,.keep_all=TRUE) %>%
  select(team_home,fifa_ranking_rank_home,elo_ranking_score_home,performance_home_last_five_matches)
data_team_away <- data_ec2024 %>%
  distinct(team_away,.keep_all=TRUE) %>%
  select(team_away,fifa_ranking_rank_away,elo_ranking_score_away,performance_away_last_five_matches)
  
R16_matches <- merge(R16_matches,data_team_home)
R16_matches <- merge(R16_matches,data_team_away)

###Predict Round of 16

# Train the model 
regr <- randomForest(x = X, y = y, maxnodes = 250, ntree = 1100, type="prob")

new_games_home <- R16_matches[,c(4,7,5,8,6,9)]
new_games_away <- R16_matches[,c(4,7,5,8,6,9)]
colnames(new_games_away) <- colnames(new_games_away)[c(2,1,4,3,6,5)]

R16_matches$winning_prob_home <- 0
R16_matches$losing_prob_home <- 0
R16_matches$draw_prob <- 0
R16_matches$prediction <- ""
R16_matches$winner <- ""
R16_matches$loser <- ""

#Predict all Group matches
prediction_group_home <- predict(regr, new_games_home, type="prob")
prediction_group_away <- predict(regr, new_games_away, type="prob")

for (m in 1:nrow(R16_matches)) {
  
  if (R16_matches$team_home[m] == "Germany") { 
    R16_matches$winning_prob_home[m] <- prediction_group_home[m,3]
    R16_matches$losing_prob_home[m] <- prediction_group_home[m,2]
    R16_matches$draw_prob[m] <- prediction_group_home[m,1]  
  } else if (R16_matches$team_away[m] == "Germany"){
    R16_matches$winning_prob_home[m] <- prediction_group_away[m,2]
    R16_matches$losing_prob_home[m] <- prediction_group_away[m,3]
    R16_matches$draw_prob[m] <- prediction_group_away[m,1] 
  }  else {
    R16_matches$winning_prob_home[m] <- (prediction_group_home[m,3]+prediction_group_away[m,2])/2
    R16_matches$losing_prob_home[m] <- (prediction_group_home[m,2]+prediction_group_away[m,3])/2
    R16_matches$draw_prob[m] <- (prediction_group_home[m,1]+prediction_group_away[m,1])/2
  }
  
  R16_matches$prediction[m] <- sample(c("win home","draw","win away"),prob=c(as.numeric(R16_matches$winning_prob_home[m]),as.numeric(R16_matches$draw_prob[m]),as.numeric(R16_matches$losing_prob_home[m])), size=1)
  
  #Add Results
 # if (R16_matches$match[m] == "Match 37") {
#  R16_matches$prediction[m] <- "win home"
#  } else if (R16_matches$match[m] == "Match 38") {
#  R16_matches$prediction[m] <- "win home"
#  } else if (R16_matches$match[m] == "Match 39") {
#    R16_matches$prediction[m] <- "win away"
#  } else if (R16_matches$match[m] == "Match 40") {
#    R16_matches$prediction[m] <- "win home"
#  } else if (R16_matches$match[m] == "Match 41") {
#    R16_matches$prediction[m] <- "win home"
#  } else if (R16_matches$match[m] == "Match 42") {
#    R16_matches$prediction[m] <- "win home"
#  } else if (R16_matches$match[m] == "Match 43") {
#    R16_matches$prediction[m] <- "win home"
#  } else if (R16_matches$match[m] == "Match 44") {
#    R16_matches$prediction[m] <- "win home"
#  }
  
  if (R16_matches$prediction[m] == "draw") {
  R16_matches$prediction[m] <- sample(c("win home","win away"),prob=c(0.5,0.5), size=1)
  }  
  
  if (R16_matches$prediction[m] == "win home") {
    R16_matches$winner[m] <- R16_matches$team_home[m]
    R16_matches$loser[m] <- R16_matches$team_away[m]
  } else {
    R16_matches$winner[m] <- R16_matches$team_away[m]
    R16_matches$loser[m] <- R16_matches$team_home[m]
  }  
  
}

print(table(R16_matches$winner))
###Data Quarter Finals
winner_M37 <- R16_matches %>%
  filter(match == "Match 37") %>%
  select(winner)
winner_M38 <- R16_matches %>%
  filter(match == "Match 38") %>%
  select(winner)
winner_M39 <- R16_matches %>%
  filter(match == "Match 39") %>%
  select(winner)
winner_M40 <- R16_matches %>%
  filter(match == "Match 40") %>%
  select(winner)
winner_M41 <- R16_matches %>%
  filter(match == "Match 41") %>%
  select(winner)
winner_M42 <- R16_matches %>%
  filter(match == "Match 42") %>%
  select(winner)
winner_M43 <- R16_matches %>%
  filter(match == "Match 43") %>%
  select(winner)
winner_M44 <- R16_matches %>%
  filter(match == "Match 44") %>%
  select(winner)

QF_matches <- data.frame(c(winner_M37$winner,winner_M41$winner,winner_M43$winner,winner_M38$winner),
                          c(winner_M39$winner,winner_M42$winner,winner_M44$winner,winner_M40$winner))
colnames(QF_matches) <- c("team_home","team_away")
QF_matches$match <- c(rep("Match 45",nrow(winner_M37)),
                       rep("Match 46",nrow(winner_M37)),
                       rep("Match 47",nrow(winner_M37)),
                       rep("Match 48",nrow(winner_M37)))

QF_matches <- merge(QF_matches,data_team_home)
QF_matches <- merge(QF_matches,data_team_away)

unique(QF_matches)
###Predict Quarter Finals

# Train the model 
regr <- randomForest(x = X, y = y, maxnodes = 250, ntree = 1100, type="prob")

new_games_home <- QF_matches[,c(4,7,5,8,6,9)]
new_games_away <- QF_matches[,c(4,7,5,8,6,9)]
colnames(new_games_away) <- colnames(new_games_away)[c(2,1,4,3,6,5)]

QF_matches$winning_prob_home <- 0
QF_matches$losing_prob_home <- 0
QF_matches$draw_prob <- 0
QF_matches$prediction <- ""
QF_matches$winner <- ""
QF_matches$loser <- ""

#Predict all Group matches
prediction_group_home <- predict(regr, new_games_home, type="prob")
prediction_group_away <- predict(regr, new_games_away, type="prob")

for (m in 1:nrow(QF_matches)) {
  
  if (QF_matches$team_home[m] == "Germany") { 
    QF_matches$winning_prob_home[m] <- prediction_group_home[m,3]
    QF_matches$losing_prob_home[m] <- prediction_group_home[m,2]
    QF_matches$draw_prob[m] <- prediction_group_home[m,1]  
  } else if (QF_matches$team_away[m] == "Germany"){
    QF_matches$winning_prob_home[m] <- prediction_group_away[m,2]
    QF_matches$losing_prob_home[m] <- prediction_group_away[m,3]
    QF_matches$draw_prob[m] <- prediction_group_away[m,1] 
  }  else {
    QF_matches$winning_prob_home[m] <- (prediction_group_home[m,3]+prediction_group_away[m,2])/2
    QF_matches$losing_prob_home[m] <- (prediction_group_home[m,2]+prediction_group_away[m,3])/2
    QF_matches$draw_prob[m] <- (prediction_group_home[m,1]+prediction_group_away[m,1])/2
  }
  
  QF_matches$prediction[m] <- sample(c("win home","draw","win away"),prob=c(as.numeric(QF_matches$winning_prob_home[m]),as.numeric(QF_matches$draw_prob[m]),as.numeric(QF_matches$losing_prob_home[m])), size=1)
  
  #Add Results
#  if (QF_matches$match[m] == "Match 45") {
#    QF_matches$prediction[m] <- "win away"
#  } else if (QF_matches$match[m] == "Match 46") {
#    QF_matches$prediction[m] <- "win home"
#  } else if (QF_matches$match[m] == "Match 47") {
#    QF_matches$prediction[m] <- "win away"
#  } else if (QF_matches$match[m] == "Match 48") {
#    QF_matches$prediction[m] <- "win home"
#  }
  
  if (QF_matches$prediction[m] == "draw") {
    QF_matches$prediction[m] <- sample(c("win home","win away"),prob=c(0.5,0.5), size=1)
  }  
  
  if (QF_matches$prediction[m] == "win home") {
    QF_matches$winner[m] <- QF_matches$team_home[m]
    QF_matches$loser[m] <- QF_matches$team_away[m]
  } else {
    QF_matches$winner[m] <- QF_matches$team_away[m]
    QF_matches$loser[m] <- QF_matches$team_home[m]
  }  
  
}

print(table(QF_matches$winner))
###Data Semi Final
winner_M45 <- QF_matches %>%
  filter(match == "Match 45") %>%
  select(winner)
winner_M46 <- QF_matches %>%
  filter(match == "Match 46") %>%
  select(winner)
winner_M47 <- QF_matches %>%
  filter(match == "Match 47") %>%
  select(winner)
winner_M48 <- QF_matches %>%
  filter(match == "Match 48") %>%
  select(winner)

SF_Matches <- data.frame(c(winner_M45$winner,winner_M47$winner),
                         c(winner_M46$winner,winner_M48$winner))
colnames(SF_Matches) <- c("team_home","team_away")
SF_Matches$match <- c(rep("Match 49",nrow(winner_M45)),
                      rep("Match 50",nrow(winner_M45)))

SF_Matches <- merge(SF_Matches,data_team_home)
SF_Matches <- merge(SF_Matches,data_team_away)


###Predict Semi Finals

# Train the model 
regr <- randomForest(x = X, y = y, maxnodes = 250, ntree = 1100, type="prob")

new_games_home <- SF_Matches[,c(4,7,5,8,6,9)]
new_games_away <- SF_Matches[,c(4,7,5,8,6,9)]
colnames(new_games_away) <- colnames(new_games_away)[c(2,1,4,3,6,5)]

SF_Matches$winning_prob_home <- 0
SF_Matches$losing_prob_home <- 0
SF_Matches$draw_prob <- 0
SF_Matches$prediction <- ""
SF_Matches$winner <- ""
SF_Matches$loser <- ""

#Predict all Group matches
prediction_group_home <- predict(regr, new_games_home, type="prob")
prediction_group_away <- predict(regr, new_games_away, type="prob")

for (m in 1:nrow(SF_Matches)) {
  
  if (SF_Matches$team_home[m] == "Germany") { 
    SF_Matches$winning_prob_home[m] <- prediction_group_home[m,3]
    SF_Matches$losing_prob_home[m] <- prediction_group_home[m,2]
    SF_Matches$draw_prob[m] <- prediction_group_home[m,1]  
  } else if (SF_Matches$team_away[m] == "Germany"){
    SF_Matches$winning_prob_home[m] <- prediction_group_away[m,2]
    SF_Matches$losing_prob_home[m] <- prediction_group_away[m,3]
    SF_Matches$draw_prob[m] <- prediction_group_away[m,1] 
  }  else {
    SF_Matches$winning_prob_home[m] <- (prediction_group_home[m,3]+prediction_group_away[m,2])/2
    SF_Matches$losing_prob_home[m] <- (prediction_group_home[m,2]+prediction_group_away[m,3])/2
    SF_Matches$draw_prob[m] <- (prediction_group_home[m,1]+prediction_group_away[m,1])/2
  }
  
  SF_Matches$prediction[m] <- sample(c("win home","draw","win away"),prob=c(as.numeric(SF_Matches$winning_prob_home[m]),as.numeric(SF_Matches$draw_prob[m]),as.numeric(SF_Matches$losing_prob_home[m])), size=1)
  

  #Add Results
  #if (SF_Matches$match[m] == "Match 49") {
  #  SF_Matches$prediction[m] <- "win home"
  #} else if (SF_Matches$match[m] == "Match 50") {
  #  SF_Matches$prediction[m] <- "win home"
  #} 
  
  if (SF_Matches$prediction[m] == "draw") {
    SF_Matches$prediction[m] <- sample(c("win home","win away"),prob=c(0.5,0.5), size=1)
  }  
  
  if (SF_Matches$prediction[m] == "win home") {
    SF_Matches$winner[m] <- SF_Matches$team_home[m]
    SF_Matches$loser[m] <- SF_Matches$team_away[m]
  } else {
    SF_Matches$winner[m] <- SF_Matches$team_away[m]
    SF_Matches$loser[m] <- SF_Matches$team_home[m]
  }  
  
}


print(table(SF_Matches$winner))

###Data Big and Small Final
winner_M49 <- SF_Matches %>%
  filter(match == "Match 49") %>%
  select(winner)
winner_M50 <- SF_Matches %>%
  filter(match == "Match 50") %>%
  select(winner)



Final_Matches <- data.frame(c(winner_M49$winner),
                         c(winner_M50$winner))
colnames(Final_Matches) <- c("team_home","team_away")
Final_Matches$match <- c(rep("Match 51",nrow(winner_M49))
                         )

Final_Matches <- merge(Final_Matches,data_team_home)
Final_Matches <- merge(Final_Matches,data_team_away)


###Predict Semi Finals

# Train the model 
regr <- randomForest(x = X, y = y, maxnodes = 250, ntree = 1100, type="prob")

new_games_home <- Final_Matches[,c(4,7,5,8,6,9)]
new_games_away <- Final_Matches[,c(4,7,5,8,6,9)]
colnames(new_games_away) <- colnames(new_games_away)[c(2,1,4,3,6,5)]

Final_Matches$winning_prob_home <- 0
Final_Matches$losing_prob_home <- 0
Final_Matches$draw_prob <- 0
Final_Matches$prediction <- ""
Final_Matches$winner <- ""
Final_Matches$loser <- ""

#Predict all Group matches
prediction_group_home <- predict(regr, new_games_home, type="prob")
prediction_group_away <- predict(regr, new_games_away, type="prob")

for (m in 1:nrow(Final_Matches)) {
  
  if (Final_Matches$team_home[m] == "Germany") { 
    Final_Matches$winning_prob_home[m] <- prediction_group_home[m,3]
    Final_Matches$losing_prob_home[m] <- prediction_group_home[m,2]
    Final_Matches$draw_prob[m] <- prediction_group_home[m,1]  
  } else if (Final_Matches$team_away[m] == "Germany"){
    Final_Matches$winning_prob_home[m] <- prediction_group_away[m,2]
    Final_Matches$losing_prob_home[m] <- prediction_group_away[m,3]
    Final_Matches$draw_prob[m] <- prediction_group_away[m,1] 
  }  else {
    Final_Matches$winning_prob_home[m] <- (prediction_group_home[m,3]+prediction_group_away[m,2])/2
    Final_Matches$losing_prob_home[m] <- (prediction_group_home[m,2]+prediction_group_away[m,3])/2
    Final_Matches$draw_prob[m] <- (prediction_group_home[m,1]+prediction_group_away[m,1])/2
  }
  
  Final_Matches$prediction[m] <- sample(c("win home","draw","win away"),prob=c(as.numeric(Final_Matches$winning_prob_home[m]),as.numeric(Final_Matches$draw_prob[m]),as.numeric(Final_Matches$losing_prob_home[m])), size=1)
  

  
  
  if (Final_Matches$prediction[m] == "draw") {
    Final_Matches$prediction[m] <- sample(c("win home","win away"),prob=c(0.5,0.5), size=1)
  }  
  
  if (Final_Matches$prediction[m] == "win home") {
    Final_Matches$winner[m] <- Final_Matches$team_home[m]
    Final_Matches$loser[m] <- Final_Matches$team_away[m]
  } else {
    Final_Matches$winner[m] <- Final_Matches$team_away[m]
    Final_Matches$loser[m] <- Final_Matches$team_home[m]
  }  
  
}

BigFinal <- Final_Matches %>%
  filter(match=="Match 51")

#Datawrapper Output
source("./2024_Euro/datawrapper_output_ec2024.R")

View(prediction_euro)
View(prediction_winner)
