library(mgcv)
library(randomForest)
library(dplyr)

euro_data <- readRDS("./Data/euro_data_complete.rds")

fit <- gam(points_home ~ s(fifa_ranking_rank_home) + s(fifa_ranking_rank_away) + 
             s(elo_ranking_score_home) + s(elo_ranking_score_away) +
             s(performance_home_last_five_matches) + s(performance_away_last_five_matches),  
           data=euro_data)

summary(fit)

#Target variable
euro_data$target <- euro_data$points_home
euro_data$target <- gsub(3,"win home",euro_data$target)
euro_data$target <- gsub(1,"draw",euro_data$target)
euro_data$target <- gsub(0,"win away",euro_data$target)
euro_data$target <- as.factor(euro_data$target)



#Select data
data_rf <- euro_data %>%
  select(fifa_ranking_rank_home,
         fifa_ranking_rank_away,
         elo_ranking_score_home,
         elo_ranking_score_away,
         performance_home_last_five_matches,
         performance_away_last_five_matches,
         target)

data_rf <- na.omit(data_rf)

X <- data_rf[,1:ncol(data_rf)-1]
y <- data_rf$target

# Train the model 
regr <- randomForest(x = X, y = y, maxnodes = 250, ntree = 1100, type="prob")

print(regr)
