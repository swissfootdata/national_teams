library(httr)

url <- "https://www.eloratings.net/1999.tsv"

all_content <- GET(url)
elo_ratings <- content(all_content, col_names = FALSE)


elo_ratings <- elo_ratings[,c(3,2,4)]
colnames(elo_ratings) <- c("two_letter_code","rank","score")
elo_ratings$date <- as.Date("1999-12-31")

years <- 2000:2024

for (year in years) {

url <- paste0("https://www.eloratings.net/",year,".tsv")
  
all_content <- GET(url)
new_data <- content(all_content, col_names = FALSE)
  
  
new_data <- new_data[,c(3,2,4)]
colnames(new_data) <- c("two_letter_code","rank","score")
new_data$date <- as.Date(paste0(year,"-12-31"))
elo_ratings <- rbind(elo_ratings,new_data)
}


