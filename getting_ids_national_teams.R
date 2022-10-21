url <- "https://www.transfermarkt.ch/weltmeisterschaft-2018/gesamtspielplan/pokalwettbewerb/WM18/saison_id/2017"
webpage <- read_html(url)

ids_id <- webpage %>%
  html_nodes(xpath = "//td/a") %>%
  html_attr("href")

ids <- data.frame(ids_id)
ids$check <- grepl("spielbericht",ids$ids_id)

ids <- na.omit(ids[ids$check == TRUE,])

new_matches <- as.integer(gsub(".*spielbericht/", "",ids$ids_id))

print(paste0(length(new_matches)," matches found"))

