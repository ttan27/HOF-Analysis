# ============================================================================
# Title: Web Scraping Tables
# Description:
#   This script web scrapes links to get data tables.
# Input(s): URLs
# Output(s): .csv files
# Author: Timothy Tan
# Date: 2-7-2018
# ============================================================================

library(XML)
library(xml2)
library(rvest)
library(magrittr)

download.file("https://www.baseball-reference.com/leaders/WAR_career.shtml", 
              'data/war_leaders.html')
leadWAR <- as.data.frame(readHTMLTable('data/war_leaders.html', header = TRUE,
                                       stringsAsFactors = FALSE))
leadWAR <- leadWAR[,-1]
names(leadWAR) <- c('Name', 'WAR', 'Bats/Throws')
leadWAR <- leadWAR[-1,]
for(i in 1:nrow(leadWAR)) {
  leadWAR$Name[i] <- gsub("\\(.*\\)", "", leadWAR$Name[i])
  leadWAR$Name[i] <- substr(leadWAR$Name[i], 1, nchar(leadWAR$Name[i])-1)
}

leadWAR$HoF <- c()
for(i in 1:nrow(leadWAR)) {
  if (substr(leadWAR$Name[i], nchar(leadWAR$Name[i]), 
             nchar(leadWAR$Name[i])) == "+") {
    leadWAR$HoF[i] <- TRUE
    leadWAR$Name[i] <- substr(leadWAR$Name[i], 1, nchar(leadWAR$Name[i])-1)
  }
  else {
    leadWAR$HoF[i] <- FALSE
  }
}
leadWAR$WAR <- as.numeric(leadWAR$WAR)
row.names(leadWAR) <- 1:nrow(leadWAR)
for(i in 1:nrow(leadWAR)) {
  if (leadWAR$Name[i] == 'Player') {
    leadWAR <- leadWAR[-i,]
  }
}

write_csv(leadWAR, "data/war_leaders.csv")






