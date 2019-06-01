######################################
##        Web Scrapping          #####
######################################

# This code performs webscrapping of the Gaceta Parlamentaria [parliamentary gazette], a 
#  repository of documents from the activities of the Mexican Chamber of Deputies

######################################
## 0. Install packages           #####
######################################

if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("magrittr")) install.packages("magrittr"); library("magrittr")
if (!require("xml2")) install.packages("xml2"); library("xml2")
if (!require("stringr")) install.packages("stringr"); library("stringr")
if (!require("XML")) install.packages("XML"); library("XML")
if (!require("combinat")) install.packages("combinat"); library("combinat")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")


######################################
## 1. Defining the URLS          #####
######################################

link <- "http://gaceta.diputados.gob.mx/Gaceta" # This is the base url
legislatura <- c("63") # Just 63 Legislature
gaceta <- c("II", "III", "IV", "V", "VI") # Session number
days <- as.numeric(days_in_month(1) + days_in_month(2) + days_in_month(3)) # Second ordinary period: Feb-Mar-Apr
years <- as.character(c(2015:2018)) # We know that the 63th Legislatures was from 2015 to 2018

years_vec <- rep(years, days)
dates <- rep(NA, length(years)*days) # This is the size of the vector to store the dates
list_of_vecs <- list() # we will store them in a list

for (i in 1:length(years)){
  vec <- seq(ymd(paste0(years[i],"/2/1")), by = "day", length.out = days)
  vec2 <- vec[which(wday(vec) %in% 1:5)] # we just keep the workdays
  list_of_vecs[[i]] <- as.character(vec2) # and we store them in the list
  }

dates <- unlist(list_of_vecs) # we get rid of the list


date_full <- sort(rep(dates,length(gaceta))) # we repeat the date to then paste the roman numerals
gaceta_full <- rep(gaceta, length(dates)) # we do the same the the roman numeral<-
date_gaceta_full <- paste0(str_remove_all(as.character(date_full), pattern =  "-"), "-", gaceta_full, ".html") # we create the final part of the URL

out_links <- rep(NA, length(date_gaceta_full)) # this is the final size of the vector that will contain all the URLS from february to march of 2016

for (i in 1:length(date_gaceta_full)){
  month <- ifelse(month(date_full[i])==2, "feb", ifelse(month(date_full[i])==3, "mar", "abr"))
  year <- year(date_full[i])
  out_links[i] <- paste(link, legislatura, year, month, date_gaceta_full[i], sep="/")
  }

head(out_links)

######################################
## 2. Scrapping through the URLS   ###
######################################

# We will create a function to read each URL and then extract the paragraphs contaning the words that we want.
# We know that some of the urls do not exist, thus, we need to adapt the function to overcome the errors
#   when the code cannot find the url. 

match <- function(url) { 
  try(
    {
      html <- read_html(url)
      p_nodes <- html %>% html_nodes("p")
      p_nodes_text <- p_nodes %>% html_text()
      words_a <- str_detect(p_nodes_text, fixed("Banco Interamericano de Desarrollo"))
      words_b <- str_detect(p_nodes_text, fixed("Interamericano de Desarrollo"))
      match_words_a <- p_nodes[words_a]
      match_words_b <- p_nodes[words_b]
      match_words_a <- str_replace_all(as.character(match_words_a), "[\r\t\n]" , "")
      match_words_b <- str_replace_all(as.character(match_words_b), "[\r\t\n]" , "")
      all_matches <- unique(c(match_words_a, match_words_b))
      return(all_matches)
    }
    , silent = TRUE
  )
}

######################################
## 3. Using the function and the URLS#
######################################

# Now that we create our function we use need to give it the vector of URLS where we want to extract the data
# We use sapply to facilitate, this will yield a list
list_BID <- sapply(out_links, match)
date_BID <- as.data.frame(unlist(list_BID))

# And then we can save it as a .csv file
setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Mexico")
write.csv(date_BID, file = "text.csv")
