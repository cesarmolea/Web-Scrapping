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
if (!require("gtools")) install.packages("gtools"); library("gtools")
if (!require("tm")) install.packages("tm"); library("tm")
if (!require("qdap")) install.packages("qdap"); library("qdap")
if (!require("dplyr")) install.packages("dplyr"); library("dplyr")
if (!require("tidytext")) install.packages("tidytext"); library("tidytext")
if (!require("wordcloud")) install.packages("wordcloud"); library("wordcloud")
if (!require("SnowballC")) install.packages("SnowballC"); library("SnowballC")


######################################
## 1. Defining the URLs          #####
######################################
setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Mexico")
link <- "http://gaceta.diputados.gob.mx/Gaceta" # This is the base url
gaceta <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII") # Session number
days <- as.numeric(days_in_month(2) + days_in_month(3) + days_in_month(4)) # Second ordinary period: Feb-Mar-Apr
years <- as.character(c(2017:2018)) # From 2000 to 2018

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
  month <- ifelse(month(date_full[i])== 2, "feb", 
                  ifelse(month(date_full[i])== 3, "mar", "abr"))
    legislatura <- ifelse(year(date_full[i]) == 2001 | year(date_full[i]) == 2002 |
                     year(date_full[i]) == 2003, "58", ifelse(year(date_full[i]) == 2004 |
                     year(date_full[i]) == 2005 | year(date_full[i]) == 2006, "59", 
                     ifelse(year(date_full[i]) == 2007 | year(date_full[i]) == 2008 |
                     year(date_full[i]) == 2009, "60", ifelse(year(date_full[i]) == 2010 |
                     year(date_full[i]) == 2011 | year(date_full[i]) == 2012, "61", 
                     ifelse(year(date_full[i]) == 2013 | year(date_full[i]) == 2014 |
                     year(date_full[i]) == 2015, "62", "63")))))
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
      words <- str_detect(p_nodes_text, "[Bb]anco-?\\s*[Ii]nter-?\\s*[Aa]mericano|[Bb]anco\\s*[Mm]undial|[Ff]ondo\\s*[Mm]onetario")
      match_words <- p_nodes[words]
      match_words <- str_replace_all(as.character(match_words), "[\r\t\n]" , "")
      match_words <- str_replace_all(as.character(match_words), "<.*?>" , "")
      match_words <- str_replace_all(as.character(match_words), "[^[:alnum:][:blank:]?&/\\-]" , "")
      match_words <- str_replace_all(as.character(match_words), "UOO", "")
      all_matches <- unique(c(match_words))
      return(all_matches)
    }
    , silent = TRUE
  )
}

######################################
## 3. Using the function and the URLs#
######################################
text <- sapply(out_links, match)
df <- as.data.frame(unlist(text))
colnames(df) <- c("text")  

######################################
## 4. Text Analysis                  #
######################################

df$text <- as.character(df$text)
df$text <- tolower(df$text)
df$text <- tm::removeNumbers(df$text)
df$text <- str_replace_all(df$text, "  ", "")
df$text <- str_replace_all(df$text, pattern = "[[:punct:]]", " ")

df$word_BID <- ifelse(str_detect(df$text, "banco interamericano"), 1, 0)
df$word_BM <- ifelse(str_detect(df$text, "banco mundial"), 1, 0)
df$word_FMI <- ifelse(str_detect(df$text, "fondo monetario"), 1, 0)

# Creating corpus and tdm
corpus <- Corpus(VectorSource(df$text)) # turn into corpus
tdm <- TermDocumentMatrix(corpus) # create tdm from the corpus

custom_stop_words <- bind_rows(stop_words,
                               data_frame(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))

# Count number of words   
df %>% 
  filter(word_BID == 1) %>%
  count()

# Most frequent words
df_bid <- df %>% 
  filter(word_BID == 1) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE)

df_bid <- filter(df_bid, !grepl("banco|bid|desarrollo|interamericano", word))
df_bid

# Wordcloud for BID
set.seed(1234)
wordcloud(words = df_bid$word, freq = df_bid$n, min.freq = 1,
          max.words=15, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))


# And then we can save it as a .csv file
write.csv(df, file = "test.csv")

