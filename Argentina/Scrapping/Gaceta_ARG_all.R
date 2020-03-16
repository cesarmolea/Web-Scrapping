### Scraping confianza

#Packages

if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("magrittr")) install.packages("magrittr"); library("magrittr")
if (!require("xml2")) install.packages("xml2"); library("xml2")
if (!require("stringr")) install.packages("stringr"); library("stringr")
if (!require("XML")) install.packages("XML"); library("XML")
if (!require("combinat")) install.packages("combinat"); library("combinat")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("RCurl")) install.packages("RCurl"); library("RCurl")
if (!require("gtools")) install.packages("gtools"); library("gtools")
if (!require("tm")) install.packages("tm"); library("tm")
if (!require("qdap")) install.packages("qdap"); library("qdap")
if (!require("dplyr")) install.packages("dplyr"); library("dplyr")
if (!require("tidytext")) install.packages("tidytext"); library("tidytext")
if (!require("wordcloud")) install.packages("wordcloud"); library("wordcloud")
if (!require("SnowballC")) install.packages("SnowballC"); library("SnowballC")
if (!require("RWeka")) install.packages("RWeka"); library("RWeka")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")
if (!require("foreach")) install.packages("foreach"); library("foreach")
if (!require("iterators")) install.packages("iterators"); library("iterators")
if (!require("parallel")) install.packages("parallel"); library("parallel")
if (!require("tm")) install.packages("tm"); library("tm")
if (!require("tabulizer")) install.packages("tabulizer"); library("tabulizer")
if (!require("stringi")) install.packages("stringi"); library("stringi")
if (!require("purrr")) install.packages("purrr"); library("purrr")


#############################################
### Set directory                      ####
#############################################
setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Preprocess/docs")
path <- setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Preprocess/docs")

#############################################
### 1. Reading the text files    ####
#############################################

# We read the text files
list.files() # to see the subfolders that we have
files <- dir(path, pattern = ".txt")
outlist <- rep(list(list()), length(files))

#############################################
### 2. Cleaning the text                 ####
#############################################
# We use the do parallel command to make it quicker, since we have tons of files
usecores <- detectCores() -1
cl <- makeCluster(usecores)
registerDoParallel(cl)

outlist <- foreach (i = 1:length(outlist)) %dopar%  { 
  library(tm)
  library(magrittr)
  library(tidyverse)
  readLines(files[i])  %>% 
    str_replace_all("[\r\n]", "") %>%
    str_replace_all("[^[:alnum:][:blank:][:punct:]]" , "") %>%
    str_replace_all("[·.]" , "") %>%
    str_replace_all("á" , "a") %>% 
    str_replace_all("é" , "e") %>%   
    str_replace_all("í" , "i") %>% 
    str_replace_all("ó" , "o") %>% 
    str_replace_all("ó" , "o") %>% 
    tolower() %>%
    removePunctuation() %>%
    removeNumbers() %>%
    stripWhitespace() 
}


#############################################
### 3. Building term document matrix     ####
#############################################

# I create a list of lists to store every single word from every line
newlist <- rep(list(list()), length(outlist))
# I store each line of words as a separate word based on the space in the lines

#newlist <- foreach (i = 1:length(outlist)) %dopar%  {
#  library(stringr)
#  unlist(strsplit(outlist[[i]],"[[:space:]]"))
# } 

newlist <- purrr::map(outlist, strsplit, "[[:space:]]")

aux <- foreach (i = 1:length(newlist)) %dopar%  {
  as.vector(unlist(newlist[i]))
} 

# I added a dictionary of stopping words in spanish
stop_words <- data.frame(word = tm::stopwords("spanish"),
                         lexicon = "custom")
stop_words$word <- as.character(stop_words$word)
list.stop.words <- as.list(stop_words[, 1])

# Then I extract the stopping words from the list of words
aux2 <- foreach (i =  1:2) %dopar%  {
  lapply(aux[[i]], function(x) {
    y = list.stop.words
    non.matching = setdiff(x, y)
    return(non.matching)
  })
}

stopCluster(cl) # we need to stop it


########################################## 
### 3.1 Unlist the words into a dataframe
##########################################

df <- rep(list(data_frame()), length(aux2))

# Now we need to add the date to each list
for (i in 1:length(aux2))  { 
  df[[i]] <- as.data.frame(unlist(aux2[i]))
  df[[i]]$date <- as.factor(substr(files[[i]], 1, 9))
  colnames(df[[i]]) <- c("word", "date")
  #df[[i]]$word <- as.character(df[[i]]$word)
}

# Appending the n dataframes from the list
all <- tibble()
for (i in 1:length(df)) {
  all <- bind_rows(all, df[[i]])
}

# Checking that we have different number of words
all %>% group_by(date) %>% 
  summarise(n  = n(), 
            n2= length(unique(word)),
            n3 = str_c(word, collapse = ', ')) %>% View()

# Counting the number of words in each document
dtm <- all %>% 
  group_by(date, word) %>% 
  dplyr::summarize(n = n())

########################################## 
### 4. Counting mentions
##########################################

mentions <- dtm %>%
  group_by(date) %>%
  filter(word == "banco" | word == "interamericano" | word == "bid" |word == "mundial" 
         |word == "fondo" |word == "monetario" |word == "internacional"
         |word == "fmi" |word == "ocde" |word == "universidad")

# We add the day, month, and number
mentions$code <- substr(mentions$date, 1, 3)
mentions$month <- substr(mentions$date, 5, 6)
mentions$day <- substr(mentions$date, 8, 9)

mentions$year <- ifelse((mentions$code) == "119", "2001",
                        ifelse((mentions$code) == "120", "2002", 
                               ifelse((mentions$code) == "121", "2003",
                                      ifelse((mentions$code) == "122", "2004",
                                             ifelse((mentions$code) == "123", "2005",
                                                    ifelse((mentions$code) == "124", "2006",
                                                           ifelse((mentions$code) == "125", "2007", 
                                                                  ifelse((mentions$code) == "126", "2008",
                                                                         ifelse((mentions$code) == "127", "2009",
                                                                                ifelse((mentions$code) == "128", "2010",      
                                                                                       ifelse((mentions$code) == "129", "2011",
                                                                                              ifelse((mentions$code) == "130", "2012",     
                                                                                                     ifelse((mention$code) == "131", "2013",
                                                                                                            ifelse((mentions$code) == "132", "2014",
                                                                                                                   ifelse((mentions$code) == "133", "2015",
                                                                                                                          ifelse((mentions$code) == "134", "2017",
                                                                                                                                 ifelse((mentions$code) == "135", "2018", "2019")))))))))))))))))





