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
if (!require("fastDummies")) install.packages("fastDummies"); library("fastDummies")


######################################
## 1. Defining the URLs          #####
######################################
setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Mexico")
link <- "http://gaceta.diputados.gob.mx/Gaceta" # This is the base url
gaceta <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII") # Session number
days <- as.numeric(days_in_month(9) + days_in_month(10) + days_in_month(11) + days_in_month(12)) # Second ordinary period: Feb-Mar-Apr
years <- as.character(c(2000:2018)) # From 2000 to 2018

years_vec <- rep(years, days)
dates <- rep(NA, length(years)*days) # This is the size of the vector to store the dates
list_of_vecs <- list() # we will store them in a list

for (i in 1:length(years)){
  vec <- seq(ymd(paste0(years[i],"/9/1")), by = "day", length.out = days)
  vec2 <- vec[which(wday(vec) %in% 1:5)] # we just keep the workdays
  list_of_vecs[[i]] <- as.character(vec2) # and we store them in the list
  }

dates <- unlist(list_of_vecs) # we get rid of the list
date_full <- sort(rep(dates,length(gaceta))) # we repeat the date to then paste the roman numerals
gaceta_full <- rep(gaceta, length(dates)) # we do the same the the roman numeral<-
date_gaceta_full <- paste0(str_remove_all(as.character(date_full), pattern =  "-"), "-", gaceta_full, ".html") # we create the final part of the URL

out_links_2 <- rep(NA, length(date_gaceta_full)) # this is the final size of the vector that will contain all the URLS from february to march of 2016

for (i in 1:length(date_gaceta_full)){
  month <- ifelse(month(date_full[i])== 9, "sep", 
                  ifelse(month(date_full[i])== 10, "oct",
                  ifelse(month(date_full[i])== 11, "nov", "dic")))
    legislatura <- ifelse(year(date_full[i]) == 2001 | year(date_full[i]) == 2002 |
                     year(date_full[i]) == 2003, "58", ifelse(year(date_full[i]) == 2004 |
                     year(date_full[i]) == 2005 | year(date_full[i]) == 2006, "59", 
                     ifelse(year(date_full[i]) == 2007 | year(date_full[i]) == 2008 |
                     year(date_full[i]) == 2009, "60", ifelse(year(date_full[i]) == 2010 |
                     year(date_full[i]) == 2011 | year(date_full[i]) == 2012, "61", 
                     ifelse(year(date_full[i]) == 2013 | year(date_full[i]) == 2014 |
                     year(date_full[i]) == 2015, "62", "63")))))
  year <- year(date_full[i])
  out_links_2[i] <- paste(link, legislatura, year, month, date_gaceta_full[i], sep="/")
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
      words <- str_detect(p_nodes_text, "[Bb]anco-?\\s*[Ii]nter-?\\s*[Aa]mericano|[Bb]anco\\s*[Mm]undial|[Ff]ondo\\s*[Mm]onetario|OCDE|CAF")
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
text2 <- sapply(out_links_2, match)
df2 <- as.data.frame(unlist(text2))
colnames(df2) <- c("text")  

# We extract the year and month 
for (i in 1:length(df2)){
  df2$year <- as.factor(substr(rownames(df2[i]), 42, 45))
  df2$month <- as.factor(substr(rownames(df2[i]), 55, 56))
}


######################################
## 4. Text Analysis                  #
######################################

df2$text <- as.character(df2$text)
df2$text <- tolower(df2$text)
df2$text <- tm::removeNumbers(df2$text)
df2$text <- str_replace_all(df2$text, "  ", "")
df2$text <- str_replace_all(df2$text, pattern = "[[:punct:]]", " ")

df2$word_BID <- ifelse(str_detect(df2$text, "banco interamericano"), 1, 0)
df2$word_BM <- ifelse(str_detect(df2$text, "banco mundial"), 1, 0)
df2$word_FMI <- ifelse(str_detect(df2$text, "fondo monetario"), 1, 0)
df2$word_OCDE <- ifelse(str_detect(df2$text, "ocde"), 1, 0)


## Joining the the data

ord.1 <- filter(df, !(word_BID==0 & word_BM==0 & word_FMI==0 & word_OCDE==0))
ord.2 <- filter(df2, !(word_BID==0 & word_BM==0 & word_FMI==0 & word_OCDE==0))
all.periods <- rbind(ord.1, ord.2)

write.csv(all.periods, file = "all.periods.csv")

all.periods$type <- ifelse((all.periods$word_BID == 1), 0, 
                           ifelse((all.periods$word_BM == 1), 1, 
                                  ifelse((all.periods$word_FMI == 1), 2, 3)))

#####################
###    BID        ###
#####################
# Counting number of words   
all.periods %>% 
  group_by(type) %>%
  count()

all.periods %>% 
  filter(word_BID == 1) %>%
  count()

# Count of most frequent words
all_bid <- all.periods %>% 
  filter(word_BID == 1) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE)

all_bid <- filter(all_bid, !grepl("banco|bid|desarrollo|interamericano", word))
all_bid

# Wordcloud for BID
set.seed(1234)
wordcloud(words = all_bid$word, freq = all_bid$n, min.freq = 1,
          max.words = 70, random.order = FALSE,  
          colors = brewer.pal(8, "Dark2"), scale =c(3.5, .25))







# We can also plot the most frequent words
barplot(all_bid[1:10,]$n, las = 2, names.arg = all_bid[1:10,]$word,
        col ="lightblue", main ="Palabras más comunes BID",
        ylab = "Frecuencia de palabras")

########################
##  Banco mundial     ##
########################
all.periods %>% 
  filter(word_BM == 1) %>%
  count()

# Count of most frequent words
all_bm <- all.periods %>% 
  filter(word_BM == 1) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE)

all_bm <- filter(all_bm, !grepl("banco|mundial", word))
all_bm

set.seed(1234)
wordcloud(words = all_bm$word, freq = all_bm$n, min.freq = 1,
          max.words = 100, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

################################
## Fondo Monetario Internacional
################################
all.periods %>% 
  filter(word_FMI == 1) %>%
  count()

# Count of most frequent words
all_fmi <- all.periods %>% 
  filter(word_FMI == 1) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE)

all_fmi <- filter(all_fmi, !grepl("fondo|monetario|internacional|fmi", word))
all_fmi

set.seed(1234)
wordcloud(words = all_fmi$word, freq = all_fmi$n, min.freq = 1,
          max.words = 35, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

## Plotting the most common words per organization
par(mfrow = c(2,2))
barplot(all_bid[1:10,]$n, names.arg = all_bid[1:10,]$word,
        col ="lightblue", main ="Palabras más comunes en menciones del BID",
        ylab = "Frecuencia de palabras")
barplot(all_bm[1:10,]$n, names.arg = all_bm[1:10,]$word,
        col ="red", main ="Palabras más comunes en menciones del Banco Mundial",
        ylab = "Frecuencia de palabras")
barplot(all_fmi[1:10,]$n, names.arg = all_fmi[1:10,]$word,
        col ="green", main ="Palabras más comunes en menciones del FMI",
        ylab = "Frecuencia de palabras")

########################
## Now for OCDE
########################
df %>% 
  filter(word_OCDE == 1) %>%
  count()

# Count of most frequent words
df_ocde <- df %>% 
  filter(word_OCDE == 1) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE)

df_ocde <- filter(df_ocde, !grepl("ocde|organización|cooperación|desarrollo|económicos", word))
df_ocde

set.seed(1234)
wordcloud(words = df_ocde$word, freq = df_ocde$n, min.freq = 1,
          max.words = 25, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, "Dark2"))

barplot(df_ocde[1:10,]$n, las = 2, names.arg = df_ocde[1:10,]$word,
        col ="lightblue", main ="Palabras más comunes",
        ylab = "Frecuencia de palabras")


# Creating corpus and tdm
corpus <- Corpus(VectorSource(all.periods$text)) # turn into corpus
tdm <- TermDocumentMatrix(corpus) # create tdm from the corpus


# comparison of mentions across decades

# 2001-2010
all.periods$year <- as.character(all.periods$year)
all.periods$year <- as.numeric(all.periods$year)


decade1 <- filter(all.periods, year <= 2009)
decade2 <- filter(all.periods, year > 2009)

bid_1 <- decade1 %>% 
  filter(word_BID == 1) %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words) %>%
  count(word, sort = TRUE)

bid_1 <- filter(bid_1, !grepl("banco|bid|desarrollo|interamericano", word))

# Wordcloud for BID
set.seed(1234)
wordcloud(words = all_bid$word, freq = bid_1$n, min.freq = 1,
          max.words = 70, random.order = FALSE,  
          colors = brewer.pal(8, "Dark2"), scale =c(3.5, .25))
