# Date: January 6th 2020
# Analysis of Argentinean Gazettes
# Author: César Eduardo Montiel Olea, IDB

############################################
### 0. Installing packages              ####
############################################

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
if (!require("quanteda")) install.packages("quanteda"); library("quanteda")


#############################################
### Set directory                      ####
#############################################
setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Preprocess/test")
path <- setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Preprocess/test")

#############################################
### 1. Reading the text files    ####
#############################################

# We read the text files
list.files() # to see the subfolders that we have
files <- dir(path, pattern = "\\.txt")
outlist <- rep(list(list()), length(files)) # we create an outlist to store the files

#############################################
### 2. Cleaning the text                 ####
#############################################
# We use the do parallel command to make it quicker, since we have tons of files
usecores <- detectCores() -1
cl <- makeCluster(usecores)
registerDoParallel(cl)

outlist <- foreach (i = 1:2) %dopar%  { 
  library(magrittr)
  library(tidyverse)
  library(tm)
  readLines(files[i]) %>%
    str_replace_all("[\r\n]", "") %>%
    removePunctuation() %>% 
    tolower() %>%
    removeNumbers() %>%
    stripWhitespace() 
} 


#############################################
### 3. Building a corpus                 ####
#############################################
#test <- paste(outlist[[1]], collapse = ' ')
#aux <- flatten(outlist)

outlist <- map(outlist, paste, collapse = " ") # we paste each sentence into a single vector
df <- rep(list(data_frame()), length(outlist))

# Now we need to add the date to each list
for (i in 1:length(outlist))  { 
  df[[i]] <- as.data.frame(unlist(outlist[i]))
  df[[i]]$date <- as.factor(substr(files[[i]], 1, 9))
  colnames(df[[i]]) <- c("word", "date")
  #df[[i]]$word <- as.character(df[[i]]$word)
}

# Appending the n dataframes from the list into a tibble
all <- tibble()
for (i in 1:length(df)) {
  all <- bind_rows(all, df[[i]])
}
# class(all)

# Creating the corpus
df.corpus <- all %>% 
              rename(text = word) %>% 
               corpus()

summary(df.corpus) # summarizing corpus

# Create document frequency matrix. In this step we also get rid of stopping words
super_dfm <- dfm(df.corpus, remove = stopwords("es"), remove_punct = TRUE)
docnames(super_dfm) <- files # we attach the name of the files
super_dfm

# We can look at the frequency of the words
textstat_frequency(super_dfm, n = 50) # 

#############################################
### 4. Counting mentions                 ####
#############################################
# From dfm to tidy text
tidy_dfm <- tidy(super_dfm)
tidy_dfm

# We break the text into tokens, in this case we want bigrams
text_df_tok <- tidy_dfm %>%
  unnest_tokens(bigram, term, token = "ngrams", n = 2)
text_df_tok

# We group by document (name of document) to have the count the mentions that we want
mentions <- text_df_tok %>%
  group_by(document) %>%
  filter(bigram == "banco mundial" | bigram == "fondo monetario" | bigram == "banco interamericano") %>%
  View()

#############################################
### 5. Most frequent words               ####
#############################################

# Top 50 words
super_dfm %>% 
  textstat_frequency(n = 50) %>% 
  ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
  geom_point() +
  coord_flip() +
  labs(x = NULL, y = "Frequency") +
  theme_minimal()

# wordcloud
set.seed(132)
textplot_wordcloud(super_dfm, max_words = 100)

#############################################
### 6. Bonus to get more analytics       ####
#############################################

###########################################
## calculate n, term frequency,
## inverse document frequency, and tf_idf
############################################
#corpus_tf_idf <- 
#  tidy_dfm %>%
#  count(document, term, sort = T) %>%
# bind_tf_idf(document, term, n) 

######################################
## check highest tf_idf words per doc 
# (tf_idf  tells you how much a word is important for a certain document)
#####################################
#corpus_tf_idf %>%
 # group_by(term) %>%
 # arrange(desc(tf_idf)) %>%
 #  slice(1:3)

#corpus_tf_idf %>% filter(term == "fondo")

#filter_names <- corpus_tf_idf %>% 
 # filter(term =="fondo") %>% 
  # pull(document)
#filter_names

#filtered_corpus <-  corpus_tf_idf %>% 
 # filter(name %in% filter_names)
#filtered_corpus

# tokens <- tokens(df.corpus, what = "word")

