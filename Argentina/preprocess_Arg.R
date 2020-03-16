###################################
###### Pre-process             ####
###################################

# We transform the pdf files into txt files


if (!require("rvest")) install.packages("rvest"); library("rvest")
if (!require("magrittr")) install.packages("magrittr"); library("magrittr")
if (!require("xml2")) install.packages("xml2"); library("xml2")
if (!require("stringr")) install.packages("stringr"); library("stringr")
if (!require("XML")) install.packages("XML"); library("XML")
if (!require("combinat")) install.packages("combinat"); library("combinat")
if (!require("lubridate")) install.packages("lubridate"); library("lubridate")
if (!require("pdftools")) install.packages("pdftools"); library("pdftools")
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
#if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("tm")) install.packages("tm"); library("tm")
if (!require("pdftools")) install.packages("pdftools"); library("pdftools")
if (!require("tabulizer")) install.packages("tabulizer"); library("tabulizer")

#############################################
### 1.Set directory                      ####
#############################################
setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Scrapping/docs")
path <- setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Scrapping/docs")

#############################################
### 1. Reading the pdf files     ####
#############################################

# We read the .pdf files
list.files() # to see the subfolders that we have
files <- dir(path, pattern = "\\.pdf")
outlist <- rep(list(list()), length(1:2))


docs <- Corpus(DirSource(path),  
               readerControl = list(reader = readPDF, language = "spanish"))

# We use the do parallel command to make it quicker, since we have tons of files
usecores <- detectCores() -1
cl <- makeCluster(usecores)
registerDoParallel(cl)


outlist <- foreach (i = 1:length(docs)) %dopar%  { 
  library(pdftools)
  library(magrittr)
  library(tidyverse)
  library(tm)
  docs[[i]] 
} 

# We store the new text files in the Preprocess location
new_path <- setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Preprocess/docs")

foreach (i = 1:length(docs)) %dopar% { 
  df <- as.data.frame(unlist(outlist[[i]]))
  date <- as.character(substr(docs[[i]]$meta[5], 1, 9))
  write.table(df, file = paste0(new_path, "/", date, ".txt"), sep = "\t", row.names = FALSE)
} 