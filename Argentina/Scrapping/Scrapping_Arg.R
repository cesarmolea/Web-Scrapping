######################################
##        Web Scrapping          #####
######################################

# This code performs webscrapping of Sessions from the Congress of Argentina
# Deputies have sessions between March 1st and November 30th of each year
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


# https://www.hcdn.gob.ar/secparl/dtaqui/diario_sesiones/acordeon.html

#########################################
## 1. Set elements to construct the URLS
#########################################

setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/")
path <- setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Argentina/Scrapping/docs")
link <- "https://www4.hcdn.gob.ar/dependencias/dtaquigrafos/diarios/periodo-"

years <- as.character(c(2001:2018)) # From 2000 to 2018
date <- seq(ymd("2001-03-01"), ymd("2018-11-30"), by = "day", lenght.out = days)
date <- as.character(date[which(wday(date) %in% 1:5)])

month.day <- substr(date, 5, 10) # Substract month and day
#month.day.full <- rep(month.day, length(years))

# period <- as.character(c("135/135", "136/136"))
out_links <- rep(NA, length(date))

for (i in 1:length(month.day)) {
     period <- ifelse(year(date[i]) == 2001, "119/119",
                    ifelse(year(date[i]) == 2002, "120/120",
                    ifelse(year(date[i]) == 2003, "121/121",
                    ifelse(year(date[i]) == 2004, "122/122",
                    ifelse(year(date[i]) == 2005, "123/123",
                    ifelse(year(date[i]) == 2006, "124/124",
                    ifelse(year(date[i]) == 2007, "125/125", 
                    ifelse(year(date[i]) == 2008, "126/126",
                    ifelse(year(date[i]) == 2009, "127/127",
                    ifelse(year(date[i]) == 2010, "128/128",      
                    ifelse(year(date[i]) == 2011, "129/129",
                    ifelse(year(date[i]) == 2012, "130/130",     
                    ifelse(year(date[i]) == 2013, "131/131",
                    ifelse(year(date[i]) == 2014, "132/132",
                    ifelse(year(date[i]) == 2015, "133/133",
                    ifelse(year(date[i]) == 2016, "133/133", 
                    ifelse(year(date[i]) == 2017, "134/134",
                    ifelse(year(date[i]) == 2018, "135/135", "136/136"))))))))))))))))))
     out_links[i] <- paste(link, period, month.day[i], ".pdf", sep = "")
} 


##########################################################
### 2. Calling the URLs and storing the pdf files     ####
##########################################################

true_links <- as.logical(NA)
# We use the function URL exists 
for (i in 1:length(out_links)) {
  true_links[i] <- url.exists(out_links[i])
} 

url <- out_links[true_links]

# Assign location to store the .pdf files
number <- c(1:length(url)) #number of files that we have
destinations <- as.character(c(rep(NA, length(url)))) #Naming the pdf files

for (i in 1:length(url)) { 
  destinations[i] <- paste0(path, "/", substr(url[i], 72, 80), ".pdf")
} 

# Downloading the files
for (i in 1:length(url)) { 
  download.file(url[i], destinations[i], mode = "wb", timeout = 2000) 
} 





