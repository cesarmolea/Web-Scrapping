######################################
##        Web-Scrapping          #####
######################################

# This code performs Web-scrapping of Congressional Sessions in Panama

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
if (!require("httr")) install.packages("httr"); library("httr")
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")
if (!require("foreach")) install.packages("foreach"); library("foreach")
if (!require("iterators")) install.packages("iterators"); library("iterators")
if (!require("parallel")) install.packages("parallel"); library("parallel")


#########################################
## 1. Set elements to construct the URLS
#########################################

setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Panama")
path <- setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Panama")
link <- "https://www.asamblea.gob.pa/APPS/LEGISPAN/PDF_ACTAS/2010_ACTAS/" # From 2010 to 2019

actas <- as.character(c(2010:2019))

date <- seq(ymd("2010-01-01"), ymd("2019-03-30"), by = "days")
date_string <- as.character(date[which(wday(date) %in% 1:5)])
date_string <- str_replace_all(as.character(date), pattern =  "-", replacement = "_") # we replace the - for underscore

out_links <- as.character(rep(NA, length(date_string)))

for (i in 1:length(date_string)) {
  yr <- substr(date_string[i], 1, 4)
  all_days_year <- seq(ymd(paste0(yr,"-01-01")), ymd(paste0(yr, "-12-31")), by = "days")
  all_days_year_string <- str_replace_all(as.character(all_days_year), pattern =  "-", replacement = "_")
    if (yr == 2010) {
    mid <- paste0(yr,"_ACTAS/", yr, "_ACTAS_PLENO/")
    }
      else {
      mid = paste0(yr,"_ACTAS/", yr, "_ACTAS_PLENO/")
      }
        out_links[i] <- paste(link, mid, date_string[i], "_A_PLENO.pdf", sep = "")
        } 

##########################################################
### 2. Calling the URLs and storing the pdf files     ####
##########################################################

# In this case, the function url.exists is not working. Instead, I use the 
#  GET function from the httr package. This function also allows to identify
#  whether a link exists or not. However, we need to make a few more additional
#  steps to get the true links and it is also a little bit more demanding.

outlist <- rep(list(list()), length(out_links))

# We check every URL to see if it exists
for (i in 1:length(outlist)) {
  library(httr)
  outlist[[i]] <- GET(out_links[i], user_agent("httr"))
    }

# We create an empty vector to store the URLs that do exist
true_links <- rep(as.logical(NA), length(outlist))

# We know that if the URL has status code of 200 it does exist
for (i in 1:length(outlist)) {
  if (outlist[[i]]$status_code == 200) {
  true_links[i] <- outlist[[i]]$url
  }
}

# And then we simply extract the true URLs from the vector
url <- true_links[!is.na(true_links)]
  
##########################################################
### 2.1 Storing the pdf files                         ####
##########################################################

# Assign location to store the .pdf files
number <- c(1:length(url)) #number of files that we have
destinations <- as.character(c(rep(NA, length(url)))) #Naming the pdf files

for (i in 1:length(url)) { 
  destinations[i] <- paste0(path, "/PN_", number[i], ".pdf")
} 

# Downloading the files
for (i in 1:length(url)) { 
  download.file(url[i], destinations[i], mode = "wb", timeout = 2000) 
} 

#############################################
### 3. PARSING THROUGH THE PDF FILES     ####
#############################################

# We read the .pdf files
list.files() # to see the subfolders that we have
files <- dir(path, pattern = ".pdf")

# We use the do parallel command to make it quicker, since we have tons of files
usecores <- detectCores() -1
cl <- makeCluster(usecores)
registerDoParallel(cl)

# We sparse the text of each file
#  We make a list of lists. In this case, each list corresponds to one session (one document). Inside each list, there
#  are multiple number of lists corresponding to the number of pages from that session.
outlist_new <- rep(list(list()), length(url))

outlist_new <- foreach (i = 1:length(files)) %dopar%  { 
  library(pdftools)
  library(magrittr)
  pdf_text(files[i]) %>% 
    strsplit(split = "\n")
}

full.w <- foreach (i = 1:length(outlist_new)) %dopar% {
  outlist_new[[i]][grepl("Banco Interamericano", outlist_new[[i]], ignore.case = T)]
}

words <- unlist(full.w)
write.csv(words, file = "BID_PN.csv")

stopCluster(cl) # we need to stop it

