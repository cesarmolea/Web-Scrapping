######################################
##        Web Scrapping          #####
######################################

# This code performs web-scrapping of the Sessions from the Congress of Guatemala
# Updated script with Parallel computing

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
if (!require("doParallel")) install.packages("doParallel"); library("doParallel")
if (!require("foreach")) install.packages("foreach"); library("foreach")
if (!require("iterators")) install.packages("iterators"); library("iterators")
if (!require("parallel")) install.packages("parallel"); library("parallel")


######################################
## 1. Defining the URLS          #####
######################################

setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Guatemala")
path <- setwd("C:/Users/CESARMO/OneDrive - Inter-American Development Bank Group/ML course/NLP/Guatemala")
url <- "https://www.congreso.gob.gt/consulta-legislativa/diario-de-sesiones/page/2/?categoria=1&fecha_i=2013-05-01&fecha_f=2019-05-28"

links <- function(url) { # function that we create to extract the links with the pdfs
  html <- read_html(url)
  results <- html %>% html_nodes(".files-download") %>%
  html_node("a") %>% 
  html_attr("href") 
  return(results)
  }

pages <- c(1:20) # Number of pages we have
pdfs <- rep(list(list()), length(pages))

first <- "https://www.congreso.gob.gt/consulta-legislativa/diario-de-sesiones/page/"
second <- "/?categoria=1&fecha_i=2013-05-01&fecha_f=2019-05-28"

out_links <- rep(NA, length(pages))

for (i in 1:length(pages)) {
  out_links[i] <- paste0(first, pages[i], second)
  }

for (i in 1:20) {
  pdfs[[i]] <- links(out_links[i])
  }

#############################################
### 2. DOWNLOAD THE pdf FILES            ####
#############################################

number <- c(1:(length(pdfs)*lengths(pdfs[1]))) #number of files that we have
destinations <- as.character(c(rep(NA, length(number)))) #Naming the pdf files

for (i in 1:length(number)) { 
  destinations[i] <- paste0(path, "/GT_", number[i], ".pdf")
} 

url <- unlist(pdfs) # we get rid of the list

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
outlist <- rep(list(list()), length(url))

outlist <- foreach (i = 1:length(files)) %dopar%  { 
    library(pdftools)
    library(magrittr)
    pdf_text(files[i]) %>% 
    strsplit(split = "\n")
}

full.w <- foreach (i = 1:length(outlist)) %dopar% {
   outlist[[i]][grepl("Banco Interamericano", outlist[[i]], ignore.case = T)]
   }

words <- unlist(full.w)
write.csv(words, file = "BID_GT.csv")

stopCluster(cl) # we need to stop it


