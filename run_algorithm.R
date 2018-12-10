library(tidyverse)
library(tidytext)
library(googlesheets)
library(googledrive)
library(reshape2)
library(gmailr)
library(grid)
library(gridExtra)
library(gtable)
library(lubridate)
library(stringi)
library(beepr)

oldw <- getOption("warn")
options(warn = -1)

gst <- gs_ls()
date_pulled <- str_sub(gst$sheet_title[grep("EMSOUTREACH_WKENDING", gst$sheet_title)[1]], -10)

go_ahead <- menu(c("Yes", "No"), title = paste("Is ", date_pulled, " correct?"))

if(go_ahead == 1){
  
  ### Run the data prep
  source("algorithm_prep.R")
  ###
  
  ### Run the algorithm on input data
  source("algorithm.R")
  ###
  
  ### Push updates
  source("push.R")
  ###
  
  ### Compose and send emails
  source("email.R")
  ###
  
  ### Clears workspace
  options(warn = oldw)
  rm(list = ls())  
  ###
  
  print("Finished - all local data has been removed")
  beep(10)
  
} else {
  
  date_pulled <- readline(prompt = "Enter correct date (no quotes): ")
  
  ### Run the data prep
  source("algorithm_prep.R")
  ###
  
  ### Run the algorithm on input data
  source("algorithm.R")
  ###
  
  ### Push updates
  source("push.R")
  ###
  
  ### Compose and send emails
  source("email.R")
  ###
  
  ### Clears workspace
  options(warn = oldw)
  rm(list = ls())  
  ###
  
  print("Finished - all local data has been removed")
  beep(10)
  
}




