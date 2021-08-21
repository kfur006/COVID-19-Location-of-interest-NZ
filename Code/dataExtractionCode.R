# Git setup----
## This is to set up access to git by PAT
#library(gitcreds)
#gitcreds_set()

# Library ----
library(tidyverse)
library(rvest)
library(lubridate)
library(googleLanguageR)
#library(ggmap) # for geoinfo

# Data Extraction
html <- read_html("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-health-advice-public/contact-tracing-covid-19/covid-19-contact-tracing-locations-interest")

list <- html %>% 
  html_elements(".table-style-two") %>% 
  html_table()

# Unlist the list and convert to dataframe
# Step 1: list to tibble with simple cleaning
locationData <- plyr::ldply(list, data.frame) %>% 
  tibble() %>% 
  mutate(Day = as.Date(Day, format = "%A %d %B"),
         Times = gsub('[.]', ':', Times),
         Date.added = as.Date(Date.added, format = "%d-%b"))

# Step 2: Time cleaning (Fix missing dash)
locationData <- locationData %>% 
  mutate(Times = ifelse(is.na(str_locate(pattern ='-', Times)[,1]), gsub('m ', 'm - ', Times), Times)) #Fixing missing dash
  

# Step 3: Time Cleaning (separate start/end)
locationData <- locationData %>%
  separate(., col = Times, into = c("Start", "End"), sep = " - ")

# Step 4: Time Cleaning (Fix missing am/pm)
locationData <- locationData %>%
  mutate(Start = ifelse(grepl(pattern ='[am|pm]', Start) == FALSE, paste0(Start, stringi::stri_sub(End,-2,-1)), Start),
         End = ifelse(grepl(pattern ='[am|pm]', End) == FALSE, paste0(Start, stringi::stri_sub(Start,-2,-1)), End)) %>% 
  mutate(Start = toupper(trimws(gsub(' ', '', Start))),
         End = toupper(trimws(gsub(' ', '', End))))

# Step 5: Time Cleaning (Separate AM/PM)
locationData <- locationData %>%
  mutate(StartAMPM = stringi::stri_sub(Start,-2,-1),
         EndAMPM = stringi::stri_sub(End,-2,-1)) %>% 
  mutate(Start = gsub('[AM|PM]', '', Start),
         End = gsub('[AM|PM]', '', End)) %>% 
  mutate(StartDay = parse_date_time(paste(Day, paste0(Start, ":00"), StartAMPM), "%Y-%m-%d %H:%M:%S %p"),
         EndDay = parse_date_time(paste(Day, paste0(End, ":00"), EndAMPM), "%Y-%m-%d %H:%M:%S %p"))



gl_translate(locationData$Location.name[1], target = "ja")$translatedText
text <- "to administer medicince to animals is frequently a very difficult matter, and yet sometimes it's necessary to do so"
## translate British into Danish
gl_translate(text, target = "da")$translatedText


#geocode(locationData$Location.name[1]) #This requires key from Google

# Use this for Lattitude and Longitude
locationData <- read_csv("https://github.com/minhealthnz/nz-covid-data/raw/main/locations-of-interest/august-2021/locations-of-interest.csv")
