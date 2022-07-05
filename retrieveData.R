#Function to retrieve hits from twitter based on a group of keywords
#related to AI.
#It takes no inputs and returns a table with the data to be used. Additionally
#an extra column is being added to generate the tweet url. The first run from
#testing during development will serve as the base table and will be grown in
#each execution. We need to deal outside of this function with the case where
#we might get the same result.
#code to retrieve data should be run either once a day, maybe twice, depending
#on limits set by APIs on getting data.

retrieveData <- function(){
  library(rtweet)
  library(tidygeocoder)
  library(dplyr)
  
  create_token(
    app = "AIcountryInfo",
    consumer_key = Sys.getenv("consumer_key"),
    consumer_secret = Sys.getenv("consumer_secret"),
    access_token = Sys.getenv("access_token"),
    access_secret = Sys.getenv("access_secret")
  )
  
  query = '-filter:replies "Artificial Inteligence" OR "Machine Learning" OR "Deep Learning" OR "Neural Network" OR "Neural Networks" OR "Data Science"'
  
  aitweets <- search_tweets(q=query, n = 1500, include_rts = F)
  
  #get user related data to understand the location
  usrdata <- users_data(aitweets)
  usrdata <- usrdata[which(!duplicated(usrdata["user_id"])),]
  
  #add tweet data and user data together
  usefulData <- subset(aitweets, select = c("user_id", "status_id", "screen_name", "favorite_count","retweet_count", "hashtags"))
  usefulData <- merge(x = usefulData, y= usrdata[,c("user_id","location")], by = "user_id")
  
  #clean data a little
  usefulData$location <- vapply(usefulData$location, gsub, pattern = "<U.*>", replacement = "", character(1L))
  #na locations will go to Antarctida so we can still display the results
  usefulData[which(usefulData["location"] == ""), "location"] <- "Antarctida"
  
  #set url so it can be indicated on the app and dierct to original post
  usefulData$url <- paste0("twitter.com/", usefulData$screen_name, "/status/", usefulData$status_id)
  
  #remove useless user. may have to add to the list later
  usefulData <- usefulData[which(usefulData["screen_name"]!= "fakeTakeDump"),]
  #unique locations to avoid overusing api calls
  locations <- usefulData %>%
    select(location) %>%
    unique
  
  #get coordinates, set to antartida coords whenever location invalid
  locations <- geocode(locations, address = location)
  locations[which(is.na(locations["lat"])),"lat"] <- -72.8438691
  locations[which(is.na(locations["long"])),"long"] <- 0.0000000
  
  #get country names
  locations <- reverse_geocode(locations, lat = lat, long = long, full_results = TRUE) %>%
    select(all_of(c("location", "lat", "long", "country", "boundingbox")))
  locations[which(is.na(locations["country"])), "country"] <- "Antarctida"
  
  #get country general coordinates
  locations <- geocode(locations, country = country)
  #recover original name and save country name
  colnames(locations)[colnames(locations) %in% c("lat...2", "long...3", "lat...6", "long...7")] <- c("lat", "long", "country_lat", "country_long")
  locations[which(is.na(locations["country_lat"])),"country_lat"] <- -72.8438691
  locations[which(is.na(locations["country_long"])),"country_long"] <- 0.0000000
  
  #map locations into original data 
  usefulData <- merge(x = usefulData, y= locations, by = "location")
  
  
  #unlist hashtag column values so it can be exported
  usefulData$hashtags <- tolower(vapply(usefulData$hashtags, paste, collapse = ",", character(1L)))
  #handle R messups with data types between saving and loading
  usefulData$user_id <- as.character(usefulData$user_id)
  usefulData$status_id <- as.character(usefulData$status_id)
  
  #cleanup unnecessary columns
  colsToKeep <- c("user_id", "status_id", "screen_name", "favorite_count", "retweet_count", "hashtags", "location",
                  "url", "lat", "long", "country","country_lat", "country_long","boundingbox")
  
  usefulData <- usefulData %>%
    select(all_of(colsToKeep))

  usefulData
}