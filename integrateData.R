#Function to load existing data if already run at least once and add a new query
#result to that file. Otherwise just get a new query. A file is then either
#created or overwritten.
#The function then returns the integrated data to be used by the shiny app.

source("retrieveData.R")
library(dplyr)

integrateData <- function(){
  
  #first table exists, no need to test for existing.
  
  #get new batch of results
  retrieved_data <- retrieveData()
  
  #read file
  datapath <- Sys.getenv("retrieved_data_path")
  currentData <- readRDS(datapath)
  
  
  #get column names in db
  colnames_data <- colnames(currentData)
  
  #find missing columns available in the DB and add as NA
  missingColList <- colnames_data[which(!colnames_data %in% colnames(retrieved_data))]
  retrieved_data[,missingColList] <- NA
  
  #subset data needed
  finalColList <- colnames(retrieved_data)[which(colnames(retrieved_data) %in% colnames_data)]
  retrieved_data <- select(retrieved_data, all_of(finalColList))
  
  
  #remove possible duplicates when adding to data 
  retrieved_data <- retrieved_data[which(!duplicated(retrieved_data["status_id"])),] %>%
    as.data.frame
  
  currentData <- rbind(currentData, retrieved_data)
  
  saveRDS(currentData, datapath)
}