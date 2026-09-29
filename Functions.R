#Functions for Census Data Project 1
#Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lubridate)


#Part 1 Functions

#Create a function to get all the variables
fun_var_list <- function(year) {

# Exclude any years outside of the following    
  if (!year %in% c(2021, 2022, 2023, 2024)) {
    stop("Year must be 2021, 2022, 2023, or 2024.")
  }

# Use the API to get the data, use paste to 
#add the year as a variable determined by the function    
  response <- GET(
    paste0("https://api.census.gov/data/",
      year,
      "/acs/acs1/pums/variables.json"))
  
# Parse the data from JSON  
  parse_var <- fromJSON(rawToChar(response$content))
 
# Pull out the variables required.  Choose the names depending 
#on the year requested because they change for certain years.  
   
  if (year %in% c(2021, 2022)) {
    
    required_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", 
                       "JWDP", "JWMNP", "PWGTP", "FER", 
                       "HHL", "SCH","SCHL", "SEX", "ST",
                       "REGION", "DIVISION")
    
  } else {
    
    required_vars <- c("AGEP", "GASP", "GRPIP", "JWAP", 
                       "JWDP","JWMNP", "PWGTP", "FER", 
                       "HHL", "SCH", "SCHL", "SEX", "STATE",
                       "REGION", "DIVISION")
  }

#create a list that includes only the required variables.    
  var_list <- parse_var$variables[required_vars]

#return the list  
  return(var_list)
}

#Run the function for each year and create a list
var_21 <- fun_var_list(2021)
var_22 <- fun_var_list(2022)
var_23 <- fun_var_list(2023)
var_24 <- fun_var_list(2024)
#Try a year out of range, should produce an error.
#var_25 <- fun_var_list(2025)

#Combine all the lists into one list
all_var_list <- list(variables_21 = var_21, 
                     variables_22 = var_22, 
                     variables_23 = var_23, 
                     variables_24 = var_24)

#Save the list as an RDS file in the data folder
saveRDS(all_var_list, "data/variable_list.RDS")


  

api_key="96f181e4ada2cf8ebea839ffd9fa684508d8fc09"
get_API_2021 <- GET("https://api.census.gov/data/2024/acs/acs1/pums?get=SEX,PWGTP,JWAP,MAR&for=division:*&SCHL=24&key=96f181e4ada2cf8ebea839ffd9fa684508d8fc09")

names(get_API_2021)

parsed_census <- fromJSON(rawToChar(get_API_2021$content))

#Helper function to create a tibble from parsed data
helper <- function(parsed_data){
  new_tibble <- as_tibble(parsed_data[-1, ], 
                          .name_repair='minimal')
  names(new_tibble) <- parsed_data[1, ]
  return(new_tibble)
}
#Use helper function to create a tibble for the parsed census data
helper(parsed_census)


#loading the all variables object
all_var_list <- readRDS("data/variable_list.RDS")


#Create the date helper function that finds
#the midpoint of the time in the given
#time interval
helper_date <- function(code, metadata){
  if(is.na(code) || code =='0'){
    return (NA)
  }
  
  label <- metadata[[as.character(code)]]
  
  if(is.null(label)){
    return(NA)
  }
  times <- strsplit(label, "to")[[1]]
  times <- gsub("\\.","", times)
  
  start_time <- parse_date_time(times[1], orders="I:M p")
  end_time <- parse_date_time(times[2], orders="I:M p")
  
  midpoint <- start_time+(end_time-start_time)/2

  return(midpoint)
}

jwap_metadata <- all_var_list$variables_24$JWAP$values$item

helper_date("198", jwap_metadata)


#Seeing why the function didn't work
jwap_metadata[["198"]]


label <- jwap_metadata[["198"]]

times <- strsplit(label, " to ")[[1]]

times

times <- gsub("\\.", "", times)

times

start <- parse_date_time(times[1], "I:M p")
end <- parse_date_time(times[2], "I:M p")

start
end

helper_num <- function(num){
  if num1=blank
  then num1 = 'AGEP'
  else
    num1 %in% c(GASP, GRPIP, JWAP, JWDP, JWMNP)
}

helper_cat <- function(category){
  if category = ''
  then 'SEX'
  else 
    category %in% c( FER, HHL, SCH, SCHL)
  if category == SEX
  as.factor(SEX),
  if category == FER
}

helper_geo <- function(geography){}

  
census_api <- function(year, num1, category, geog){
  # Exclude any years outside of the following    
  if (!year %in% c(2021, 2022, 2023, 2024)) {
    stop("Year must be 2021, 2022, 2023, or 2024.")
  }
  
  
  
}

