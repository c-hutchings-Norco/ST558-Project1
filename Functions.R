#Functions for Census Data Project 1
#Load libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyverse)
library(ggplot2)


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

#Combine all the lists into one list
all_var_list <- list(variables_21 = var_21, 
                     variables_22 = var_22, 
                     variables_23 = var_23, 
                     variables_24 = var_24)

#Save the list as an RDS file in the data folder
saveRDS(all_var_list, "data/variable_list.RDS")


  

api_key="96f181e4ada2cf8ebea839ffd9fa684508d8fc09"
get_API_2021 <- GET("https://api.census.gov/data/2024/acs/acs1/pums?get=SEX,PWGTP,MAR&for=division:*&SCHL=24&key=96f181e4ada2cf8ebea839ffd9fa684508d8fc09")

names(get_API_2021)

parsed_census <- fromJSON(rawToChar(get_API_2021$content))

helper <- function(parsed_data){
  new_tibble <- as_tibble(parsed_data[-1, ])
  names(new_tibble) <- parsed_data[1, ]
  return(new_tibble)
  }
helper(parsed_census)
