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


#Using the api get the data and parse it from JSON

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
helper_time <- function(code, var_name, year_metadata){
  #code 0 as NA
  if(is.na(code) || code == "0"){
    return(NA)
  }
  #Pull the label from the variable using its value
  label <-
    year_metadata[[var_name]]$values$item[[as.character(code)]]
  
  if(is.null(label)){
    return(NA)
  }
  #Format the time into a usable format by removing to and the fixing p.m.
  times <- strsplit(label, " to ")[[1]]
  times <- gsub("\\.","", times)
  
  start <- parse_date_time(times[1], "I:M p")
  end   <- parse_date_time(times[2], "I:M p")
  #Finding the midpoint of the two times in the label
  midpoint <- start + (end - start)/2
  #Return the midpoint in hour and minute format
  return(hm(format(midpoint, "%H:%M")))
}


#Using the helper time function to make sure it works and
#returns the time in hour:minute format
helper_time("198","JWAP" , all_var_list$variables_21)

#Checking validity of creating factors for the variables
length(all_var_list$variables_24$FER$values$item)
length(all_var_list$variables_24$HHL$values$item)
length(all_var_list$variables_24$SCH$values$item)
length(all_var_list$variables_24$SCHL$values$item)
all_var_list$variables_24$SCHL$values$item

# Create a factor helper function
helper_factor <- function(x, labels){
  factor(
    x,
    levels = names(labels),
    labels = unlist(labels)
  )
}


api_key <- "96f181e4ada2cf8ebea839ffd9fa684508d8fc09"


#create a function to get the data from the api and create a tibble.
get_api_data <- function(
    #default values
    year = 2024,
    num_var = "AGEP",
    cat_var = "SEX",
    geography = "State",
    geo_value = NULL){
  
  # Check year is in the given 4 years
  if(length(year) != 1 ||
     !year %in% c(2021, 2022, 2023, 2024)){
    stop("Year must be 2021, 2022, 2023, or 2024")
  }
  
  # Select metadata year
  if(year == 2021){
    year_metadata <- all_var_list$variables_21
  } else if(year == 2022){
    year_metadata <- all_var_list$variables_22
  } else if(year == 2023){
    year_metadata <- all_var_list$variables_23
  } else {
    year_metadata <- all_var_list$variables_24
  }
  
  # Check numeric variable is one of the set variables
  allowed_num <- c(
    "AGEP",
    "GASP",
    "GRPIP",
    "JWAP",
    "JWDP",
    "JWMNP"
  )
  
  if(length(num_var) != 1){
    stop("Specify exactly one numeric variable")
  }
  
  if(!num_var %in% allowed_num){
    stop("Invalid numeric variable")
  }
  
  # PWGTP always included
  api_vars <- c(num_var, "PWGTP")
  
  # Check categorical variable is from given list
  allowed_cat <- c(
    "FER",
    "HHL",
    "SCH",
    "SCHL",
    "SEX"
  )
  
  if(length(cat_var) != 1){
    stop("Specify exactly one categorical variable")
  }
  
  if(!cat_var %in% allowed_cat){
    stop("Invalid categorical variable")
  }
  #define the available variables to choose from
  api_vars <- c(api_vars, cat_var)
  
  # Check geography is one of the values specified
  allowed_geo <- c(
    "Region",
    "Division",
    "State"
  )
  
  if(length(geography) != 1){
    stop("Specify exactly one geography level")
  }
  
  if(!geography %in% allowed_geo){
    stop("Geography must be Region, Division, or State")
  }
  
  # Default geography value
  if(is.null(geo_value)){
    
    geo_defaults <- c(
      State = "06",
      Division = "9",
      Region = "4"
    )
    
    geo_value <- unname(
      geo_defaults[geography]
    )
    
  }
  
  # Validate geography value
  geo_var <- toupper(geography)
  
  valid_geo <- names(
    year_metadata[[geo_var]]$values$item
  )
  
  if(tolower(geo_value) != "all" &&
     !geo_value %in% valid_geo){
    stop("Invalid geography value")
  }
  
  # Build URL
  if(tolower(geo_value) == "all"){
    
    url <- paste0(
      "https://api.census.gov/data/",
      year,
      "/acs/acs1/pums?get=",
      paste(api_vars, collapse = ","),
      "&for=",
      tolower(geography),
      ":*",
      "&key=",
      api_key
    )
    
  } else {
    
    url <- paste0(
      "https://api.census.gov/data/",
      year,
      "/acs/acs1/pums?get=",
      paste(api_vars, collapse = ","),
      "&for=",
      tolower(geography),
      ":",
      geo_value,
      "&key=",
      api_key
    )
    
  }
  
  response <- GET(url)
  
  parsed <- fromJSON(
    rawToChar(response$content)
  )
  
  census_tbl <- helper(parsed)
  
  # Convert numeric/time variable
  if(num_var %in% c(
    "JWAP",
    "JWDP",
    "JWMNP")){
    
    census_tbl[[num_var]] <- sapply(
      census_tbl[[num_var]],
      helper_time,
      var_name = num_var,
      year_metadata = year_metadata
    )
    
  } else {
    
    census_tbl[[num_var]] <-
      as.numeric(census_tbl[[num_var]])
    
  }
  
  # Convert PWGTP
  census_tbl$PWGTP <-
    as.numeric(census_tbl$PWGTP)
  
  # Convert categorical variable
  labels <- year_metadata[[cat_var]]$values$item
  
  census_tbl[[cat_var]] <-
    helper_factor(
      census_tbl[[cat_var]],
      labels
    )
  
  # Add census class
  class(census_tbl) <-
    c("census", class(census_tbl))
  
  return(census_tbl)
  
}
get_api_data()

# Different categorical variable
get_api_data(cat_var = "SCHL")

get_api_data(num_var = "GRPIP")

get_api_data(num_var="JWAP")
