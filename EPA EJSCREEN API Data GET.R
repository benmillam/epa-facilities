#---
#title: EPA EJSCREEN API Get Data Save JSON
#author: Ben Millam
#date: November 22, 2019
#description: takes geo coordinates and queries EPA EJSCREEN API for the EPA's set of 'environmental justice' data values
#   for a point and a specified radius; saves the results as individual JSON text files and for convenience as one single 
#   text file (a JSON array). Assumes inputs coming from 'hifld-prison_boundaries-geocoded-from-shapefiles-by-ucd-team.csv' via corresponding
#   R script "HFILD Geocode Prisons for Lat Long.R"
#references:
#   error handling resource: http://mazamascience.com/WorkingWithData/?p=912
#   API url structure for reference: https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx?namestr=&geometry={"spatialReference":{"wkid":4326},"x":-90.43382492108161,"y":32.848323133090894}&distance=1&unit=9035&areatype=&areaid=&f=pjson
#notes: see first 'setup' section for config variables
#---


############################################################################ - setup

################################### - user-config:
#   working_directory (chr)
#   prisons_file (chr): Assumes 'hifld-prison_boundaries-geocoded-from-shapefiles-by-ucd-team.csv' produced by corresponding R script "HFILD Geocode Prisons for Lat Long.R"
#   savetodirectory(chr): subdirectory to save text files to, can comment out if none desired
#   collect_all_records(logical): if TRUE, will collect all supplied records in prisons_file, if FALSE, will only collect first 3 (for testing)
#
#   note that several API parameters like radius are hard coded in collect_results()

working_directory <- "C:/hack-ca-local/epa-facilities"
prisons_file <- "hifld-prison_boundaries-geocoded-from-shapefiles-by-ucd-team.csv"
savetodirectory <- "testing" # asdk;ljfasdk;llllllllllj NEED FILL THESE OUT, ALSO NEED ADD PRINT MESSAGES FOR STATUS LIKE ERROR REPORTING, ALSO ADD CONFIGS FOR OTHER API PARAMS!
collect_all_records <- TRUE #

################################### - load-libraries
library(httr)
library(tidyverse)


############################################################################ - helper-functions

################################### - ejscreen-api-function
ejscreen_api_call <- function(params, endpoint) {
  # """
  # Gets EJSCREEN results for a single point (long/lat coordinates) from https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx
  # 
  #
  # Args:
  #   params (list): A list of API expected parameters, passed to httr::GET().
  #   endpoint (chr): the API endpoing URL.
  # 
  # Returns:
  #   list(character(),logical(),character())
  #   [['results']] (chr) the API JSON
  #   [['successflag']] (logical) TRUE if expected JSON was retrieved
  #   [['error_message']] (chr) error message if not successful
  # """
  
  maxtries <- 3
  trynum <- 1
  
  response_status <- character()
  expected_content <- logical()
  
  while (trynum < maxtries + 1) {
    
    response <- GET(url = endpoint, query = params)

    response_status <- response$status_code
    expected_content <- grepl("RAW_E_PM25", x = content(response, as="text")) #check that an expected JSON key is present, we'll handle error catching outside of the api call function
    if (expected_content) {
      break
    } else {
      error_message <- paste("Expected JSON key RAW_E_PM25 not found in response content of attempt",trynum,"of",maxtries,". Response code",response_status,"content:",content(response, as="text"))
      cat(error_message)
      Sys.sleep(1)
      trynum <- trynum + 1
    }
  }
  
  results <- list(character(),logical(),character())
  names(results) <- c("results","successflag","error_message")
  
  if (expected_content) {
      results[['results']] <- content(response, as="text")
      results[['successflag']] <- TRUE
      results[['error_message']] <- NA
      return(results)
  } else {
      results[['results']] <- NA
      results[['successflag']] <- FALSE
      results[['error_message']] <- error_message
      return(results)
    }
}

################################### - savetextfile
save_text_file <- function(charstring, filename, extension = ".txt", overwrite = FALSE) {
  # """
  # Saves a text file in UTF-8 encoding, prevents overwrite if desired.
  # 
  #
  # Args:
  #   charstring (coerced to chr): Content to save.
  #   filename (chr)
  #   extension (chr)
  #   overwrite (logical) if FALSE, will error if file already exists
  # 
  # Returns:
  #   NULL
  # """
  
  filename <- paste0(filename, extension)
  
  current_encoding <- getOption("encoding")
  options("encoding" = "UTF-8")
  
  if(!overwrite) {
    if (file.exists(filename)) {
      error_message <- paste("Overwrite error; this file already exists:", filename)
      stop(error_message)
    }
  }
  
  write(charstring, file = filename)
  
  options("encoding" = current_encoding)
  
  return(NULL)
}

################################### - enterdirectory
enter_directory <- function(dirname) {
  # """
  # Enters a directory, if doesn't exist => creates it first.
  # 
  #
  # Args:
  #   dirname (chr): The directory name.
  # 
  # Returns:
  #   NULL
  # """
  if (dir.exists(savetodirectory)) {
    setwd(savetodirectory)
  } else {
    dir.create(savetodirectory)
    setwd(savetodirectory)
  }
  
  return(NULL)
}

################################### - collectresults
collect_results <- function(prison_row, savefiles = T, subdirectory = character(), verbose = FALSE) {
  # """
  # A function for apply(), relying on helper functions above, it structures call parameters, queries api,
  # and saves individual JSON text file, for a single set of coordinates.
  #
  # Args:
  #   prison_row (chr vector): The data.frame row, passed as a named vector and coerced to character.
  #   savefiles (logical): if TRUE will save text files; FALSE useful if just want results into memory.
  #   subdirectory (chr): subdirectory for saving files, optional, if not specified will save to current working directory
  #   verbose (logical): if TRUE, will print error messages to console.
  #
  # Returns:
  #   list(character(),logical(),character()), the results; returned from ejscreen_api_call(), see that function for details
  # """  
  
  #I couldn't find any API good citizen guidelines/rate limits, we'll sleep for 1 second
  Sys.sleep(1)
  
  #note
  endpoint <- "https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx"
  
  #endpoint <- "https://asdlkajsdflkjaskldjfas.com" #endpoint for testing
  
  lat <- as.character(prison_row["latitude"]) #apply is passing the row as a named vector, likely already character but just in case
  long <- as.character(prison_row["longitude"])
  
  geo_query_string <- paste0('{"spatialReference":{"wkid":4326},"x":',long,',"y":',lat,'}') #reference: '{"spatialReference":{"wkid":4326},"x":-90.43382492108161,"y":32.848323133090894}'
  
  params <- list(
    namestr = "",
    distance = "1", #API appears to accept fractional distances too
    areatype = "",
    unit = "9036", #specifies units as kilometers, "9035" is miles
    f = "json", #format
    geometry = I(geo_query_string)#wrapping in I() to avoid character conversion eg ' ' into %20, this fixed a bug where API was returning error
    #note the hardcoded spatialReference in geo_query_string above, 4326 is standard for long/lat
  )
  
  tryCatch( 
    expr = {
      
      response <- ejscreen_api_call(params, endpoint)
      
      save_to_subdirectory <- length(subdirectory) != 0 #character() length == 0
      
      if (save_to_subdirectory) {
        enter_directory(subdirectory)
      }
      
      datequeried <- Sys.Date()
      filename <- paste0(prison_row["FACILITYID"],'-ejscreen-data-queried-',datequeried) #save_text_file will append '.txt'
      
      save_text_file(response[['results']], filename = filename, extension = ".txt")
      
      return(response) #list of 'results' char and 'successflag' logical
      
    },
    warning = function(w) {
      if (verbose) {
        message(w)
      }
    },
    error = function(e) {
      if (verbose) {
        message(e)
      }
      results <- list(character(),logical(),character())
      names(results) <- c("results","successflag","error_message")
      results[['results']] <- NA
      results[['successflag']] <- FALSE
      results[['error_message']] <- e
      
      return(results)
    },
    finally = {
      #are we in subdirectory?
      if (basename(getwd()) == subdirectory) {
        #move back up to our original working directory
        setwd('..')
      }
    }
  )
}


############################################################################ - read-in-prisons-and-collect-data
#read in prisons data from config
prisons <- read_csv(prisons_file)

#set subdir from config if applicable
subdirectory = character()
if (savetodirectory) {
  subdirectory <- savetodirectory
}

#set number of records to collect
if (collect_all_records) {
  numrecords <- 1:nrow(prisons)
} else {
  numrecords <- 1:3
}

#collect results!
run_query <- apply(prisons[numrecords,], MARGIN = 1, FUN = collect_results, savefiles = T, subdirectory = subdirectory, verbose = TRUE)

#put them in a data.frame
run_query <- as.data.frame(do.call(rbind, run_query))

#check for errors
successvector <- do.call(run_query$successflag,paste)
sum(!successvector) #error count
1 - mean(successvector) #error rate

#save all results to one file in a JSON array, later fromJSON will output a nice dataframe vs having to read individual files and rbind etc.
one_big_string <- do.call(paste, c(run_query['results'], collapse = ","))
one_big_string <- paste0('[\n',one_big_string,'\n]')

#save file  
if (length(savetodirectory) != 0) {
  enter_directory(savetodirectory)
}
datequeried <- Sys.Date()
filename <- paste0('ALL-FACILITIES-ONE-FILE-ejscreen-data-queried-',datequeried)
save_text_file(one_big_string, filename = filename, extension = ".txt", overwrite = FALSE) #overwrite useful for quick testing
#are we in subdirectory?
if (basename(getwd()) == savetodirectory) {
        #move back up to our original working directory
        setwd('..')
}