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
#   
#   radius (int): radius around coordinate point; API appears to accept fractional distances too
#   unit (chr): "9036" specifies units as kilometers, "9035" for miles
#   endpoint (chr): the endpoint URL
#   maxtries (int): the number of retries to query the API if a request isn't successful.   
#
#   collect_all_records(logical): if TRUE, will collect all supplied records in prisons_file, if FALSE, will only collect first 3 (for testing)
#
#   savefiles(logical): TRUE #option to save text files, FALSE will query API and keep results in a data.frame without saving text files
#   overwrite(logical): if TRUE will overwrite existing files (otherwise throws error), TRUE is useful for testing
#   savesubdirectory(chr): savesubdirectory(of one level only) to save text files to, can comment out if none desired
#   extension(chr): I'm overengineering here! note: full filename is hardcoded based on HIFLD facility ID and date e.g. "10003650-ejscreen-data-queried-2019-11-22.txt"
# 
#   verbose = TRUE

working_directory <- "C:/hack-ca-local/epa-facilities"
prisons_file <- "hifld-prison_boundaries-geocoded-from-shapefiles-by-ucd-team.csv"

radius <- "1"
unit <- "9036"
endpoint <- "https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx"
maxtries <- 3

collect_all_records <- FALSE

savefiles <-  TRUE
overwrite <- FALSE
savesubdirectory <- "testing"
extension <- ".txt"

verbose <- TRUE

################################### - load-libraries
library(httr)
library(tidyverse)


############################################################################ - helper-functions

################################### - ejscreen-api-function
ejscreen_api_call <- function(params, endpoint, maxtries) {
  # """
  # Gets EJSCREEN results for a single point (long/lat coordinates) from https://ejscreen.epa.gov/mapper/ejscreenRESTbroker.aspx
  # 
  #
  # Args:
  #   params (list): A list of API expected parameters, passed to httr::GET().
  #   endpoint (chr): the API endpoing URL.
  #   maxtries (int): the number of retries to query the API if a request isn't successful.
  #
  # Returns:
  #   list(character(),logical(),character())
  #   [['results']] (chr) the API JSON
  #   [['successflag']] (logical) TRUE if expected JSON was retrieved
  #   [['error_message']] (chr) error message if not successful
  #   [['response']] (list response) response object from httr, useful for inspecting errors
  # """
  
  trynum <- 0
  
  response <- NULL
  response_status <- character()
  expected_content <- logical()
  
  while (trynum < maxtries) {
    
    response <- GET(url = endpoint, query = params)

    response_status <- response[['status_code']]
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
  
  results <- list(character(),logical(),character(),character())
  names(results) <- c("results","successflag","error_message","response")
  
  if (expected_content) {
      results[['results']] <- content(response, as="text")
      results[['successflag']] <- TRUE
      results[['error_message']] <- NA
      results[['response']] <- response #store the whole response, useful for errors/diagnostics
      return(results)
  } else {
      results[['results']] <- NA
      results[['successflag']] <- FALSE
      results[['error_message']] <- error_message
      results[['response']] <- response
      return(results)
    }
}

################################### - savetextfile
save_text_file <- function(charstring, filename, extension, overwrite) {
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
  if (dir.exists(savesubdirectory)) {
    setwd(savesubdirectory)
  } else {
    dir.create(savesubdirectory)
    setwd(savesubdirectory)
  }
  
  return(NULL)
}

################################### - collectresults
collect_results <- function(prison_row, radius, unit, savefiles, overwrite, extension, savesubdirectory, verbose, endpoint, maxtries) {
  # """
  # A function for apply(), relying on helper functions above, it structures call parameters, queries api,
  # and saves individual JSON text file, for a single set of coordinates.
  #
  # Args:
  #   prison_row (chr vector): The data.frame row, passed as a named vector and coerced to character.
  #   see user config section at top for all others
  #
  # Returns:
  #   list(character(),logical(),character(),character()), the results; returned from ejscreen_api_call(), see that function for details
  # """  
  
  #I couldn't find any API good citizen guidelines/rate limits, we'll sleep for 1 second
  Sys.sleep(1)
  
  #note
  endpoint <- endpoint
  
  #endpoint <- "https://asdlkajsdflkjaskldjfas.com" #endpoint for testing
  
  lat <- as.character(prison_row["latitude"]) #apply is passing the row as a named vector, likely already character but just in case
  long <- as.character(prison_row["longitude"])
  
  geo_query_string <- paste0('{"spatialReference":{"wkid":4326},"x":',long,',"y":',lat,'}') #reference: '{"spatialReference":{"wkid":4326},"x":-90.43382492108161,"y":32.848323133090894}'
  
  params <- list(
    namestr = "",
    distance = radius,
    areatype = "",
    unit = unit,
    f = "json", #format
    geometry = I(geo_query_string)#wrapping in I() to avoid character conversion eg ' ' into %20, this fixed a bug where API was returning error
    #note the hardcoded spatialReference in geo_query_string above, 4326 is standard for long/lat
  )
  
  tryCatch( 
    expr = {
      
      response <- ejscreen_api_call(params = params, endpoint = endpoint, maxtries = maxtries)
      
      if (nchar(savesubdirectory) > 1) { #did user specify a savesubdirectory?
        enter_directory(savesubdirectory)
      }
      
      datequeried <- Sys.Date()
      filename <- paste0(prison_row["FACILITYID"],'-ejscreen-data-queried-',datequeried) #save_text_file will append user config'd extension
      
      save_text_file(response[['results']], filename = filename, extension = extension, overwrite = overwrite)
      
      return(response) #list of 'results' char, 'successflag' logical, 'error_message', 'request_url'
      
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
      results <- list(character(),logical(),character(),character())
      names(results) <- c("results","successflag","error_message","response")
      results[['results']] <- NA
      results[['successflag']] <- FALSE
      results[['error_message']] <- e
      results[['response']] <- NA
      
      return(results)
    },
    finally = {
      #are we in savesubdirectory?
      if (basename(getwd()) == savesubdirectory) {
        #move back up to our original working directory
        setwd('..')
      }
    }
  )
}

############################################################################ - read-in-prisons-and-collect-data
#read in prisons data from config
prisons <- read_csv(prisons_file)

#set number of records to collect
numrecords <- int()
if (collect_all_records) {
  numrecords <- 1:nrow(prisons)
} else {
  numrecords <- 1:3
}

#collect results!
time_estimate = max(numrecords) * 2.1 / 60^2 #calculate seconds to hours

message("Starting to collect ", max(numrecords)," records with an estimated time of ", time_estimate, " hours to complete.")
run_query <- apply(prisons[numrecords,],
                   MARGIN = 1,
                   
                   FUN = collect_results, #the function doin' the work
                   
                   radius = radius, #user config'd
                   unit = unit,
                   savefiles = savefiles,
                   overwrite = overwrite,
                   extension = extension,
                   savesubdirectory = savesubdirectory, 
                   verbose = verbose,
                   endpoint = endpoint,
                   maxtries = maxtries
                   )

#put results in a data.frame
run_query <- as.data.frame(do.call(rbind, run_query))

#check for errors
message("Data collection complete.")
Sys.sleep(2)
message("Now checking for errors...")
Sys.sleep(2)
successvector <- as.logical(run_query$successflag) #need convert df list column of logicals into logical vector 
message("There were ",sum(!successvector)," errors out of ",nrow(prisons[numrecords,])," records.") #error count
Sys.sleep(3)
message("The error rate was ",1 - mean(successvector),".") #error rate
Sys.sleep(2)
message("The 'run_query' has a success flag column indicating which records errored.") #error rate

#save all results to one file in a JSON array, later fromJSON will output a nice dataframe vs having to read individual files and rbind etc.
one_big_string <- do.call(paste, c(run_query['results'], collapse = ","))
one_big_string <- paste0('[\n',one_big_string,'\n]')

#save file  
if (nchar(savesubdirectory) > 0) {
  enter_directory(savesubdirectory)
}
datequeried <- Sys.Date()
filename <- paste0('ALL-FACILITIES-ONE-FILE-ejscreen-data-queried-',datequeried)
save_text_file(one_big_string, filename = filename, extension = extension, overwrite = overwrite)
#are we in savesubdirectory?
if (basename(getwd()) == savesubdirectory) {
        #move back up to our original working directory
        setwd('..')
}