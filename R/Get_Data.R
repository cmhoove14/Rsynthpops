#' @title Get PUMS
#' 
#' @description Interfaces with ACS PUMS ftp site to download pums data for specified state
#' 
#' @param YEAR survey year to download data. 
#' @param FIVE_YEAR whether to download five year survey data or one year. defaults to five year, set to FALSE for 1 year
#' @param STATE two-letter state abbreviation for state to grab data for
#' @param LEVEL person or household level data download. "p" for person, "h" for household
#' @param KEEP logical of whether to keep raw downloaded zip file, defaults to FALSE
#' @param KEEP_DIR directory at which to store downloaded zip file
#' 
#' @details 
#' 
#' @return 
#' @export
#' 

get_pums <- function(YEAR, 
                     FIVE_YEAR = TRUE, 
                     STATE, 
                     LEVEL, 
                     KEEP = FALSE, 
                     KEEP_DIR){
  
# Prerun checks
  STATE <- tolower(STATE)
  LEVEL <- tolower(LEVEL)
  
  if(!LEVEL %in% c("p", "h")) stop("Level must be either 'p' for person or 'h' for household level data")
  
# Base url and file name  
  base_url     <- paste0("https://www2.census.gov/programs-surveys/acs/data/pums/", YEAR)
  download_zip <- paste0("csv_", LEVEL, STATE, ".zip")
    
# Complete url to download file  
  if(FIVE_YEAR){
    download_url <- paste0(base_url, "/5-Year/", download_zip)
  } else {
    download_url <- paste0(base_url, "/1-Year/", download_zip)
  }

# Download
  if(KEEP){
    if(dir.exists(KEEP_DIR)){
      system(paste0("wget -O ", 
                    KEEP_DIR, download_zip, " " ,
                    download_url)) 
      
      unzipped <- unzip(paste0(KEEP_DIR, download_zip), exdir = KEEP_DIR)
      csv_file <- unzipped[grepl("psam", unzipped)]
      out <- read_csv(csv_file)
      
    } else {
      dir.create(KEEP_DIR)
      system(paste0("wget -O ", 
                    KEEP_DIR, download_zip, " " ,
                    download_url))
      
      unzipped <- unzip(paste0(KEEP_DIR, download_zip), exdir = KEEP_DIR)
      csv_file <- unzipped[grepl("psam", unzipped)]
      out <- read_csv(csv_file)
      
    }
  } else {
    temp <- tempfile(fileext = ".zip")
    system(paste0("wget -O ", 
                  temp, " " ,
                  download_url))
    
    unzipped <- unzip(temp, exdir = tempdir())
    csv_file <- unzipped[grepl("psam", unzipped)]
    out <- read_csv(csv_file)
    unlink(temp)
  }
  
  return(out)
  
}
