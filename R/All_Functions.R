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
    if(!dir.exists(KEEP_DIR)){
      dir.create(KEEP_DIR)
    }  
      download.file(url = download_url,
                    destfile = here::here(paste0(KEEP_DIR, download_zip)))
      
      unzipped <- unzip(here::here(paste0(KEEP_DIR, download_zip)), exdir = KEEP_DIR)
      csv_file <- unzipped[grepl("psam", unzipped)]
      out <- read_csv(csv_file)
      
  } else {
    temp <- tempfile(fileext = ".zip")
    download.file(url = download_url,
                  destfile = temp)
    
    unzipped <- unzip(temp, exdir = tempdir())
    csv_file <- unzipped[grepl("psam", unzipped)]
    out <- read_csv(csv_file)
    unlink(temp)
  }
  
  return(out)
  
}





#' @title Person target
#' 
#' @description Creates person target for input to ipf
#' 
#' @param CT character containing name of CT as it appears in ACS data
#' @param OCCP data table/tibble that contains occupational data for target geography
#' @param AGE_SEX data table/tibble that contains ACS data for target geography stratified by sex and age
#' @param RACE data table/tibble that contains ACS data for target geography stratified by race/ethnicity
#' 
#' @details 
#' 
#' @return list containing targets for occupation, age, sex and race for the CT
#' @export
#' 
p_target <- function(CT, OCCP, AGE_SEX, RACE){
  
  # calculate totals stratified by occupation
  acs_occup_reshape <- OCCP %>%
    filter(NAME == CT) %>%
    group_by(variable) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    left_join(acs_subject_var18[,c("name","label")], by = c("variable" = "name")) %>%
    separate(label, into = paste("level",1:6, sep = ""), sep = "!!") %>%
    filter(!(is.na(level5) & is.na(level6))) %>%
    group_by(level5) %>%
    mutate(freq = n()) %>%
    ungroup() %>%
    filter(!(is.na(level6) & freq >1)) %>%
    mutate(label.new = ifelse(freq > 1, level6, level5)) %>%
    dplyr::select("variable","label.new","estimate") %>%
    mutate(code=c(1:10, 10, 11:12, 12, 13:23)) %>%
    group_by(code) %>%
    summarise(estimate = sum(estimate))
  print(acs_occup_reshape)
  
  acs_age_sex_reshape <- AGE_SEX %>%
    filter(NAME == CT) %>%
    group_by(variable) %>%
    summarise(estimate = sum(estimate)) %>%
    ungroup() %>%
    slice(-1) %>%
    mutate(sex = rep(c("male","female"), each = 24), 
           agegroup = rep(c(0,1,1,2,2,2,3,3,3,3,4,4,5,5,6,6,7,7,7,7,8,8,9,9), 2)) %>%
    dplyr::select(-1) %>%
    group_by(sex, agegroup) %>%
    summarise(estimate=sum(estimate)) %>%
    spread(sex, estimate) %>%
    slice(-1)
  print(acs_age_sex_reshape)
  
  acs_race_reshape <- RACE %>% 
    filter(NAME==CT) %>%
    filter(variable %in% c("B03002_003", "B03002_004", "B03002_005", "B03002_006", "B03002_007",
                           "B03002_008", "B03002_009", "B03002_012")) %>%
    group_by(variable) %>%
    summarise(estimate = sum(estimate)) %>%
    mutate(racegroup = c(1:8)) %>%
    ungroup() %>%
    select(estimate, racegroup) %>%
    spread(racegroup, estimate) 
  print(acs_race_reshape)
  
  acs_all_sex <- colSums(acs_age_sex_reshape[,2:3])
  acs_all_agegroup <- rowSums(acs_age_sex_reshape[,2:3])
  acs_all_occup <- rbind(acs_occup_reshape %>% rename("Code" = code), data.frame("Code" = 0, estimate = sum(acs_all_sex) - sum(acs_occup_reshape$estimate)))
  
  #==== personal target =======
  p_targets <- list()
  p_targets$sex <- data.frame(male = acs_all_sex[['male']], female = acs_all_sex[['female']])
  p_targets$sex <- as.tibble(p_targets$sex)
  colnames(p_targets$sex) <- 1:2
  
  p_targets$age <- data.frame(matrix(acs_all_agegroup, nrow = 1))
  p_targets$age <- as.tibble(p_targets$age)
  colnames(p_targets$age) <- 1:23
  
  p_targets$occp <- data.frame(matrix(acs_all_occup$estimate, nrow = 1))
  p_targets$occp <- as.tibble(p_targets$occp)
  colnames(p_targets$occp) <- c(1:23, 0)
  
  p_targets$race <- acs_race_reshape
  
  return(p_targets)
}






#' @title Household target
#' 
#' @description Creates household target for input to ipf
#' 
#' @param CT character containing name of CT as it appears in ACS data
#' @param INCOME data table/tibble that contains household income data for target geography all income is in 2018-inflation-adjusted dollars
#' @param SIZE data table/tibble that contains ACS data for target geography stratified by household size
#' 
#' @details 
#' 
#' @return list containing targets for income and size distribution of households within the CT
#' @export
#' 
h_target <- function(CT, INCOME, SIZE){
  
  acs_size <- SIZE %>% 
    filter(NAME == CT) %>%
    dplyr::select(variable, estimate) %>% 
    spread(., key=variable, value=estimate, fill=NA)
  
  acs_income <- INCOME %>%
    filter(NAME==CT)
  
  acs_income_target <- acs_income$estimate[2:11]/100 * acs_hh_income_sf$estimate[1]
  
  #create hh targets list and add tibble of family size
  h_targets <-list()
  #1-7 represents number of household members, estimates are sums of family and non-family hh
  #7 represents households of 7 or more
  h_targets$hhsize <- tibble(
    "1"=acs_size$non_1,
    "2"=acs_size$fam_2 + acs_size$non_2,
    "3"=acs_size$fam_3 + acs_size$non_3,
    "4"=acs_size$fam_4+acs_size$non_4,
    "5"=acs_size$fam_5+acs_size$non_5,
    "6"=acs_size$fam_6+acs_size$non_6,
    "7"=acs_size$fam_7plus+acs_size$non_7plus
  )
  
  h_targets$hhincome <- tibble(
    "1" = sum(acs_income_target[1:5]),
    "2" = sum(acs_income_target[6:7]),
    "3" = sum(acs_income_target[8:10]),
  )
  
  return(h_targets)
}
