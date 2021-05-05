#---------------------------------------------
# Rsynthpops get data functions 
# Chris Hoover April 2021 
#---------------------------------------------

#' @title Get group quarters target data
#' 
#' @description Interfaces with census api to download census summary file f1 and extract group quarters data. Currently fetches data from the 2010 census, will be updated for 2020 census when data is made available
#' 
#' @param STATES Single state or character vector of states (two letter abbreviations) to download data
#' @param LEVEL Level to return data for, options are "State", "County", "Tract"
#' 
#' @details See <https://gl-li.netlify.app/2017/08/29/process-2010-census-data-with-data-table/> for useful information on processing raw census supplement data. Each returned data frame in the list of states will contain data frame with id record from geography census file, the state, county, and tract fips code (as relevant per input LEVEL), and then 63 columns corresponding to variables in table P43 (see https://www.census.gov/prod/cen2010/doc/sf1.pdf page 6-55)
#' 
#' @return list containing data frame of group quarters data for each state at the indicated level. 
#' @export
#' 

rsp_get_gq <- function(STATES, LEVEL){
  
  if(!LEVEL %in% c("State", "County", "Tract")){
    stop("LEVEL not recognized")
  }
  
  STATES <- STATES[order(STATES)]
  
  state_names <- state.name[which(state.abb %in% STATES)]
  
  state_urls <- paste0("https://www2.census.gov/census_2010/04-Summary_File_1/Urban_Rural_Update/",
                       state_names[order(state_names)], "/",
                       tolower(STATES), "2010.ur1.zip")
  
  state_dats <- lapply(1:length(STATES), function(s){
    tmp <- tempfile()
    download.file(url = state_urls[s], 
                  destfile = tmp,
                  method = "curl")
    # Geography guide file
    dat1 <- read.table(unz(tmp, paste0(tolower(STATES[s]), "geo2010.ur1")),
                       sep = "\n",
                       header = FALSE)
    # Data file 6 with Gq data
    dat6 <- read.table(unz(tmp, paste0(tolower(STATES[s]), "000062010.ur1")),
                       sep = ",")
    unlink(tmp)  
      
    dat1_parse <- dat1 %>% 
      mutate(
        LOGRECNO  = as.numeric(substr(V1, 19, 25)),
        SUMLEV    = substr(V1, 9, 11),
        ST_fips   = substr(V1, 28, 29),
        Cnty_fips = substr(V1, 30, 32),
        Ct_fips   = substr(V1, 55, 60),
        GEOID     = paste0(ST_fips, Cnty_fips, Ct_fips),
        PUMA      = substr(V1, 478, 482)
      )
    
    if(LEVEL == "Tract"){
      
      dat1_fin <- dat1_parse %>% 
        filter(SUMLEV == "140") %>% 
        dplyr::select(c("LOGRECNO", "GEOID", "ST_fips", "Cnty_fips", "Ct_fips"))
      
    } else if(LEVEL == "County"){
      
      dat1_fin <- dat1_parse %>% 
        filter(SUMLEV == "050") %>% 
        dplyr::select(c("LOGRECNO", "GEOID", "ST_fips", "Cnty_fips", "Ct_fips"))
      
    } else if(LEVEL == "State"){
      
      dat1_fin <- dat1_parse %>% 
        filter(SUMLEV == "040") %>% 
        dplyr::select(c("LOGRECNO", "GEOID", "ST_fips", "Cnty_fips", "Ct_fips"))
      
    } else {
      stop("LEVEL not recognized")
    }
    
    # Columns corresponding to LOGRECNO and table P43 containing group quarters data
    dat6_parse <- dat6 %>% 
      filter(V5 %in% dat1_fin$LOGRECNO) %>% 
      dplyr::select(c(5, 179:241))
    
      P04300_nums <- c(1:63)
      P04300_cols <- paste0("P04300", if_else(nchar(P04300_nums) == 1, 
                                              paste0("0", P04300_nums), 
                                              as.character(P04300_nums)))
      colnames(dat6_parse) <- c("LOGRECNO", P04300_cols)
      
    dat_fin <- dat1_fin %>% 
      left_join(dat6_parse, by = "LOGRECNO")   
      
    return(dat_fin)
  })
  
  names(state_dats) <- STATES
  
  return(state_dats)
}







#' @title Get pums data
#' 
#' @description Get PUMS data for desired variables
#' 
#' @param VARS character vector containing desired characteristics of agents. See details for currently supported characteristics.
#' @param SURVEY Survey type to get data from. "acs1" or "acs5"
#' @param STATES two letter state abbreviation or character vector containing multiple
#' @param YEAR year from which data is desired
#' 
#' @details See included data file `pums_to_acs` for currently supported characteristics and Tutorial/Add_New_Characteristics for guide on adding additional characteristics  
#' 
#' @return 
#' @export
#' 

rsp_get_pums <- function(VARS, SURVEY = "acs5", STATES, YEAR){
  # Get Pums data
  pums_vars <- unlist(lapply(VARS, function(var){
    pums_to_acs[[var]][["PUMS"]]
  }))
  
  pums_vars <- c("PUMA", pums_vars)
  
  pums_dat <- tidycensus::get_pums(
    variables = pums_vars,
    state     = STATES,
    survey    = SURVEY,
    year      = YEAR
  )
  
  return(pums_dat)
}






#' @title Get pums data
#' 
#' @description Get ACS data for desired variables
#' 
#' @param VARS character vector containing desired characteristics of agents. See details for currently supported characteristics.
#' @param SURVEY Survey type to get data from. "acs1" or "acs5"
#' @param STATES two letter state abbreviation or character vector containing multiple
#' @param LEVEL Level to return data for, options are "State", "County", "Tract"
#' @param YEAR year from which data is desired
#' 
#' @details See included data file `pums_to_acs` for currently supported characteristics and Tutorial/Add_New_Characteristics for guide on adding additional characteristics 
#' 
#' @return 
#' @export
#' 

rsp_get_acs <- function(VARS, SURVEY = "acs5", STATES, LEVEL, YEAR){
  # Get acs data
  acs_tabs <- unlist(lapply(VARS, function(var){
    pums_to_acs[[var]][["ACS"]]
  }))
  
  acs_tab_dups <- duplicated(acs_tabs)
  
  list_names <- sapply(1:length(acs_tabs), function(n){
    tab_num <- sum(acs_tabs == acs_tabs[n])
    if(tab_num == 1){
      out <- VARS[n]
    } else {
      out <- str_c(VARS[which(acs_tabs == acs_tabs[n])], collapse = "_")
    }
    
    return(out)
  })
  
  acs_tabs_fin <- acs_tabs[!acs_tab_dups]
  
  acs_names_fin <- list_names[!acs_tab_dups]
  
  acs_dat <- lapply(acs_tabs_fin, function(t){
    get_acs(
      geography = tolower(LEVEL), 
      table     = t, 
      year      = YEAR, 
      state     = STATES, 
      survey    = SURVEY
    )
  })
  
  names(acs_dat) <- acs_names_fin
  
  return(acs_dat)
}
