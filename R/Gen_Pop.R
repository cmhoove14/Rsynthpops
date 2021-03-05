#' @title Generate population
#' 
#' @description One-liner function to generate population
#' 
#' @param FIPS US FIPS code for corresponding geography. If length 2, will generate for corresponding state. If length 5, will generate for corresponding county
#' @param CHARS Agent characteristics to include. See `details`
#' @param OUT_DIR Directory to save generated population to. Also will house intermediate downloaded files if KEEP_ALL is TRUE
#' @param KEEP_ALL Logical of whether to keep all intermediate files used to generate populaiton. Defaults to TRUE
#' 
#' @details 
#' 
#' @return 
#' @export
#' 
gen_synth_pop <- function(FIPS, CHARS, OUT_DIR, KEEP_ALL = TRUE){
# Process inputs  
  STATE_FIPS <- substr(FIPS,1,2)
  STATE_GET  <- cts_to_pumas %>% 
    filter(STATEFP == STATE_FIPS) %>% 
    slice(1) %>% 
    pull(STATE)
  
  if(nchar(FIPS) == 2){
    PUMS <- cts_to_pumas %>% 
      filter(STATEFP == FIPS) %>% 
      pull(PUMA5CE) %>% 
      unique()
  } else if(nchar(FIPS) == 5){
    PUMS <- cts_to_pumas %>% 
      filter(STCNTYFP == FIPS) %>% 
      pull(PUMA5CE) %>% 
      unique()
  } else if(nchar(FIPS) == 11){
    PUMS <- cts_to_pumas %>% 
      filter(CTFP == FIPS) %>% 
      pull(PUMA5CE) %>% 
      unique()
  } else {
    stop("Input FIPS code must be length 2 for state, 5 for county, or 11 for census tract")
  }
  
  if(length(PUMS)<1){stop("No PUMS records found for input FIPS code")}
    
# Get all data to generate population   
  p_dat <- get_pums(YEAR      = 2018, 
                    FIVE_YEAR = TRUE, 
                    STATE     = STATE_GET, 
                    LEVEL     = "p", 
                    KEEP      = KEEP_ALL,
                    KEEP_DIR  = OUT_DIR) %>% 
    filter(PUMA %in% PUMS)
  
  h_dat <- get_pums(YEAR      = 2018, 
                    FIVE_YEAR = TRUE, 
                    STATE     = STATE_GET, 
                    LEVEL     = "h", 
                    KEEP      = KEEP_ALL,
                    KEEP_DIR  = OUT_DIR) %>% 
    filter(PUMA %in% PUMS)
  
# Generate weights from PUMS
  p_occp <- p_dat %>% 
    group_by(OCCP) %>%
    summarise(num = n(), prop = num/nrow(.)) %>%
    left_join(OCC_list, by = c("OCCP" = "Code")) %>%
    ungroup() %>%
    mutate(Group_Code = if_else(is.na(Group_Code), "00", str_pad(Group_Code, 2, pad = "0")))
}