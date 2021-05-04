#' Variables of interest relation to PUMS variable names and acs data table codes 
#'
#' `pums_to_acs <- readRDS(here::here(data/pums_to_acs.rds))`
#'
#' @format A list of variables where each list contains a PUMS code or codes and an ACS data table code or codes which is past to `tidycensus` functions for downloading data from user input attributes. See `R/rsp_get_data()` and `Analysis/ACS_PUMS_List.R`  
#' 
"pums_to_acs"




#' Puma areas relation to FIPS codes
#'
#' Dataset to relate FIPS areas/codes to PUMAS data
#' `cts_to_pumas <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")`
#' Added state two-letter codes from internal R `state.abb` dataset
#'
#' @format A data frame with 74091 rows and 4 variables:
#' \describe{
#'   \item{STATEFP}{two-digit state fips code,e.g. 01 = Alabama}
#'   \item{COUNTYFP}{three digit county fips code}
#'   \item{TRACTCE}{six-digit census tract fips code}
#'   \item{PUMA5CE}{5-digit code for PUMA area}
#'   \item{STCNTYFP}{combined state and county fips code}
#'   \item{CTFP}{combined state, county, census tract fips code}
#'   ...
#' }
#' @source \url{https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt}
#' 
"cts_to_pumas"



#' Occupations codes
#'
#' Dataset to relate occupational codes to acs variables
#'
#' @format A data frame with 563 rows and 6 variables:
#' \describe{
#'   \item{Code}{Specific codes corresponding to occupation}
#'   \item{Code2}{Occupation group}
#'   \item{Description}{Occupation description}
#'   \item{d2_code}{Two digit occupation group}
#'   \item{occp_match}{field used to merge pums and acs datasets. See `Get_Helpers.R`}
#'   \item{acs_var}{acs variable code corresponding to number of individuals in that occupation in the census geography}
#'   ...
#' }
#' @source \url{https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2015_2019CodeLists.xlsx}
#' 
"acs_occp_lookup"


#' Age by sex lookup table
#'
#' Dataset to relate acs variables from B01001 tables (age by sex and age by sex by race/ethnicity) to age categories and sex
#'
#' @format A data frame with 298 rows and 6 variables:
#' \describe{
#'   \item{name}{acs variable name}
#'   \item{label}{acs variable label}
#'   \item{concept}{variable description}
#'   \item{age_min}{minimum age captured in variable extracted from label}
#'   \item{age_max}{maximum age captured in variable extracted from label}
#'   \item{sex}{sex captured in variable extracted from label}
#'   \item{sex_num}{numeric sex where Male=1, female=2}
#'   ...
#' }
#' 
"acs_sex_by_age_lookup"


#' Race and ethnicity lookup table
#'
#' Dataset to relate acs variables from B03002 tables (race and ethnicity totals) to race and ethnicity categories
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{name}{acs variable name}
#'   \item{hispanic}{binary of hispanic or latino (1) or not hispanic or latino (0)}
#'   \item{race}{character description of race}
#'   ...
#' }
#' 
"acs_race_eth_lookup"

#' School grade lookup table
#'
#' Dataset to relate acs variables from B14007_ table (detailed school grade) to pums school grades
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{name}{acs variable name}
#'   \item{label}{acs variable label}
#'   \item{grade}{two-character grade classifier found in pums SCHG variable}
#'   ...
#' }
#' 
"acs_grade_lookup"


#' Household income lookup table
#'
#' Dataset to relate acs variables from S1901_C01 tables (household income groups) to aid merging to pums income categories
#'
#' @format A data frame with 18 rows and 3 variables:
#' \describe{
#'   \item{name}{acs variable name}
#'   \item{label}{acs variable label}
#'   \item{concept}{variable description}
#'   \item{income_min}{minimum income captured in variable extracted from label}
#'   \item{income_max}{maximum income captured in variable extracted from label}
#'   ...
#' }
#' 
"acs_hhincome_lookup"