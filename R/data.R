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
#'   \item{PEMA5CE}{5-digit code for PUMA area}
#'   ...
#' }
#' @source \url{https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt}
#' 
"cts_to_pumas"