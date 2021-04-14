library(tidyverse)

# FIPS codes to PUMAS lookuptable --------
cts_to_pumas <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
cts_to_pumas$STCNTYFP <- paste0(cts_to_pumas$STATEFP, cts_to_pumas$COUNTYFP)
cts_to_pumas$CTFP <- paste0(cts_to_pumas$STATEFP, cts_to_pumas$COUNTYFP, cts_to_pumas$TRACTCE)

usethis::use_data(cts_to_pumas, overwrite = T)

# Occupation codes lookup table --------
# First get ACS_PUMs dataset with occupation codes and descriptors
temp <- tempfile(fileext = ".xlsx")
download.file(url = "https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2015_2019CodeLists.xlsx", 
              destfile = temp, mode="wb")
OCC_init <- read_excel(temp, sheet="OCCP & SOCP", range="A10:C881")
unlink(temp)

colnames(OCC_init) <- c("Code", "Code2", "Description")

OCC_list <- OCC_init %>%
  filter(!is.na(Code)) %>% 
  mutate(occ_group = as.numeric(substr(Code2,1,2)))

# Get occupational variable descriptors from acs to match to pums
acs_vars_subject<-tidycensus::load_variables(2019, paste0("acs5", "/subject"), cache=FALSE) %>% 
  filter(grepl("S2401_C01", name)) %>% 
  separate(label, into = paste("level",1:6, sep = ""), sep = "!!") %>% 
  filter(!is.na(level4)) %>% 
  mutate(lowest_level = case_when(is.na(level5) & is.na(level6) ~ level4,
                                  is.na(level6) ~ level5,
                                  TRUE ~ level6),
         occp_match = gsub(" ","",stringr::str_to_lower(gsub("[[:punct:]]","", lowest_level))))
  
acs_occp_lookup <- OCC_list %>% 
  mutate(occp_match = gsub(" ","",stringr::str_to_lower(gsub("[[:punct:]]","", Description)))) %>% 
  left_join(acs_vars_subject, by = "occp_match") %>% 
  dplyr::select(Code:name) %>% 
  rename("acs_var" = name)


usethis::use_data(acs_occp_lookup, overwrite = T)


# Age by sex lookup table ------------
acs_vars<-tidycensus::load_variables(2019, "acs5", cache=FALSE)

# Function to extract numbers from string
extract_numbers_min <- function(string){
  string %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% min
}

extract_numbers_max <- function(string){
  string %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% max
}

acs_sex_by_age_lookup <- acs_vars %>% 
  filter(grepl("B01001", name)) %>% 
  mutate(age_min = map_dbl(label, extract_numbers_min),
         age_min = if_else(grepl("Under 5", label), 0, age_min),
         age_max = map_dbl(label, extract_numbers_max),
         sex = if_else(grepl("Male", label), "Male", "Female"),
         sex_num = if_else(sex == "Male", 1, 2)) %>% 
  filter(is.finite(age_min))


usethis::use_data(acs_sex_by_age_lookup, overwrite = T)

# Race ethnicity lookup table -------------
acs_race_eth_lookup <- acs_vars %>% 
  filter(grepl("B03002", name)) %>% 
  separate(label, into = paste("level",1:4, sep = ""), sep = "!!") %>% 
  mutate(hispanic = if_else(grepl("Not Hispanic", level3), 0 ,1),
         race     = level4) %>% 
  filter(!is.na(race)) %>% 
  dplyr::select(name, hispanic, race)
  
usethis::use_data(acs_race_eth_lookup, overwrite = T)

# School grade lookup table -----------------
acs_grade_lookup <- acs_vars %>% 
  filter(grepl("B14007_", name)) %>% 
  slice(-c(1,2)) %>% # remove aggregate totals
  mutate(grade = c(paste0("0", c(1:9)), 10:16, "bb")) %>%  # Match levels in pums data
  dplyr::select(name, label,grade)
  
usethis::use_data(acs_grade_lookup, overwrite = T)


# Household income lookup table ------------
extract_number_money_min <- function(string){
  string %>% str_replace_all(., ",", "") %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% min
}

extract_number_money_max <- function(string){
  string %>% str_replace_all(., ",", "") %>% str_match_all("[0-9]+") %>% unlist %>% as.numeric %>% max
}

acs_hhincome_lookup <- acs_vars %>% 
  filter(grepl("B19001_", name)) %>% 
  mutate(income_min = map_dbl(label, extract_number_money_min),
         income_min = if_else(grepl("Less than", label), 0, income_min),
         income_max = map_dbl(label, extract_number_money_max),
         income_max = if_else(grepl("or more",label), 1e7, income_max)) %>% 
  filter(is.finite(income_min) & income_min != 12)

usethis::use_data(acs_hhincome_lookup, overwrite = T)
