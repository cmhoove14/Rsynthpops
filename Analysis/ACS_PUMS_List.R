# Variable to PUMS to ACS lookup table
pums_to_acs <- list()

# Sex
  pums_to_acs$Sex      <- list()
  pums_to_acs$Sex$PUMS <- "SEX"
  pums_to_acs$Sex$ACS  <- "B01001"

# Age
  pums_to_acs$Age      <- list()
  pums_to_acs$Age$PUMS <- "AGEP"
  pums_to_acs$Age$ACS  <- "B01001"
  
# Race
  pums_to_acs$Race      <- list()
  pums_to_acs$Race$PUMS <- "RAC1P"
  pums_to_acs$Race$ACS  <- "B03002"
  
# Ethnicity  
  pums_to_acs$Ethnicity      <- list()
  pums_to_acs$Ethnicity$PUMS <- "HISP"
  pums_to_acs$Ethnicity$ACS  <- "B03002"
  
# Occupation  
  pums_to_acs$Occupation      <- list()
  pums_to_acs$Occupation$PUMS <- "OCCP"
  pums_to_acs$Occupation$ACS  <- "S2401"
  
# School grade
  pums_to_acs$Grade      <- list()
  pums_to_acs$Grade$PUMS <- "SCHG"
  pums_to_acs$Grade$ACS  <- "B14007"
  
# School type
  pums_to_acs$School_Type      <- list()
  pums_to_acs$School_Type$PUMS <- "SCH"
  pums_to_acs$School_Type$ACS  <- "B14003"
 
# Household Income
  pums_to_acs$HH_Income      <- list()
  pums_to_acs$HH_Income$PUMS <- c("ADJINC", "HINCP")
  pums_to_acs$HH_Income$ACS  <- "B19001"
  
# Household Type  
  pums_to_acs$HH_Type      <- list()
  pums_to_acs$HH_Type$PUMS <- "HHT"
  pums_to_acs$HH_Type$ACS  <- "B11016"

# Household Size    
  pums_to_acs$HH_Size      <- list()
  pums_to_acs$HH_Size$PUMS <- "NP"
  pums_to_acs$HH_Size$ACS  <- "B11016"
  
  usethis::use_data(pums_to_acs, overwrite = T)
  