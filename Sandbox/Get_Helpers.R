library(tidyverse)

# FIPS codes to PUMAS lookuptable --------
cts_to_pumas <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
cts_to_pumas$STCNTYFP <- paste0(cts_to_pumas$STATEFP, cts_to_pumas$COUNTYFP)
cts_to_pumas$CTFP <- paste0(cts_to_pumas$STATEFP, cts_to_pumas$COUNTYFP, cts_to_pumas$TRACTCE)

state.abb2 <- data.frame("STATE" = state.abb, STATEFP = c(1:50))
state.abb2$STATEFP = if_else(nchar(state.abb2$STATEFP) == 1,paste0("0",state.abb2$STATEFP),as.character(state.abb2$STATEFP))

cts_to_pumas <- left_join(cts_to_pumas, state.abb2, by = "STATEFP")

usethis::use_data(cts_to_pumas, overwrite = T)

# Occupation codes --------
temp <- tempfile(fileext = ".xlsx")
download.file(url = "https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2014_2018CodeLists.xlsx", 
              destfile = temp, mode="wb")
OCC_list <- read_excel(temp, sheet="OCCP & SOCP", range="A10:C881")
unlink(temp)

OCC_list <- OCC_list %>% 
  rename(Code        = `2018 Census PUMS Occupation Code`,
         Code2       = `2018 SOC Code`,
         Description = `Description (2018 Census Occupation Code)`) %>%
  filter(!is.na(Code)) %>% 
  mutate(d2_code = as.numeric(substr(Code2,1,2)))

occ_groups <- OCC_list %>% 
  filter(grepl("0000", Code2), nchar(Code2) == 7) %>% 
  pull(Description)

d_occ_groups <- OCC_list %>% 
  filter(Description %in% occ_groups) %>% 
  mutate(Group_Code = c(1:nrow(.))) %>% 
  dplyr::select(d2_code, Group_Code) %>% 
  bind_rows(tibble(d2_code = 55, # Add military occps manually
                   Group_Code = length(occ_groups)+1))

OCC_list <- OCC_list %>% 
  left_join(d_occ_groups, by = "d2_code") %>% 
  filter(nchar(Code) == 4)

usethis::use_data(OCC_list, overwrite = T)
