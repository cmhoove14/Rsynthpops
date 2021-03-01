library(readr)
library(tigris)
library(tmap)
library(ipfr)
library(tidycensus)
library(tidyverse)
library(readxl)

# See https://github.com/datamade/census for information about census data
# See https://api.census.gov/data/2018/acs/acs5/variables.html about variables
# see https://censusreporter.org/profiles/05000US06075-san-francisco-county-ca/ for variables

# age, household size, race, income, and occupation 
# variables wanted for generating synthetic population

#============================ maps ==============================
ca.pumas <- pumas("CA", year = 2018)

tmap_mode("view")
tm_basemap(leaflet::providers$OpenStreetMap)+
tm_shape(ca.pumas) +
  tm_polygons(id = "NAMELSAD10", alpha = 0) +
  tm_shape(ca.pumas[grepl("San Francisco", ca.pumas$NAMELSAD10), ]) +
  tm_polygons(id = "NAMELSAD10", alpha = 0, border.col = "red")

ca.place <- places("CA", year = 2018)
tm_basemap(leaflet::providers$Esri.WorldImagery)+
  tm_shape(ca.place) +
  tm_polygons(id = "NAME", alpha = 0) +
  tm_shape(ca.place[ca.place$NAME == "San Francisco",]) +
  tm_polygons(id = "NAME", alpha = 0, border.col = "red")


#====================== household seed data ============================
#downloaded from Census FTP site: #https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/
# 1. Less than $10,000
# 2. $10,000 to $14,999
# 3. $15,000 to $24,999
# 4. $25,000 to $34,999
# 5. $35,000 to $49,999
# 6. $50,000 to $74,999
# 7. $75,000 to $99,999
# 8. $100,000 to $149,999
# 9. $150,000 to $199,999
# 10. $200,000 or more

temp <- tempfile()
download.file("https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/csv_hca.zip",temp)
PUMS_2018_h <- read_csv(unz(temp, "psam_h06.csv"))
unlink(temp)

h <- read_csv("csv_hca (1)/psam_h06.csv")

h_seed<- PUMS_2018_h %>% 
  filter(PUMA %in% c("07501", "07502", "07503", "07504", "07505", "07506", "07507"), NP!=0, !is.na(SERIALNO), !is.na(HINCP)) %>%
  mutate(hhsize = ifelse(NP > 6, 7, NP), 
         hhincome = HINCP *(ADJINC/1000000), # adjust all income to 2018 dollars
         hhincome = cut(HINCP, breaks = c(-20000, 50000, 100000, Inf), include.lowest = TRUE, right = FALSE), hhincome = as.numeric(hhincome)) %>%
  dplyr::select(SERIALNO, hhsize, hhincome)


#============================ processing seed data ==============================
# read in person data from PUMS 2014-2018 5 year survey
temp <- tempfile()
download.file("https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/csv_pca.zip",temp)
PUMS_2018_p <- read_csv(unz(temp, "psam_p06.csv"))
unlink(temp)

p<-read_csv("csv_hca (1)/psam_p06.csv")
# for SF county only
# PUMA list https://www.census.gov/geographies/reference-maps/2010/geo/2010-pumas/california.html
PUMS_2018_sf <- PUMS_2018_p %>%
  filter(PUMA %in% c("07501", "07502", "07503", "07504", "07505", "07506", "07507"))
nrow(PUMS_2018_sf)
PUMS_2018_sf$Agegroup <- cut(PUMS_2018_sf$AGEP, breaks = c(-Inf, 4, 11, 18, 51, 70, Inf), right = FALSE)
pop.prop <- PUMS_2018_sf %>%
  group_by(Agegroup) %>%
  summarize(prop = n()) %>%
  mutate(prop = prop/sum(prop))
pop.prop


#### Make OCC dataset
temp <- tempfile(fileext = ".xlsx")
download.file(url = "https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2014_2018CodeLists.xlsx", destfile = temp, mode="wb")
OCC.list <- read_excel(temp, sheet="OCCP & SOCP", range="A10:C881")
unlink(temp)

OCC.list <- OCC.list %>% 
  rename(Code = `2018 Census PUMS Occupation Code`) %>%
  rename(Description = `Description (2018 Census Occupation Code)`) %>%
  filter(!is.na(Code)) 

occ_groups <- c("Management Occupations:", "Business and Financial Operations Occupations:", "Computer and mathematical occupations:",
                "Architecture and Engineering Occupations:", "Life, Physical, and Social Science Occupations:", "Community and Social Service Occupations:",
                "Legal Occupations:", "Educational Instruction and Library Occupations:", "Arts, Design, Entertainment, Sports, and Media Occupations:",
                "Healthcare Practitioners and Technical Occupations:", "Healthcare Support Occupations:", "Protective Service Occupations:", "Food Preparation and Serving Related Occupations:",
                "Building and Grounds Cleaning and Maintenance Occupations:", "Personal Care and Service Occupations:", "Sales and Related Occupations:",
                "Office and Administrative Support Occupations:", "Farming, Fishing, and Forestry Occupations:", "Construction and Extraction Occupations:",
                "Installation, Maintenance, and Repair Occupations:", "Production Occupations:", "Transportation Occupations:", "Material Moving Occupations:")

d_occ_groups <- filter(OCC.list, Description %in% occ_groups)

OCC.list$New_Code <- NA
cutoffs <- str_split(d_occ_groups$Code, "-")
for(i in 1:23){
  cutoff_group <- cutoffs[[i]]
  if(cutoff_group[2]=="4150")
    cutoff_group[2] <- "4160"
  OCC.list$New_Code[OCC.list$Code >= cutoff_group[1] & OCC.list$Code <= cutoff_group[2]] <- i
}

sf_occp <- PUMS_2018_sf %>%
  filter(! OCCP %in% c("9800", "9810", "9825", "9830")) %>%   # remove military
  group_by(OCCP) %>%
  summarise(num = n(), prop = num/nrow(PUMS_2018_sf)) %>%
  left_join(OCC.list, by = c("OCCP" = "Code")) %>%
  ungroup() %>%
  mutate(new.code = ifelse(is.na(`New_Code`), "00", str_pad(`New_Code`, 2, pad = "0")))


### Race
# 1. White alone
# 2. .Black or African American alone
# 3. American Indian alone
# 3. Alaska Native alone
# 3. American Indian and Alaska Native tribes specified; or American Indian or Alaska Native, not specified and no other races
# 4. Asian alone
# 5. Native Hawaiian and Other Pacific Islander alone
# 6. Some Other Race alone
# 7. Two or More Races
# 8. Hispanic

p_seed <- PUMS_2018_sf %>%
  filter(! OCCP %in% c("9800", "9810", "9825", "9830")) %>%   # remove military
  dplyr::select(SERIALNO, SEX, AGEP, OCCP, SCHG, JWMNP, JWTR, HISP, RAC1P) %>%
  mutate(sex = SEX, 
         age = cut(AGEP, breaks = c(0,10,20,30,40,50,60,70,80,Inf), right = FALSE, include.lowest = TRUE), 
         age = as.numeric(age)) %>%  
         left_join(OCC.list[,c("Code", "New_Code")], by = c("OCCP" = "Code")) %>%
  mutate(occp = ifelse(is.na(`New_Code`), 0, `New_Code`),
         # combine american indian and alaskan native categories
         # and recode variable
         combine_aain = ifelse(RAC1P %in% c(4, 5), 3, RAC1P), 
         all_race = ifelse(combine_aain >= 6, combine_aain-2, combine_aain),
         race = ifelse(HISP != '01', 8, all_race)) %>%
  dplyr::select(SERIALNO, sex, age, occp, race) %>% 
  filter(!is.na(SERIALNO), SERIALNO %in% h_seed$SERIALNO)






#============================ processing target data ==============================
# see different geography here https://walker-data.com/tidycensus/articles/basic-usage.html
# See hierarchy of geography here https://api.census.gov/data/2018/acs/acs5/geography.html
# See here for different table https://data.census.gov/cedsci/

census_api_key("443aa1caca4e8860f0b3cdaa41b3ac3de6664725", install = TRUE, overwrite = TRUE)    # replace with your key, which can be applied from here https://api.census.gov/data/key_signup.html
#create variable search df from tidycensus
acs_var18<-load_variables(2018, "acs5", cache=FALSE)
acs_subject_var18<-load_variables(2018, "acs5/subject", cache=FALSE)

#### Load all data ####
# occupation by sex, race/hispanic origin, age, etc.
# filter for SF county
#======= occupation by sex ==============
# 16 years and older
# read codesheet
# 1	Management occupations
# 2	Business and financial operations occupations
# 3	Computer and mathematical occupations
# 4	Architecture and engineering occupations
# 5	Life, physical, and social science occupations
# 6	Community and social service occupations
# 7	Legal occupations
# 8	Educational instruction, and library occupations
# 9	Arts, design, entertainment, sports, and media occupations
# 10	Health diagnosing and treating practitioners and other technical occupations
# 10	Health technologists and technicians
# 11	Healthcare support occupations
# 12	Firefighting and prevention, and other protective service workers including supervisors
# 12	Law enforcement workers including supervisors
# 13	Food preparation and serving related occupations
# 14	Building and grounds cleaning and maintenance occupations
# 15	Personal care and service occupations
# 16	Sales and related occupations
# 17	Office and administrative support occupations
# 18	Farming, fishing, and forestry occupations
# 19	Construction and extraction occupations
# 20	Installation, maintenance, and repair occupations
# 21	Production occupations
# 22	Transportation occupations
# 23	Material moving occupations

acs_occup <- get_acs(geography = "tract", table = "S2401", year = 2018, state = "CA", survey = "acs5") 

acs_occup_sf <- acs_occup %>%
  filter(grepl("San Francisco County, California", NAME)) %>%
  filter(grepl("C01", variable)) 


#======= age by sex ====================
# sex
# 1 male
# 2 female

# age group     recode
# 1 <5          1
# 2 5-9         1      
# 3 10-14       2
# 4 15-17       2
# 5 18-19       2
# 6 20          3
# 7 21          3
# 8 22-24       3
# 9 25-29       3
# 10 30-34      4
# 11 35-39      4
# 12 40-44      5
# 13 45-49      5
# 14 50-54      6
# 15 55-59      6
# 16 60-61      7
# 17 62-64      7
# 18 65-66      7
# 19 67-69      7
# 20 70-74      8
# 21 75-79      8
# 22 80-84      9
# 23 85+        9

acs_age_sex <- get_acs(geography = "tract", table = "B01001", year = 2018,  state="CA", survey = "acs5")

acs_age_sex_sf <- acs_age_sex %>%
  filter(grepl("San Francisco County, California", NAME))


#================== race =======================
acs_race <- get_acs(geography = "tract", table = "B03002", year = 2018,  state="CA", survey = "acs5")

acs_race_sf <- acs_race %>%
  filter(grepl("San Francisco County, California", NAME))


#====================== household target data ==========================
acs_hhsize<- get_acs(geography = "tract",
                     variables = c(fam_2="B11016_003", fam_3="B11016_004", fam_4="B11016_005",
                                   fam_5="B11016_006", fam_6="B11016_007", fam_7plus="B11016_008",
                                   non_1="B11016_010", non_2="B11016_011", non_3="B11016_012",
                                   non_4="B11016_013", non_5="B11016_014", non_6="B11016_015",
                                   non_7plus="B11016_016"),
                     state = "CA",
                     year = 2018)

acs_hhsize_sf <- acs_hhsize %>%
  filter(grepl("San Francisco County, California", NAME))

#======= household income ====================
# 1. Less than $10,000        1  
# 2. $10,000 to $14,999       1
# 3. $15,000 to $24,999       1
# 4. $25,000 to $34,999       1
# 5. $35,000 to $49,999       1 
# 6. $50,000 to $74,999       2
# 7. $75,000 to $99,999       2
# 8. $100,000 to $149,999     3 
# 9. $150,000 to $199,999     3
# 10. $200,000 or more        3
acs_hh_income <- get_acs(geography = "tract", table = "S1901", year = 2018,  state = "CA", survey = "acs5")

acs_hh_income_sf <- acs_hh_income %>%
  filter(grepl("San Francisco County, California", NAME))


#### get list of all census tracts in SF ####
CT_sf <- unique(acs_occup_sf$NAME)


#### define functions that will generate household targets and person targets  for each census tract ####
#### and to create a synthetic population

p_target <- function(ct_name, occup, age_sex, race){
  # function for creating a person target for a specific census tract in SF county
  # input: 
    # ct_name=character containing name of CT as it appears in ACS data
    # occup=data table/tibble that contains occupational data for all of SF county
    # age_sex=data table/tibble that contains ACS data for SF county stratified by 
    #   sex and age
    # race=data table/tibble that contains ACS data for SF county stratified by
    #   race/hispanic origin
  # output: 
    # target=list containing targets for occupation, age, sex and race for the ct_name
  
  # calculate totals stratified by occupation
  acs_occup_reshape <- occup %>%
    filter(NAME == ct_name) %>%
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
  
  acs_age_sex_reshape <- age_sex %>%
    filter(NAME == ct_name) %>%
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
  
  acs_race_reshape <- race %>% 
    filter(NAME==ct_name) %>%
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

h_target <- function(ct_name, income, size){
  # function for creating a household target for a specific census tract in SF county
  # input: 
  # ct_name=character containing name of CT as it appears in ACS data
  # income=data table/tibble that contains household income data for all of SF county
  #   all income is in 2018-inflation-adjusted dollars
  # size=data table/tibble that contains ACS data for SF county stratified by 
  #   household size
  # output: 
  # target=list containing targets for occupation, age, sex and race for the ct_name
  
  acs_size <- size %>% 
    filter(NAME == ct_name) %>%
    dplyr::select(variable, estimate) %>% 
    spread(., key=variable, value=estimate, fill=NA)
   
  acs_income <- income %>%
    filter(NAME==ct_name)
  
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

syn_pop <- function(ct_name, ptarget, htarget, pseed, hseed) {
  # create and save synthetic population 
  # input: 
  # p_target=list containing target counts for different characteristics of the population
  # h_target=list containing target counts for different housing characteristics
  # p_seed=data frame containing person pums data for SF county
  # h_seed=data frame containing household pums data for SF county 
  # output: 
  # synthetic pop
  
  #============ IPU ===================
  ct_ipu <- ipu(hseed, htarget, pseed, ptarget, primary_id="SERIALNO")
  ct_syn_h <- synthesize(ct_ipu$weight_tbl, primary_id="SERIALNO")
  ct_syn_p <- left_join(ct_syn_h, pseed, by="SERIALNO") %>%
    mutate(indiv_id=rownames(.)) %>% 
    rename(house_id=new_id) %>%
    select(house_id, indiv_id, hhsize, hhincome, sex, age, occp, race)
  
  ct_syn_p
}


#### test function/sanity checks ####
# looks good!
# a<-p_target(CT_sf[1], acs_occup_sf, acs_age_sex_sf, acs_race_sf)
# acs_age_sex_sf %>% filter(NAME==CT_sf[1])
# acs_race_sf %>% filter(NAME==CT_sf[1])
# sum(a$race)
# 
# b<-h_target(CT_sf[1], acs_hh_income_sf, acs_hhsize_sf)
# sum(filter(acs_hh_income_sf, NAME==CT_sf[1])$estimate[2:6])/100*1450
#
# ct_syn_p <- syn_pop(CT_sf[1], a, b, p_seed, h_seed)
# max(ct_syn_p$house_id)
# ct_syn_p %>% group_by(house_id) %>% 
#   summarise(size=n()) %>% 
#   group_by(size) %>% 
#   summarise(estimate = n()/max(ct_syn_p$house_id))
# b$hhsize/sum(b$hhsize) # proportions of houses of different sizes seem to match pretty closely
# ct_syn_p %>% group_by(house_id) %>% 
#   summarise(inc=mean(hhincome)) %>%
#   group_by(inc) %>%
#   summarise(estimate=n()/max(ct_syn_p$house_id))
# b$hhincome/sum(b$hhincome) # proportions of houses with different incomes seem to match closely
# ct_syn_p %>%
#   group_by(race) %>%
#   summarise(estimate=n()/nrow(ct_syn_p))
# a$race/sum(a$race) # population looks similar stratified by race!

#### create and save population for each census tract ####
# CT 9804.01 and 9901 have population size 0
# filter out those ct names
CT_sf_filtered <- CT_sf[!(grepl(("9804.01|9901"), CT_sf))]

total_pop <- NULL
for (ct in CT_sf_filtered[1]) {
  persons <- p_target(ct, acs_occup_sf, acs_age_sex_sf, acs_race_sf)
  hh <- h_target(ct, acs_hh_income_sf, acs_hhsize_sf)
  pop <- syn_pop(ct, persons, hh, p_seed, h_seed)
  # find geoid
  has_id <- acs_occup_sf %>% filter(NAME==ct)
  # add geoid column
  pop$geoid <- has_id$GEOID[1]
  total_pop <- rbind(total_pop, pop)
}


# for (ct in CT_sf[194:196]) {
#   persons <- p_target(ct, acs_occup_sf, acs_age_sex_sf, acs_race_sf)
#   hh <- h_target(ct, acs_hh_income_sf, acs_hhsize_sf)
#   syn_pop(ct, persons, hh, p_seed, h_seed)
# }


# # add geoid to each csv
# for (ct in CT_sf_filtered) {
#   # read in csv
#   pop <- read.csv(paste("synthetic_pops/", ct, ".csv", sep=""))
#   # find geoid
#   has_id <- acs_occup_sf %>% filter(NAME==ct)
#   # add geoid column
#   pop$geoid <- has_id$GEOID[1]
#   total_pop <- rbind(total_pop, pop)
# }

write.csv(total_pop, "synthetic_pops/total_sf_pop.csv", row.names = FALSE)
