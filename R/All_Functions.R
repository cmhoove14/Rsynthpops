

#' @title Person seed
#' 
#' @description Creates person seed for input to ipf
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
p_seed <- function(){
  
}




#' @title Household seed
#' 
#' @description Creates household seed for input to ipf
#' 
#' @param h_pums household PUMS data (e.g. downloaded from `get_pums`)
#' @param OCCP data table/tibble that contains occupational data for target geography
#' @param AGE_SEX data table/tibble that contains ACS data for target geography stratified by sex and age
#' @param RACE data table/tibble that contains ACS data for target geography stratified by race/ethnicity
#' 
#' @details 
#' 
#' @return list containing targets for occupation, age, sex and race for the CT
#' @export
#' 
h_seed <- function(){
  
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






#' @title Create synthetic population for target census tract
#' 
#' @description Creates synthetic population from input target and seed data for a 
#' 
#' @param CT character containing name of CT as it appears in ACS data
#' @param PTGT list generated from `p_target` with person characteristics
#' @param HTGT list generated from `h_target` with household characteristics
#' @param PSEED data frame containing person pums data for target geography
#' @param HSEED data frame containing household pums data for target geography
#' 
#' @details 
#' 
#' @return data.frame of all generated agents
#' @export
#' 
gen_ct_pop <- function(CT, PTGT, HTGT, PSEED, HSEED) {

  ct_ipu   <- ipu(HSEED, HTGT, PSEED, PTGT, primary_id="SERIALNO")
  ct_syn_h <- synthesize(ct_ipu$weight_tbl, primary_id="SERIALNO")
  ct_syn_p <- left_join(ct_syn_h, pseed, by="SERIALNO") %>%
    mutate(indiv_id=rownames(.),
           ct = CT) %>% 
    rename(house_id=new_id) %>%
    select(house_id, indiv_id, hhsize, hhincome, sex, age, occp, race, ct)
  
  ct_syn_p
}






#' @title Create synthetic population 
#' 
#' @description Creates synthetic population from input target and seed data for a geography larger than a census tract
#' 
#' @param CT character containing name of CT as it appears in ACS data
#' @param PTGT list generated from `p_target` with person characteristics
#' @param HTGT list generated from `h_target` with household characteristics
#' @param PSEED data frame containing person pums data for SF county
#' @param HSEED data frame containing household pums data for SF county 
#' 
#' @details 
#' 
#' @return data.frame of all generated agents
#' @export
#' 
gen_pop <- function(CTs){
  
  # Loop through all cts to construct population
  pop <- bind_rows(lapply(CTs, function(CT){
    
    persons <- p_target(ct, acs_occup_sf, acs_age_sex_sf, acs_race_sf)
    hh      <- h_target(ct, acs_hh_income_sf, acs_hhsize_sf)
    
    ct_pop <- gen_ct_pop(CT = CT, 
                         PTGT = persons,
                         HTGT = hh,
                         PSEED = PSEED,
                         HSEED = HSEED)
    
    return(ct_pop)
  }))
  
  #Correct individual and household ids to give global unique id rather than unique within ct
  pop$ct_hh_id <- paste0(pop$CT, "_", pop$house_id)  
  pop$hhid     <- as.numeric(factor(pop$ct_hh_id))
  pop$house_id <- pop$ct_hh_id <- NULL # remove superfluous columns
  
  pop$ct_indiv_id <- paste0(pop$CT, "_", pop$indiv_id)  
  pop$id          <- as.numeric(factor(pop$ct_indiv_id))
  pop$indiv_id    <- pop$ct_indiv_id <- NULL # remove superfluous columns
  
  return(pop)
}
