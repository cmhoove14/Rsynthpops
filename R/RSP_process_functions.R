#---------------------------------------------
# Rsynthpops process data functions 
# Chris Hoover April 2021 
#---------------------------------------------

#' @title Process household level target data from acs totals
#' 
#' @description 
#' 
#' @param acs_hh_dat list of data frames containing household level data from acs/census download, filtered to contain household variables
#' @param chars vector of characteristics to include in population. will only look for data in `acs_hh_dat` that is indicated by characteristics in this vector. e.g. will look for and process household income data if "HH_Income" is an element of `chars`
#' @param fips_use fips code to restrict acs data by. 
#' @param hh_income_breaks vector of breaks at which to divide hh income categories. be sure to match this to same breaks used in `rsp_process_hh_seed()` to ensure levels match
#' 
#' @details 
#' 
#' @return list containing named elements that match columns names of household level variables in seed data. in each list is a data frames where columns match categories of the variables in seed data.
#' @export
#' 
rsp_process_hh_tgt <- function(acs_hh_dat, chars, fips_use, hh_income_breaks = c(-100,50,100,100000)*1000){
  #List to fill and return
  hh_tgt <- list()
  
  # household size -----------------------
  if("HH_Size" %in% chars){
    acs_hhsize_tgt <- acs_hh_dat[["HH_Type_HH_Size"]] %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      left_join(acs_hhsize_type_lookup, by = c("variable" = "name")) %>% 
      dplyr::select(-hh_type) %>% 
      filter(!is.na(hh_size)) %>% 
      group_by(GEOID, hh_size) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = hh_size, values_from = estimate)
    
    hh_tgt$HH_Size <- acs_hhsize_tgt
  }
  
  # household type -----------------------
  if("HH_Type" %in% chars){
    acs_hhtype_tgt <- acs_hh_dat[["HH_Type_HH_Size"]] %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      left_join(acs_hhsize_type_lookup, by = c("variable" = "name")) %>% 
      dplyr::select(-hh_size) %>% 
      filter(!is.na(hh_type)) %>% 
      group_by(GEOID, hh_type) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = hh_type, values_from = estimate)
    
    hh_tgt$HH_Type <- acs_hhtype_tgt
  }
  
  # household income ---------------
  if("HH_Income" %in% chars){
    acs_hhincome_tgt <- acs_hh_dat[["HH_Income"]] %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      left_join(acs_hhincome_lookup, by = c("variable" = "name")) %>% 
      filter(!is.na(label)) %>% 
    # match income groups to seed data
      mutate(hhincome = as.factor(cut(income_max,
                                      breaks = hh_income_breaks,
                                      right = FALSE,
                                      labels = FALSE))) %>% 
      group_by(GEOID, hhincome) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = hhincome, values_from = estimate)
    
    hh_tgt$HH_Income <- acs_hhincome_tgt
  }
  
  return(hh_tgt)
}
  
  
  
#' @title Process household level seed data from pums
#' 
#' @description 
#' 
#' @param pums_dat pums dataset, e.g. from `rsp_get_pums`
#' @param pumas puma areas to include 
#' @param hh_vars vector of household variable names found in pums dat to process
#' @param hh_income_breaks vector of breaks at which to divide hh income categories
#' 
#' @details 
#' 
#' @return data frame containing household pums records with all household level characteristics of interest 
#' @export
#' 

rsp_process_hh_seed <- function(pums_dat, pumas, hh_vars, hh_income_breaks = c(-100,50,100,10000)*1000){
  hh_seed <- pums_dat %>% 
    filter(PUMA %in% pumas) %>% 
    # Ignore individuals in group quarters as they are synthesized seperately
    filter(!grepl("GQ", SERIALNO)) %>% 
    # Restrict to our household variables
    dplyr::select(c("SERIALNO", "SPORDER", "PUMA", all_of(hh_vars)))
  
  
  if("HINCP" %in% hh_vars){
    hh_seed <- hh_seed %>% 
    # Create household income adjusted for inflation variable and then categorize
    mutate(
      HHINCADJ  = HINCP*as.numeric(ADJINC),
      HH_Income = as.factor(cut(HHINCADJ,
                                breaks = hh_income_breaks,
                                right = FALSE,
                                labels = FALSE))
    ) %>% 
      dplyr::select(-c("HHINCADJ", "HINCP", "ADJINC"))
  } 
  
  if("NP" %in% hh_vars){
    hh_seed <- hh_seed %>% 
      mutate(
        HH_Size = if_else(NP >= 7, 7, NP), # any households with >7 people grouped together to match acs
      ) %>% 
      dplyr::select(-"NP")
    
    
  }
  
  if("HHT" %in% hh_vars){
    hh_seed <- hh_seed %>% 
      mutate(
        HH_Type = if_else(HHT %in% c("1", "2", "3"), 1, 2)     # Family for 1,2,3 ; GQ/other/Non-family for b, 4-7
      ) %>% 
      dplyr::select(-"HHT")
  }
  
  
  hh_seed_out <- hh_seed %>% 
    # Only one observation per household
    group_by(SERIALNO) %>% 
    summarise(across(.cols = everything(),
                     .fns = first))
  
  return(hh_seed_out)
}  





#' @title Process person level target data from acs totals
#' 
#' @description 
#' 
#' @param acs_p_dat list of data frames containing person level data from acs/census download, filtered to contain person variables
#' @param chars vector of characteristics to include in population. will only look for data in `acs_p_dat` that is indicated by characteristics in this vector. e.g. will look for and process age data if "Age" is an element of `chars`
#' @param fips_use fips code to restrict acs data by. 
#' @param p_age_breaks vector of breaks at which to divide hh income categories. be sure to match this to same breaks used in `rsp_process_hh_seed()` to ensure levels match
#' 
#' @details 
#' 
#' @return list containing named elements that match columns names of household level variables in seed data. in each list is a data frames where columns match categories of the variables in seed data.
#' @export
#' 

rsp_process_p_tgt <- function(acs_p_dat, chars, fips_use, p_age_breaks = c(0,10,20,30,40,50,60,70,80,150)){
  #List to fill and return
  p_tgt <- list()
  
  # Age -------------
  if("Age" %in% chars){
    acs_age_tgt <- acs_p_dat[["Sex_Age"]] %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      # merge with acs_sex_by_age lookup table to get age categories and sex associated with each variable
      left_join(acs_sex_by_age_lookup, by = c("variable" = "name")) %>% 
      filter(!variable %in% c("B01001_001", "B01001_002","B01001_026")) %>%  # Remove total aggregate, aggregate male, aggregate female counts 
      # match decile age groups to seed data
      mutate(age_cat = as.factor(cut(age_max, breaks = p_age_breaks, 
                                     labels = FALSE, right = FALSE, include.lowest = TRUE))) %>% 
      group_by(GEOID,age_cat) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = age_cat, values_from = estimate)
    
    p_tgt$Age <- acs_age_tgt
    
  }
 
  
  # Sex ---------------------
  if("Sex" %in% chars){
    acs_sex_tgt <- acs_p_dat[["Sex_Age"]] %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      # merge with acs_sex_by_age lookup table to get age categories and sex associated with each variable
      left_join(acs_sex_by_age, by = c("variable" = "name")) %>% 
      filter(!variable %in% c("B01001_001", "B01001_002","B01001_026")) %>%  # Remove total aggregate, aggregate male, aggregate female counts 
      group_by(GEOID,sex_num) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = sex_num, values_from = estimate)
    
    p_tgt$Sex <- acs_sex_tgt
  }
  
  # Race ---------------------
  if("Race" %in% chars){
    acs_race_tgt <- acs_p_dat[["Race_Ethnicity"]]  %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      # merge with lookup table to get race categories and binary ethnicity associated with each variable
      left_join(acs_race_eth_lookup, by = c("variable" = "name")) %>% 
      filter(!variable %in% c("B03002_001", "B03002_002", "B03002_012")) %>% # Remove aggregate totals variables
      # recode race variable to match pums seed data
      mutate(race = case_when(grepl("White", race) ~ 1,
                              grepl("Black", race) ~ 2,
                              grepl("American Indian", race) ~ 3,
                              grepl("Asian", race) ~ 6,
                              grepl("Native Hawaiian", race) ~ 7,
                              grepl("Some other", race) ~ 8,
                              grepl("Two or more", race) ~ 9)) %>% 
      group_by(GEOID,race) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = race, values_from = estimate)
    
    p_tgt$Race <- acs_race_tgt
  }
  
  # Ethnicity ---------------------
  if("Ethnicity" %in% chars){
    acs_eth_tgt <- acs_p_dat[["Race_Ethnicity"]] %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      # merge with lookup table to get race categories and binary ethnicity associated with each variable
      left_join(acs_race_eth_lookup, by = c("variable" = "name")) %>% 
      filter(!variable %in% c("B03002_001", "B03002_002", "B03002_012")) %>% # Remove aggregate totals variables
      group_by(GEOID,hispanic) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = hispanic, values_from = estimate)
    
    p_tgt$Ethnicity <- acs_eth_tgt
  }
  
  # School grade ----------------
  if("Grade" %in% chars){
    acs_grade_tgt <- acs_p_dat[["Grade"]] %>% 
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
      left_join(acs_grade_lookup, by = c("variable" = "name")) %>% 
      filter(!is.na(grade)) %>%  # filter out aggregate totals
      group_by(GEOID, grade) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = grade, values_from = estimate)
    
    p_tgt$Grade <- acs_grade_tgt
  }
  
  # School type ----------------
  if("School_Type" %in% chars){
    acs_scltype_tgt <- acs_p_dat[["School_Type"]] %>% 
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use,
             variable %in% c("B14003_003", "B14003_031",      # Males and females in public schools
                             "B14003_012", "B14003_040",      # Males and females in private schooles
                             "B14003_021", "B14003_049")) %>% # Totals not in school
      mutate(scltype = case_when(variable %in% c("B14003_003", "B14003_031") ~ "pub",
                                 variable %in% c("B14003_012", "B14003_040") ~ "pvt",
                                 variable %in% c("B14003_021", "B14003_049") ~ "non")) %>% 
      group_by(GEOID, scltype) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = scltype, values_from = estimate)
    
    p_tgt$School_Type <- acs_scltype_tgt
    
  }
  
  
  # Occupation --------------
  if("Occupation" %in% chars){
    # Filter out higher level aggregations of occupational classes to only get lowest level classes
    lowest_acs_occp_vars <- acs_occp_lookup %>% 
      filter(str_length(Code2) < 10, !is.na(acs_var)) %>% 
      pull(acs_var)
    
    acs_occup_tgt_init <- acs_p_dat[["Occupation"]] %>%
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use,
             variable %in% lowest_acs_occp_vars) %>% 
      # Merge with lookup table in order to match with seed data
      left_join(acs_occp_lookup, by = c("variable" = "acs_var")) %>% 
      group_by(GEOID, occ_group) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup()
    
    # No unemployed counts in acs data, so need to determine number of people not working by subtracting working population from total population
    pop_totals <- acs_p_dat[["Sex_Age"]] %>% 
      filter(substr(GEOID, 1, nchar(fips_use)) == fips_use,
             variable == "B01001_001") # Total population
    wrk_totals <- acs_occup_tgt_init %>% group_by(GEOID) %>% summarise(estimate = sum(estimate)) #Total working population
    
    nwrk_pop <- pop_totals %>% left_join(wrk_totals, by = c("GEOID")) %>% #Total non-working population
      mutate(occ_group = 99,
             estimate = estimate.x - estimate.y) %>% 
      dplyr::select(GEOID, occ_group, estimate)
    
    
    # Join unemployed with employment data and Pivot wider for final
    acs_occup_tgt <- rbind(acs_occup_tgt_init, nwrk_pop) %>% 
      group_by(GEOID, occ_group) %>% 
      summarise(estimate = sum(estimate)) %>% 
      ungroup() %>% 
      pivot_wider(names_from = occ_group, values_from = estimate)
    
    p_tgt$Occupation <- acs_occup_tgt
  }

  return(p_tgt)
}




#' @title Process person level seed data from pums
#' 
#' @description 
#' 
#' @param pums_dat pums dataset, e.g. from `rsp_get_pums`
#' @param pumas puma areas to include 
#' @param p_vars vector of person level  variable names found in pums_dat to process
#' @param p_age_breaks vector of breaks at which to divide age categories, defaults to deciles
#' 
#' @details 
#' 
#' @return data frame containing person-level pums records with all household level characteristics of interest 
#' @export
#' 
rsp_process_p_seed <- function(pums_dat, pumas, p_vars, p_age_breaks = c(0,10,20,30,40,50,60,70,80,150)){
  p_seed <- pums_dat %>% 
    filter(PUMA %in% pumas) %>% 
    # Ignore individuals in group quarters who will be added separately
    filter(!grepl("GQ", SERIALNO)) %>% 
    # Restrict to our person variables
    dplyr::select(unique(c("SERIALNO", "SPORDER", "PUMA", all_of(p_vars))))
  
  if("AGEP" %in% p_vars){
    p_seed <- p_seed %>% 
      mutate(
        # Condense age into categories
        Age = as.factor(cut(AGEP, breaks = p_age_breaks, 
                            labels = FALSE, right = FALSE, include.lowest = TRUE))
      ) %>% 
      dplyr::select(-AGEP)
  }
  
  if("RAC1P" %in% p_vars){
    p_seed <- p_seed %>% 
      mutate(
        # Condense American Indian and Alaska Native categories to match acs reporting of aggregate totals
        Race = if_else(RAC1P %in% c("3", "4", "5"), "3", RAC1P)
      )%>% 
      dplyr::select(-RAC1P)
  } 
  
  if("HISP" %in% p_vars){
    p_seed <- p_seed %>% 
      mutate(
        # make hispanic category numeric
        Hispanic = if_else(HISP == "01", 0, 1)
      )%>% 
      dplyr::select(-HISP)
  }
  
  if("SCH" %in% p_vars){
    p_seed <- p_seed %>% 
      mutate(
        # More informative school type codes
        School_Type = case_when(SCH == "b" ~ "non",
                                SCH == "1" ~ "non",
                                SCH == "2" ~ "pub",
                                SCH == "3" ~ "pvt")
      )%>% 
      dplyr::select(-SCH)
  }
  
  if("SCHG" %in% p_vars){
    p_seed <- p_seed %>% 
      rename("Grade" = SCHG)
  }
  
  if("SEX" %in% p_vars){
    p_seed <- p_seed %>% 
      rename("Sex" = SEX)
  }
  
  if("OCCP" %in% p_vars){
    p_seed <- p_seed %>% 
      rename("Occupation" = OCCP) %>% 
      # Merge with lookup table which relates acs and occp codes then add code 9 for unemployed/retired/in school
      left_join(acs_occp_lookup %>% dplyr::select(Code, occ_group),
                by = c("Occupation" = "Code")) %>% 
      mutate(occ_group = if_else(Occupation == "0009", 99, occ_group)) %>% 
      filter(occ_group != 55) # Leave out military for now due to lack of target data
      
    
  }
  
  return(p_seed)
  
}




#' @title Process group quarters target data from acs supplement file 1
#' 
#' @description 
#' 
#' @param gq_dat data frame returned as element of list from `rsp_get_gq()` with processed data from census table P43
#' @param fips_use fips code to restrict acs data by. 
#' 
#' @details 
#' 
#' @return list containing named elements that match columns names of group quarters variables in seed data. in each list is a data frames where columns match categories of the variables in seed data.
#' @export
#' 
rsp_process_gq_tgt <- function(gq_dat, fips_use){
  gq_tgt <- list()
  
  # Group quarters sex totals --------------
  gq_sex_tgt <- gq_dat %>% 
    filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
    group_by(GEOID, GQ_Sex) %>% 
    summarise(estimate = sum(estimate)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = GQ_Sex, values_from = estimate)
  
  gq_tgt$GQ_Sex <- gq_sex_tgt
  
  # Group quarters age totals --------------
  gq_age_tgt <- gq_dat %>% 
    filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
    group_by(GEOID, GQ_Age) %>% 
    summarise(estimate = sum(estimate)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = GQ_Age, values_from = estimate)
  
  gq_tgt$GQ_Age <- gq_age_tgt
  
# Group quarters type totals --------------
  gq_type_tgt <- gq_dat %>% 
    filter(substr(GEOID, 1, nchar(fips_use)) == fips_use) %>% 
    group_by(GEOID, GQ_Type) %>% 
    summarise(estimate = sum(estimate)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = GQ_Type, values_from = estimate)
  
  gq_tgt$GQ_Type <- gq_type_tgt
  
  # Return target list --------------
  return(gq_tgt)
}


#' @title Process group quarters seed data from pums
#' 
#' @description Processes pums data to extract all those in group quarters and generate variables to match information in census table P43 that serves as target data
#' 
#' @param pums_dat pums dataset, e.g. from `rsp_get_pums()`
#' @param pumas puma areas to include 
#' 
#' @details 
#' 
#' @return data frame containing person pums records for individuals in group quarters 
#' @export
#' 
rsp_process_gq_seed <- function(pums_dat, pumas){
  gq_seed <- pums_dat %>% 
    filter(PUMA %in% pumas) %>% 
    filter(grepl("GQ", SERIALNO)) %>% 
    # Assume those in school or with job are not institutionalized, all others are
    mutate(GQ_Type = if_else(SCHG != "bb" | !OCCP %in% c("0009", "9920"), "NonInst", "Inst"),
           GQ_Age = cut(AGEP,
                        breaks = c(0,18,64,150),
                        right = FALSE,
                        labels = FALSE)) %>% 
    rename("GQ_Sex" = SEX)
  
  return(gq_seed)
}


#' @title Process group quarters synthetic population to get aggregate totals
#' 
#' @description Used to correct person-level targets for group quarters population prior to synthesis. Used in `rsp_hhp_synth()` to correct marginal person totals (target populations) for group quarters population
#' 
#' @param gq_pop Synthesized group quarters population, e.g. from `rsp_gq_synth()`
#' @param chars person-level characteristics to be synthesized. Will be used to aggregate and summarize data in `gq_pop`
#' @param fips_use fips code corresponding to target area to synthesize group quarters population
#' @param p_age_breaks vector of breaks at which to divide age categories, defaults to deciles
#' 
#' @details 
#' 
#' @return list with names as in chars with marginal totals of each characteristic represented in gq population. 
#' @export
#' 
rsp_process_gq_pop <- function(gq_pop, chars, fips_use, p_age_breaks = c(0,10,20,30,40,50,60,70,80,150)){
  
  gq_pop_sums <- list()
  
  gq_pop_fips <- gq_pop %>% 
    mutate(GEOID_use = substr(GEOID, 1, nchar(fips_use))) %>% 
    filter(GEOID_use == fips_use)
  
  if(nrow(gq_pop_fips) > 0){
    if("Age" %in% chars){
      gq_pop_age <- gq_pop_fips %>% 
        mutate(
          # Condense age into categories
          Age = as.factor(cut(AGEP, breaks = p_age_breaks, 
                              labels = FALSE, right = FALSE, include.lowest = TRUE))
        ) %>% 
        dplyr::select(-AGEP) %>% 
        group_by(GEOID_use, Age) %>% 
        summarise(estimate = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = Age, values_from = estimate)
      
      gq_pop_sums$Age <- gq_pop_age
    }
    
    if("Race" %in% chars){
      gq_pop_race <- gq_pop_fips %>% 
        mutate(
          # Condense American Indian and Alaska Native categories to match acs reporting of aggregate totals
          Race = if_else(RAC1P %in% c("3", "4", "5"), "3", RAC1P)
        )%>% 
        dplyr::select(-RAC1P) %>% 
        group_by(GEOID_use, Race) %>% 
        summarise(estimate = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = Race, values_from = estimate)
      
      gq_pop_sums$Race <- gq_pop_race
    } 
    
    if("Ethnicity" %in% chars){
      gq_pop_eth <- gq_pop_fips %>% 
        mutate(
          # make hispanic category numeric
          Hispanic = if_else(HISP == "01", 0, 1)
        )%>% 
        dplyr::select(-HISP) %>% 
        group_by(GEOID_use, Hispanic) %>% 
        summarise(estimate = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = Hispanic, values_from = estimate)
      
      gq_pop_sums$Ethnicity <- gq_pop_eth
    }
    
    if("School_Type" %in% chars){
      gq_pop_scltype <- gq_pop_fips %>% 
        mutate(
          # More informative school type codes
          School_Type = case_when(SCH == "b" ~ "non",
                                  SCH == "1" ~ "non",
                                  SCH == "2" ~ "pub",
                                  SCH == "3" ~ "pvt")
        )%>% 
        dplyr::select(-SCH) %>% 
        group_by(GEOID_use, School_Type) %>% 
        summarise(estimate = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = School_Type, values_from = estimate)
      
      gq_pop_sums$School_Type <- gq_pop_scltype
    }
    
    if("Grade" %in% chars){
      gq_pop_grade <- gq_pop_fips %>% 
        rename("Grade" = SCHG) %>% 
        group_by(GEOID_use, Grade) %>% 
        summarise(estimate = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = Grade, values_from = estimate)
      
      gq_pop_sums$Grade <- gq_pop_grade
    }
    
    if("Sex" %in% chars){
      gq_pop_sex <- gq_pop_fips %>% 
        rename("Sex" = GQ_Sex) %>% 
        group_by(GEOID_use, Sex) %>% 
        summarise(estimate = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = Sex, values_from = estimate)
      
      gq_pop_sums$Sex <- gq_pop_sex
    }
    
    if("Occupation" %in% chars){
      gq_pop_occp <- gq_pop_fips %>% 
        rename("Occupation" = OCCP) %>% 
        # Merge with lookup table which relates acs and occp codes then add code 9 for unemployed/retired/in school
        left_join(acs_occp_lookup %>% dplyr::select(Code, occ_group),
                  by = c("Occupation" = "Code")) %>% 
        mutate(occ_group = if_else(Occupation == "0009", 99, occ_group)) %>% 
        filter(occ_group != 55) %>% 
        group_by(GEOID_use, occ_group) %>% 
        summarise(estimate = n()) %>% 
        ungroup() %>% 
        pivot_wider(names_from = occ_group, values_from = estimate)
      
      gq_pop_sums$Occupation <- gq_pop_occp
    }
    
  } else {
    gq_pop_sums <- NULL
  }
  
  return(gq_pop_sums)
  
}