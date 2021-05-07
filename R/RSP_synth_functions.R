#---------------------------------------------
# Rsynthpops synthesize population functions
# Chris Hoover April 2021 
#---------------------------------------------

#' @title Synthesize group quarters
#' 
#' @description 
#' 
#' @param fips_use fips code corresponding to target area to synthesize group quarters population
#' @param gq_seed seed data frame for group quarters population 
#' @param gq_tgt target list of data frames with names corresponding to characteristics in columns of seed for group wuarters population
#' 
#' @details 
#' 
#' @return data frame of synthesized population with all characteristics contained in `gq_seed` maintained
#' @export
#' 
rsp_gq_synth <- function(fips_use, gq_seed, gq_tgt){
  fips_puma    <- cts_to_pumas %>% 
    filter(substr(CTFP, 1, nchar(fips_use)) == fips_use) %>% 
    pull(PUMA5CE)
  
  fips_gq_seed  <- gq_seed %>% 
    filter(PUMA == fips_puma) %>% 
    dplyr::select(-PUMA)
  
  fips_gq_tgt <- lapply(gq_tgt, function(i){
    i %>% 
      filter(substr(GEOID,1,nchar(fips_use)) == fips_use) %>% 
      dplyr::select(-GEOID)
  })
  
  if(sum(fips_gq_tgt[[1]][1,]) > 0){
    fips_ipu <- ipu(fips_gq_seed, fips_gq_tgt)
    fips_syn_gq <- synthesize(fips_ipu$weight_tbl, primary_id="SERIALNO") %>% 
      mutate(GEOID = fips_use,
             p_id  = paste0(GEOID, "GQ", new_id))
  } else {
    fips_syn_gq <- NULL
  }
  
  
  return(fips_syn_gq)
  
}


#' @title Synthesize population
#' 
#' @description 
#' 
#' @param fips_use fips code corresponding to target area to synthesize group quarters population
#' @param hh_seed seed data frame for households
#' @param hh_tgt target list of data frames with names corresponding to characteristics in columns of seed for households 
#' @param p_seed seed data frame for persons
#' @param p_tgt target list of data frames with names corresponding to characteristics in columns of seed for persons
#' @param gq_pop synthesized group quarters population. if included, characteristics will be summarised and subtracted from the marginal (target) person totals of all characteristics
#' 
#' @details 
#' 
#' @return data frame of synthesized population with all characteristics contained in `gq_seed` maintained
#' @export
#' 
rsp_hhp_synth <- function(fips_use, hh_seed, p_seed, hh_tgt, p_tgt, gq_pop = NULL){
  fips_puma    <- cts_to_pumas %>% 
    filter(substr(CTFP, 1, nchar(fips_use)) == fips_use) %>% 
    pull(PUMA5CE)
  
  fips_p_seed  <- p_seed %>% 
    filter(PUMA == fips_puma) %>% 
    dplyr::select(-PUMA)
  
  p_ids <- fips_p_seed$SERIALNO
  fips_hh_seed <- hh_seed %>% 
    # Make sure all households have corresponding individuals in seed data  
    filter(PUMA == fips_puma & SERIALNO %in% p_ids) %>% 
    dplyr::select(-PUMA)
  
  fips_hh_tgt <- lapply(hh_tgt, function(i){
    i %>% 
      mutate(GEOID_use = substr(GEOID, 1, nchar(fips_use))) %>% 
      filter(GEOID_use == fips_use) %>% 
      dplyr::select(-GEOID)
  })
  
  fips_p_tgt <- lapply(p_tgt, function(j){
    j %>% 
      mutate(GEOID_use = substr(GEOID, 1, nchar(fips_use))) %>% 
      filter(GEOID_use == fips_use) %>% 
      dplyr::select(-GEOID)
  })
  
  if(!is.null(gq_pop)){
    person_chars <- names(fips_p_tgt)
    
    gq_sums <- rsp_process_gq_pop(gq_pop, person_chars, fips_use)
    
    if(!is.null(gq_sums)){
      
    hh_tgt <- lapply(person_chars, function(i){
      tgt_gq_merge <- hh_tgt[[i]] %>% 
        pivot_longer(all_of(colnames(.)[-1]),
                     names_to = "Var",
                     values_to = "Val") %>% 
        left_join(gq_sums[[i]] %>% 
                    pivot_longer(all_of(colnames(.)[-1]),
                                 names_to = "Var",
                                 values_to = "Val"), by = c("GEOID_use", "Var"),
                  suffix = c("_tgt", "_gq")) %>% 
        mutate(Val_Crctd = Val_tgt - Val_gq) %>% 
        dplyr::select(GEOID_use, Var, Val_Crctd) %>% 
        pivot_wider(names_from = Var, values_from = Val_Crctd)
      })
    }
  }
  
  
  fips_ipu <- ipu(hh_seed, hh_tgt, p_seed, p_tgt, primary_id="SERIALNO")
  fips_syn_h <- synthesize(fips_ipu$weight_tbl, primary_id="SERIALNO")
  fips_syn_p <- left_join(fips_syn_h, p_seed, by="SERIALNO") %>%
    rename(house_id=new_id) %>% 
    mutate(GEOID = fips_use,
           p_id  = paste0(GEOID, new_id, SPORDER))
  
  return(fips_syn_p)
  
}
