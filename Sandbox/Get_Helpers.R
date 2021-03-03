
cts_to_pumas <- read_csv("https://www2.census.gov/geo/docs/maps-data/data/rel/2010_Census_Tract_to_2010_PUMA.txt")
cts_to_pumas$STCNTYFP <- paste0(cts_to_pumas$STATEFP, cts_to_pumas$COUNTYFP)
cts_to_pumas$CTFP <- paste0(cts_to_pumas$STATEFP, cts_to_pumas$COUNTYFP, cts_to_pumas$TRACTCE)

state.abb2 <- data.frame("STATE" = state.abb, STATEFP = c(1:50))
state.abb2$STATEFP = if_else(nchar(state.abb2$STATEFP) == 1,paste0("0",state.abb2$STATEFP),as.character(state.abb2$STATEFP))

cts_to_pumas <- left_join(cts_to_pumas, state.abb2, by = "STATEFP")

usethis::use_data(cts_to_pumas, overwrite = T)
