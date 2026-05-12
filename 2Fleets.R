#rm(list=ls())
library(tidyverse)
library(gfwr)
library(sf)
library(purrr)
library(stringdist)
library(leaflet)


buques<- read.table("buques.csv", dec=".", sep=",", header = T)
head(buques)

bes <- tibble(buque = buques$buque)

#### function to download vessel registries
safe_get_vessel <- safely(function(name) {
  get_vessel_info(
    where = paste0("shipname LIKE '%", name, "%' AND flag = 'URY' ",
                   "AND callsign LIKE 'CX%'"),
    search_type = "search"
  )
})

# this might take a while
search_results <- map(bes$buque, ~ safe_get_vessel(.x))
search_results[1]

## access info on the list combinedSourcesInfo
combined_info_list <- map(search_results, function(x) {
  res <- x$result
  if (!is.null(res$combinedSourcesInfo) && nrow(res$combinedSourcesInfo) > 0) {
    return(as_tibble(res$combinedSourcesInfo))
  } else {
    return(NULL) 
  }
})

combined_info_df <- bind_rows(combined_info_list, .id = "index")
head(combined_info_df)

unique(combined_info_df$geartypes_name)

## filter fishing vessels
pesqueros<-combined_info_df %>% 
        filter(geartypes_name %in% c("TRAWLERS", "SET_LONGLINES","FISHING", 
      "POTS_AND_TRAPS","PURSE_SEINES", "DRIFTING_LONGLINES","DREDGE_FISHING",
      "FIXED_GEAR", "POLE_AND_LINE"))

## filter year of interest and keep vesselID only
pesqueros<- pesqueros %>% filter (geartypes_yearFrom<=2024 & geartypes_yearTo>=2024) %>% 
  dplyr::select(vesselId)

## remove duplicates
pesqueros<- unique(pesqueros)        


## access info of list selfReportedInfo
self_info_list <- map(search_results, function(x) {
    res <- x$result
    if (!is.null(res$selfReportedInfo) && nrow(res$selfReportedInfo) > 0) {
    return(as_tibble(res$selfReportedInfo))
  } else {
    return(NULL)
  }
})

self_info_df <- bind_rows(self_info_list, .id = "index")
head(self_info_df)

## filter by year
index<- year(self_info_df$transmissionDateFrom)<=2024 & year(self_info_df$transmissionDateTo)>=2024
buques_gfw<-self_info_df[index,]

##filter flags
unique(buques_gfw$flag)
buques_gfw<- buques_gfw %>% filter(flag=="URY")

## filter fishing vessels (those included in "pesqueros")
buques_gfw<- buques_gfw %>% filter (vesselId %in% pesqueros$vesselId)
head(buques_gfw)

##
fleets <- merge(buques, buques_gfw, by = NULL)

# distance among names
fleets$name_dist <- stringdist(fleets$shipname, fleets$buque, method = "jw") # Jaro-Winkler
fleets$callsign_match <- fleets$callsign.x == fleets$callsign.y

# create a score (lower is better)
fleets$score <- fleets$name_dist + ifelse(fleets$callsign_match, 0, 0.5)
head(fleets)

# Take the best match for each vesselID on buques_gfw
fleets <- fleets %>%
  group_by(vesselId) %>%
  arrange(score) %>%   
  slice(1) %>%              
  ungroup()

head(fleets)

fleet_id <-fleets %>% dplyr::select(vesselId, shipname, buque, categ,
                             transmissionDateFrom, transmissionDateTo, score)
fleet_id<- fleet_id %>% arrange(buque, score)

### write the info and hand check the information
write.table(flota_id, "flota_id.csv", dec=".", sep=";", row.names =F )
