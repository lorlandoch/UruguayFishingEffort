#setwd("/home/luis/Documents/Pesca/Esfuerzo")
#rm(list=ls())
library(patchwork)
library(maps)
library(gfwr)
library(sf)
library(leaflet)
library(purrr)
library(tidyverse)

### select the curated fleet file
#flotaid<-file.choose()

flotaid<- read.table("flota_id2.csv", dec=".", sep=",", header=T)
flota<- flotaid %>%  filter(categ=="A")
flotb<- flotaid %>%  filter(categ=="B")
unique(flota$buque)

#### port fleet A ####

## load the info 
porta <- get_event(event_type = "PORT_VISIT",
                   vessels = flota$vesselId,
                   start_date = "2024-01-01",
                   end_date = "2024-12-31")
names(porta)
head(porta)

## keep duration,confidence,position and month of each port event
tpuerta <- porta %>%
  mutate(
    durationHrs = as.numeric(purrr::map(event_info, "durationHrs", .default = NA_real_) ),
    month = month (start),
    confidence  = as.numeric(purrr::map(event_info, "confidence", .default = NA_real_))) %>% 
    dplyr::select(durationHrs, confidence, vesselId, lat, lon, month)  

head(tpuerta)

### spatial check dinarama geo_check returns 1 when position is inside ports
source("geo_check1_1.R")
check<-with(tpuerta, geo_check(lat=lat, long=lon, poly="ports"))

check
tpuerta<- tpuerta[check==1,]

### days in port, add vessel name
port_tima<- tpuerta %>% 
  mutate(days=durationHrs/24) %>% 
  left_join(flota %>% dplyr::select(vesselId, buque), by="vesselId") %>% 
  dplyr::select(-vesselId)

table(port_tima$confidence)

##menthly summary, apply cap to maximum days in month
port_mesa<- port_tima %>% group_by(buque, month) %>% 
  summarise(days= sum(days)) %>% 
  mutate(daysmax=days_in_month(month), 
         dys=case_when(days>daysmax ~ daysmax,
                                TRUE ~ days)) 

port_mesa

## visualization 
plotmesa<- port_mesa %>% group_by(month) %>%
  summarise(meandys=mean(dys), maxdys=mean(daysmax) ) %>% 
  mutate(perc=(meandys/maxdys)*100, month=factor(month))
ggplot(plotmesa)+
  geom_col(aes(x=month, y=rep(100,12)) )+
  geom_col(aes(x=month, y=perc, fill="port"))
  
plotmesa

#### Fleet B ####
portb <- get_event(event_type = "PORT_VISIT",
                   vessels = flotb$vesselId,
                   start_date = "2024-01-01",
                   end_date = "2024-12-31")
names(portb)

## genero base con tiempo de puerto y confianza de cada evento
tpuertb <- portb %>%
  mutate(
    durationHrs = as.numeric(purrr::map(event_info, "durationHrs", .default = NA_real_) ),
    month = month (start),
    confidence  = as.numeric(purrr::map(event_info, "confidence", .default = NA_real_))) %>% 
  dplyr::select(durationHrs, confidence, vesselId, lat, lon, month)  

head(tpuertb)

###dinarama geo_check
#source("geo_check1_1.R")
check<-with(tpuertb, geo_check(lat=lat, long=lon, poly="ports"))
check
tpuertb<- tpuertb[check==1,]

### calculo dias en puerto, agrego nombre de buque
port_timb<- tpuertb %>% 
  mutate(days=durationHrs/24) %>% 
  left_join(flotb %>% dplyr::select(vesselId, buque), by="vesselId") %>% 
  dplyr::select(-vesselId)

port_timb

##resumen mensual
port_mesb<- port_timb %>% group_by(buque, month) %>% 
  summarise(days= sum(days)) %>% 
  mutate(daysmax=days_in_month(month), 
         dys=case_when(days>daysmax ~ daysmax,
                       TRUE ~ days)) 

port_mesb

plotmesb<- port_mesb %>% group_by(month) %>%
  summarise(meandys=mean(dys), maxdys=mean(daysmax) ) %>% 
  mutate(perc=(meandys/maxdys)*100)
plotmesb<- rbind(plotmesb, c(1, 0, 31, 0))
plotmesb<- plotmesb %>% arrange(month) %>% mutate (month=factor(month))
plotmesb

ggplot(plotmesb)+
  geom_col(aes(x=month, y=rep(100,12)) )+
  geom_col(aes(x=month, y=perc, fill="port"))
