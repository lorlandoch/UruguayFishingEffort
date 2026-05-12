#rm(list=ls())
library(patchwork)
library(maps)
library(gfwr)
library(sf)
library(leaflet)
library(purrr)
library(tidyverse)

### select fleet from file
#flotaid<-file.choose()

flotaid<- read.table("flota_id2.csv", dec=".", sep=",", header=T)
flota<- flotaid %>%  filter(categ=="A")
flotb<- flotaid %>%  filter(categ=="B")

#### h of fishing fleet A ####
#rm(list=ls())

pesca <- get_event(event_type = "FISHING",
                   vessels = flota$vesselId,
                   start_date = "2024-01-01",
                   end_date = "2024-12-31")

names(pesca)

## database with fishing time, position and date, speed
tpesca<-pesca %>% 
mutate(tpesca=as.numeric(difftime(end, start, units="hours")),
dist = as.numeric(purrr::map(event_info, "totalDistanceKm", .default = NA_real_) ),
knots  = as.numeric(purrr::map(event_info, "averageSpeedKnots", .default = NA_real_))) %>% 
dplyr::select(tpesca, vesselId, lat, lon, start, dist, knots, vessel_name)

par(mfrow=c(2,2), mar=c(1,1,1,1))
hist(tpesca$knots, breaks=30, main= "Fishing speed", xlim=c(0,10) )
hist(tpesca$tpesca, breaks=30, main= "Fishing time" , xlim=c(0,250))


#filter by speed and duration
tpesca<-tpesca %>% filter(tpesca<8 & tpesca>0.5 & knots<5 & knots>3) 
sum(tpesca$tpesca)
unique(tpesca$vessel_name)
head(tpesca)

hist(tpesca$knots, breaks=30, main= NULL, xlim=c(0,10))
hist(tpesca$tpesca, breaks=30, main= NULL, xlim=c(0,250))
par(mfrow=c(1,1))


#visualizarion
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng= tpesca$lon, lat=tpesca$lat, popup = tpesca$start)

#### h arrastre B ####
pescb <- get_event(event_type = "FISHING",
                   vessels = flotb$vesselId,
                   start_date = "2024-01-01",
                   end_date = "2024-12-31")

names(pescb)

## genero base con tiempo de pesca, posicion y fecha
tpescb<-pescb %>% 
  mutate(tpesca=as.numeric(difftime(end, start, units="hours")),
         dist = as.numeric(purrr::map(event_info, "totalDistanceKm", .default = NA_real_) ),
         knots  = as.numeric(purrr::map(event_info, "averageSpeedKnots", .default = NA_real_))) %>% 
  dplyr::select(tpesca, vesselId, lat, lon, start, dist, knots, vessel_name)

hist(tpescb$knots, breaks=30)
hist(tpescb$tpesca, breaks=30)

#filtrar por velocidad y duracion de los eventos de pesca
tpescb<-tpescb %>% filter(tpesca<8 & tpesca>0.5 & knots<5 & knots>3)

tpescb %>% group_by(month(start)) %>% summarise(x=sum(tpesca))

#visualizar
leaflet() %>%
  addTiles() %>%
  addCircleMarkers(lng= tpescb$lon, lat=tpescb$lat, popup = tpescb$vessel_name)


paste ("B mean hawl time(h)", mean(tpescb$tpesca))
paste ("B total trawling time (h)", sum(tpescb$tpesca))
paste ("B total trawled distance(km) (pair fishing)", sum(tpescb$dist))

paste ("A mean hawl time(h)", mean(tpesca$tpesca))
paste ("A total trawling time (h)", sum(tpesca$tpesca))
paste ("A total trawled distance(km)", sum(tpesca$dist))

source("posicion1_1.R")
source("inv_posicion1_1.R")


##### effort by cuadrant and season ####
cpesca<- cbind(tpesca,
           posicion(lat=tpesca$lat, long=tpesca$lon, subcuad=T)[,3:4])

cpesca<- cpesca %>% mutate(month=month(start)) %>% 
          group_by( cuad, subcuad, month) %>% 
          summarise(tpesca=sum(tpesca))
cpesca<- cbind(cpesca, 
               inv_posicion(cpesca$cuad, cpesca$subcuad))
cpesca<- cpesca %>% mutate(trim= case_when( month<4 ~1,
                                   month>3 & month<7 ~2,
                                   month>6 & month<10 ~3,
                                   month>9 ~4))
cpesca %>% group_by(trim) %>% summarise(c=sum(tpesca))

totality<-sum(cpesca$tpesca)
cpesca<- cpesca %>% mutate(rtpesca=(tpesca/totality)*100)

### distance by season
dpesca<- cbind(tpesca,
  posicion(lat=tpesca$lat, long=tpesca$lon, subcuad=T)[,3:4]) %>% 
  mutate(month=month(start), trim= case_when( month<4 ~1,
                                              month>3 & month<7 ~2,
                                              month>6 & month<10 ~3,
                                              month>9 ~4)) %>% 
  group_by(trim) %>% 
    summarise(dpesca=sum(dist))
dpesca

#### maps
## sudamerica base
SA <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Ecuador",
        "Guyana", "Paraguay", "Peru", "Suriname", "Uruguay", "Venezuela", "Falkland Islands")
SA <- giscoR::gisco_get_countries(country = SA)
SA <- sf::st_union(SA$geometry)
plot(SA)


pescat<- cpesca %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))
ttt<-paste0( round(sum(pescat$tpesca),0), "(h)")

  pesca1<- cpesca %>% filter(trim==1) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))

pesca2<- cpesca %>% filter(trim==2) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))


pesca3<- cpesca %>% filter(trim==3) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))

pesca4<- cpesca %>% filter(trim==4) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))

## common fill
cfill<-scale_fill_gradient( 
      limits=range(min(pescat$tpesca), max(pescat$tpesca) ),
      low = "#e8e814",
      high = "red")

pat<-ggplot(data=pescat) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title=paste("Total"))+
  ylab('Latitude') + xlab('Longitude')+
  coord_sf(ylim=c(-42, max(pescat$latR)),
           xlim=c(min(pescat$longR), max(pescat$longR)))

pa1<-ggplot(data=pesca1) +
    geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
    cfill+
    geom_sf(data=SA, fill="grey60" )+
    theme_bw()+
    labs(fill = "Fishing effort (%)", title= paste("Summer ") )+
    ylab('Latitude') + xlab(NULL)+
    theme(legend.position="none")+
    coord_sf(ylim=c(-42, max(pescat$latR)),
             xlim=c(min(pescat$longR), max(pescat$longR)))

pa2<-ggplot(data=pesca2) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title= paste("Autum") )+
  ylab(NULL) + xlab(NULL)+
  theme(legend.position="none")+
  coord_sf(ylim=c(-42, max(pescat$latR)),
           xlim=c(min(pescat$longR), max(pescat$longR)))

pa3<-ggplot(data=pesca3) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title= paste("Winter ") )+
  ylab('Latitude') + xlab('Longitude')+
  theme(legend.position="none")+
  coord_sf(ylim=c(-42, max(pescat$latR)),
           xlim=c(min(pescat$longR), max(pescat$longR)))

pa4<-ggplot(data=pesca4) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title= paste("Spring ") )+
  ylab(NULL) + xlab('Longitude')+
  theme(legend.position="none")+
  coord_sf(ylim=c(-42, max(pescat$latR)),
           xlim=c(min(pescat$longR), max(pescat$longR)))

layout <- "
AAAABBCC
AAAADDEE
"

pat + pa1 + pa2 + pa3 + pa4 +
  plot_layout(
    design = layout,
    guides = "collect"  
  ) &
  theme(legend.position = "right")

##### esfuerzB por cuadrante/estacion ####
cpescb<- cbind(tpescb,
               posicion(lat=tpescb$lat, long=tpescb$lon, subcuad=T)[,3:4])

cpescb<- cpescb %>% mutate(month=month(start)) %>% 
  group_by( cuad, subcuad, month) %>% 
  summarise(tpesca=sum(tpesca))
cpescb<- cbind(cpescb, 
               inv_posicion(cpescb$cuad, cpescb$subcuad))
cpescb<- cpescb %>% mutate(trim= case_when( month<4 ~1,
                                            month>3 & month<7 ~2,
                                            month>6 & month<10 ~3,
                                            month>9 ~4))
cpescb %>% group_by(trim) %>% summarise(x=sum(tpesca))

totality<-sum(cpescb$tpesca)
cpescb<- cpescb %>% mutate(rtpesca=(tpesca/totality)*100)


### distancia x estacion
dpescb<- cbind(tpescb,
               posicion(lat=tpescb$lat, long=tpescb$lon, subcuad=T)[,3:4]) %>% 
  mutate(month=month(start), trim= case_when( month<4 ~1,
                                              month>3 & month<7 ~2,
                                              month>6 & month<10 ~3,
                                              month>9 ~4)) %>% 
  group_by(trim) %>% 
  summarise(dpescb=sum(dist))

sum(dpescb$dpescb)

#### mapas
pescbt<- cpescb %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))

pescb1<- cpescb %>% filter(trim==1) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))

pescb2<- cpescb %>% filter(trim==2) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(na.omit(rtpesca)))

pescb3<- cpescb %>% filter(trim==3) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(rtpesca))

pescb4<- cpescb %>% filter(trim==4) %>% 
  group_by (latR, longR) %>%
  summarise(tpesca=sum(na.omit(rtpesca)))

## common fill
cfill<-scale_fill_gradient( 
  limits=range(min(pescbt$tpesca), max(pescbt$tpesca) ),
  low = "#e8e814",
  high = "red")

pbt<-ggplot(data=pescbt) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title=paste("Total"))+
  ylab('Latitude') + xlab('Longitude')+
  coord_sf(ylim=c(-38, -33),
           xlim=c(-59, -53))

pb1<-ggplot(data=pescb1) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title= paste("Summer ") )+
  ylab('Latitude') + xlab('Longitude')+
  theme(legend.position="none")+
  coord_sf(ylim=c(-38, -33),
           xlim=c(-59, -53))

pb2<-ggplot(data=pescb2) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey90" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title=paste("Autum"))+
  ylab('Latitude') + xlab('Longitude')+
  theme(legend.position="none")+
  coord_sf(ylim=c(-38, -33),
           xlim=c(-59, -53))

pb3<-ggplot(data=pescb3) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title=paste("Winter"))+
  ylab('Latitude') + xlab('Longitude')+
  theme(legend.position="none")+
  coord_sf(ylim=c(-38, -33),
           xlim=c(-59, -53))

pb4<-ggplot(data=pescb4) +
  geom_tile(aes(x=longR, y=latR, fill=tpesca ))+
  cfill+
  geom_sf(data=SA, fill="grey60" )+
  theme_bw()+
  labs(fill = "Fishing effort (%)", title=paste("Spring"))+
  ylab('Latitude') + xlab('Longitude')+
  theme(legend.position="none")+
  coord_sf(ylim=c(-38, -33),
           xlim=c(-59, -53))

layout <- "
AAAABBCC
AAAADDEE
"

pbt + pb1 + pb2 + pb3 + pb4 +
  plot_layout(
    design = layout,
    guides = "collect"   # ensures only one legend
  ) &
  theme(legend.position = "right")
