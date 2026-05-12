#rm(list=ls())
#library(usethis)
#usethis::edit_r_environ()
#remotes::install_github("GlobalFishingWatch/gfwr", dependencies = TRUE)
#install.packages("rnaturalearth")
#install.packages("rnaturalearthdata")
#install.packages("curl")
#library(curl)

library(tidyverse)
library(gfwr)
library(sf)
library(pdftools)
library(readxl)

#### 1.1 Local list Uruguayan Fishing vessels ####

## Download: “Vigencia permisos de pesca industriales.pdf” 
### https://www.gub.uy/ministerio-ganaderia-agricultura-pesca/politicas-y-gestion/vigencia-permisos-pesca-comerciales-listados

# select pdf and read
permits<-pdf_text(file.choose())

# Combine all pages and split into lines
lines <- unlist(strsplit(paste(permits, collapse = "\n"), "\n"))

#remove headers and empty lines
lines <- trimws(lines)
lines <- lines[lines != "" & !grepl("^Buque pesquero|^Permisos|^m$", lines)]
lines

# Filter lines that contain a category and date (start of a new row)
data <- lines[grepl("\\b[A-C]\\b.*\\d{2}-\\d{2}-\\d{2}", lines)]

# Split using 2+ spaces as separator
data <- strsplit(data, "\\s{2,}")

# Convert to data.frame
df <- do.call(rbind, lapply(data, function(x) x[c(1,3,4)]))
colnames(df) <- c("digitado", "categ", "venc")
df <- as.data.frame(df, stringsAsFactors = FALSE)
df


write.table(df, "permisos.csv", row.names=F, dec=".", sep=";")
# open csv y edit mannualy checking for error
rm(data, df, lines, permits )


#### 1.2 Cross info with CTMFM ####
permit<- read.table( "permisos.csv", header=T, dec=".", sep=",")

## download info from uruguayan vessels on: 
### https://ctmfm.org/buques-autorizados-en-zcp/buques-uruguayos/

# temp:"FLOTA-PESQUERA-URUGUAY-04-2025.xlsx"
ctmfm<-read_excel("FLOTA-PESQUERA-URUGUAY-04-2025.xlsx")
colnames(ctmfm)<- c("buque_ctmfm", "matricula", "eslora")

## the local vessel list was created combining the information available
write.table(datos_buques, "buques.csv", dec=".", sep=";", row.names = F)
