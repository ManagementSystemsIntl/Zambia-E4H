
source("scripts/r prep.R")
library(sf)
library(crsuggest)
library(rgeoboundaries)
library(geodata)

#read in admin boundaris

ZMB <- geoboundaries(country = "ZMB"
                     , adm_lvl = "adm1")

#Select a projection
CRS_options <- suggest_crs(ZMB) #4415

CRS <- 4415

#Transform the shape
ZMB <- st_transform(ZMB, CRS = CRS)


#read in data
dat_mat <- readxl::read_xls("data/Downlaod Extract Maternal Yearly By Province.xls")



