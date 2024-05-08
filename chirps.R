install.packages("chirps")
library("chirps")
library(sf)
library(dplyr)
load('output_data/data.Rda')   #produced after 0_prepare_data.R script

pts<-st_centroid(mun)




dat <- get_chirps(pts,
                  dates = c("2013-01-01","2013-12-31"), 
                  server = "ClimateSERV")
