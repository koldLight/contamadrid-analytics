##########################################################################
# Luz Frias, 2016-06-12
# Gets contamination data
##########################################################################

library(data.table)

source("R/air-data.R")
source("R/meteo-data.R")
source("R/traffic-data.R")

download_historical_air_data()
download_meteo_data()
download_traffic_data()
