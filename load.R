##########################################################################
# Luz Frias, 2016-06-12
# Gets contamination data
##########################################################################

source("R/air-data.R")
source("R/meteo-data.R")
source("R/traffic-data.R")
source("R/calendar-data.R")

# Historical data
download_historical_air_data()
download_meteo_data()
download_traffic_data()
build_calendar_data()

# Load into DB
load_historical_air_data_into_db()

# Incremental data
download_current_day_air_data()
