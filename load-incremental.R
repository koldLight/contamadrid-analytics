##########################################################################
# Luz Frias, 2016-06-12
# Incremental data load
##########################################################################

source("R/air-data.R")

# Incremental data
download_current_day_air_data()
load_current_air_data_into_db()
