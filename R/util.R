##########################################################################
# Luz Frias, 2016-07-03
# Utils
##########################################################################

# Currently working for data.table
# The NA values will be replaced with the closest row value
fix_NAs_closest_value <- function(dt, colname) {
  # Set the index number
  dt[, ind := .I]
  
  # Split by valid and invalid rows
  invalid.rows <- dt[ is.na(get(colname))]
  valid.rows   <- dt[!is.na(get(colname))]
  
  for (i in invalid.rows$ind) {
    # Calculate the distance to the row with the NA value
    valid.rows[, distance := abs(i - ind)]
    
    # Fix with the mean value, in case there are several values with the same distance
    closest <- mean(valid.rows[distance == min(valid.rows$distance), get(colname)])
    dt[ind == i, eval(colname) := closest]
  }
  
  dt[, ind := NULL]
  dt
}