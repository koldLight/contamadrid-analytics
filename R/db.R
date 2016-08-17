library(RPostgreSQL)
source("conf/conf.R")

open.clousr.db.connection <- function() {
  drv <- dbDriver("PostgreSQL")
  return(dbConnect(drv, dbname=G.DB.DATABASE, host=G.DB.HOST,
                   port=G.DB.PORT,user=G.DB.USER,password=G.DB.PASS))
}

close.connection <- function(con) {
  dbDisconnect(con)
}