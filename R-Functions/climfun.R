climfun <- function(con, tbl, site_id){
  #get data from connection con
  qry <- paste0("select * from ", tbl, " where id = '", site_id, "';")
  clim <- data.table(dbGetQuery(con, qry))

  # process data to be usable as 'climate' in LWFBrook90R::runLWFB90()
  # use suitable names
  setnames(clim, c("grhds", "rrds","sddm","tadm", "tadn", "tadx","wsdm"),
           c("globrad", "prec","vpd","tmean", "tmin", "tmax","wind"))
  # convert units
  clim[, c("globrad", "prec","vpd","tmean", "tmin", "tmax","wind") :=
         list(globrad/100, prec/100,vpd/100,tmean/100, tmin/100, tmax/100,wind/100)]

  # create date variable
  clim[,dates := as.Date(paste(year, month, day, sep="-"))]

  # calculate vappres from vpd
  clim[, es :=  ifelse(tmean >= 0,
                       6.1 * 10^( (7.5 * tmean) / (tmean + 237.2) ) ,
                       6.1 * 10^( (9.5*tmean) / (tmean + 265.5) ) )]
  clim[, vappres := ifelse( (es - vpd) <0, 0, (es - vpd)*0.1)] #vpd und es in hPa

  # return sorted
  clim[order(dates),list(dates, tmin, tmax, tmean, globrad, vappres, prec, wind)]
}
