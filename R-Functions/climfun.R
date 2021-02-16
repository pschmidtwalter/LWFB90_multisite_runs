#' Function to fetch and process climatic data for one site, for use in \code{\link[LWFBrook90R]{runLWFB90}}.
#'
#' @param con A database connection object of a database with climatic data.
#' @param tbl The table that contains climatic data.
#' @param site_id Character ID of the site for which data will be extracted.
#'
#' @return
#' @export
#' @examples
#'
#' \dontrun{
#' library(RSQLite)
#' db <- dbConnect(SQLite(), "NFIWADS_input.sqlite")
#' dbListTables(db)
#' dbListFields(db, "climate_daily_int100_sample")
#' climfun(db, tbl = "climate_daily_int100_sample", site_id = "12126_2")
#' }
climfun <- function(con, tbl, site_id){
  #get data from connection con
  qry <- paste0("select * from ", tbl, " where id = '", site_id, "';")
  clim <- data.table(dbGetQuery(con, qry))

  # process data to be usable as 'climate' in LWFBrook90R::run_LWFB90()
  # use suitable names
  setnames(clim, c("grhds", "rrds","sddm","tadm", "tadn", "tadx","wsdm"),
           c("globrad", "prec","vpd","tmean", "tmin", "tmax","windspeed"))
  # convert units
  clim[, c("globrad", "prec","vpd","tmean", "tmin", "tmax","windspeed") :=
         list(globrad/100, prec/100,vpd/100,tmean/100, tmin/100, tmax/100,windspeed/100)]

  # create date variable
  clim[,dates := as.Date(paste(year, month, day, sep="-"))]

  # calculate vappres from vpd
  clim[, es :=  ifelse(tmean >= 0,
                       6.1 * 10^( (7.5 * tmean) / (tmean + 237.2) ) ,
                       6.1 * 10^( (9.5*tmean) / (tmean + 265.5) ) )]
  clim[, vappres := ifelse( (es - vpd) <0, 0, (es - vpd)*0.1)] #vpd und es in hPa

  # return sorted
  clim[order(dates),list(dates, tmin, tmax, tmean, globrad, vappres, prec, windspeed)]
}
