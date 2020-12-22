#' Function to fetch and process climatic data for one site, for use in \code{\link[LWFBrook90R]{runLWFB90}}.
#'
#' @param con A database connection object of a database with climatic data.
#' @param tbl The table that contains climatic data.
#' @param site_id Character ID of the site for which data will be extracted.
#' @param id_fieldnm name of the column in which to look up\code{site_id}.
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
#' climfun(db, tbl = "climate_daily_int100_sample", site_id = "12126_2", id_fieldnm = "id")
#' }
climfun <- function(con, tbl, site_id, id_fieldnm){
  #get data
  qry <- paste0("select * from ", tbl, " where ", id_fieldnm, " = '", site_id, "';")
  clim <- data.table(dbGetQuery(con, qry))

  # process data to be usable as 'climate' in LWFBrook90R::runLWFB90()
  setnames(clim, c("grhds", "rrds","sddm","tadm", "tadn", "tadx","wsdm"),c("globrad", "prec","vpd","tmean", "tmin", "tmax","wind"))
  clim[, c("globrad", "prec","vpd","tmean", "tmin", "tmax","wind") :=
         list(globrad/100, prec/100,vpd/100,tmean/100, tmin/100, tmax/100,wind/100)]
  clim[,dates := as.Date(paste(year, month, day, sep="-"))]
  clim[, es :=  ifelse(tmean >= 0,
                       6.1 * 10^( (7.5 * tmean) / (tmean + 237.2) ) ,
                       6.1 * 10^( (9.5*tmean) / (tmean + 265.5) ) )]
  clim[, vappres := ifelse( (es - vpd) <0, 0, (es - vpd)*0.1)] #vpd und es in hPa
  clim[order(dates),list(dates, tmin, tmax, tmean, globrad, vappres, prec, wind)]
}
