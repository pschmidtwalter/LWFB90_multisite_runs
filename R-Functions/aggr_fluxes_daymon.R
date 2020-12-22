#' Aggregate daily values of evapday.asc to monthly values
#'
#' @param dat data.frame or data.table from runLWFB90, daily_output
#'
#' @return a data.table with monthly aggregated values and stress-indicators
#' integrating over monthly intervals
#' @export
fluxes_dailytomonthly <- function(dat) {
  setDT(dat)
  dat_month <- dat[,list(evap= sum(evap),
                         tran = sum(tran),
                         irvp = sum(irvp),
                         isvp = sum(isvp),
                         slvp=sum(slvp),
                         snvp = sum(snvp),
                         flow = sum(flow),
                         byfl = sum(byfl),
                         dsfl = sum(dsfl),
                         ptran = sum(ptran),
                         pslvp = sum(pslvp),
                         tdiff = round(sum(ptran-tran),1),

                         TRATIO_avg = round(mean(ifelse(ptran>0, tran/ptran,1)),3),
                         TRATIO_min = round(min(ifelse(ptran>0, tran/ptran,1)),3),
                         Days_TRATIO_lower50 = sum((ifelse(ptran > 0, tran / ptran, 1)) < 0.5),
                         Durations_TRATIO_lower50 = paste((rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.5)
                                                           $lengths[rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.5)$values]), collapse = " "),
                         Defsum_TRATIO_lower50 = round(sum((1 - (ifelse(ptran > 0, tran / ptran, 1) / 0.5)) * (ifelse(ptran > 0, tran / ptran, 1) < 0.5)),3),

                         Days_TRATIO_lower80 = sum((ifelse(ptran > 0, tran / ptran, 1)) < 0.8),
                         Durations_TRATIO_lower80 = paste((rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.8)
                                                           $lengths[rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.8)$values]), collapse = " "),
                         Defsum_TRATIO_lower80 = round(sum((1 - (ifelse(ptran > 0, tran / ptran, 1) / 0.8)) * (ifelse(ptran > 0, tran / ptran, 1) < 0.8)),3)
  ),
  by = list(yr, mo)]
  return(dat_month)
}
