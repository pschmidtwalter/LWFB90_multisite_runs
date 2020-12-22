#' Aggregate daily values of evapday.asc to Vegetationperiod-Sums and Stress-Indices
#'
#' @param dat data.frame or data.table of evapday.asc
#' @param vp.start start-doys (day of year) for the beginning of aggregation,
#' corresponding to vp.years
#' @param vp.end end-doys for the end of aggregation,
#' corresponding to vp.years
#'
#' @return a data.table with yearly aggregated values and stress-indicators
#' integrating over vegetation or other intervals
#' @export
fluxes_dailytovegper <- function(dat, vp.start, vp.end) {

  setDT(dat)
  vp <- data.table(yr = unique(dat$yr), start = vp.start, end = vp.end)
  setkey(dat, yr)
  setkey(vp, yr)

  dat <- dat[vp]

  dat_vp <- dat[which(doy >= start & doy <= end),list(
    vpstartdoy = start[1],
    vpenddoy = end[1],
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
    TRATIO_avg = round(mean(ifelse(ptran > 0, tran / ptran, 1)),3),
    TRATIO_min = round(mean(ifelse(ptran > 0, tran / ptran, 1)),3),
    Days_TRATIO_lower50 = sum((ifelse(ptran > 0, tran / ptran, 1)) < 0.5),
    Durations_TRATIO_lower50 = paste((rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.5)
                                      $lengths[rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.5)$values]), collapse = " "),
    Defsum_TRATIO_lower50 = round(sum((1 - (ifelse(ptran > 0, tran / ptran, 1) / 0.5)) * (ifelse(ptran > 0, tran / ptran, 1) < 0.5)),3),

    Days_TRATIO_lower80 = sum((ifelse(ptran > 0, tran / ptran, 1)) < 0.8),
    Durations_TRATIO_lower80 = paste((rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.8)
                                      $lengths[rle((ifelse(ptran > 0, tran / ptran, 1)) < 0.8)$values]), collapse = " "),
    Defsum_TRATIO_lower80 = round(sum((1 - (ifelse(ptran > 0, tran / ptran, 1) / 0.8)) * (ifelse(ptran > 0, tran / ptran, 1) < 0.8)),3)
  ),
  by = list(yr)]
  return(dat_vp)

}
