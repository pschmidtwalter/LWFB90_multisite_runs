#' Aggregate SWAT.ASC
#'
#' Aggregate the daily layer-outputs of \code{runLWFB90} soil moisture state representations on profile-level.

#'
#' @param layeroutput data.frame or data.table
#' @param soil data.frame or data.table like soil argument of \code{\link{Run.B90}}
#'
#'
#' @return data.table with soil water state variable, aggregated to profile-level
#' @export
aggr_swati <- function(layer_output, soil){

  #create layer-column if not existing
  soil$nl <- 1:nrow(soil)

  setDT(soil)
  setDT(layer_output)

  soil[,c("thick","FK","PWP") := list(upper  - lower,
                                      MvG.swc(63, alpha / 100, npar, ths, thr) * (1 - gravel),
                                      MvG.swc(10^4.2, alpha / 100, npar, ths, thr) * (1 - gravel))]
                                      #MvG.swc(20000, alpha / 100, npar, ths, thr) * (1 - gravel))]

  soil[,c("nFK","FK.mm","PWP.mm", "nFK.mm" ) := list(FK - PWP,
                                                     FK * 100 * thick * 10,
                                                     PWP * 100 * thick * 10,
                                                     #PWPpsicr * 100 * thick * 10,
                                                     (FK-PWP) * 100 * thick * 10
  )]
  root_lower_nl <- max(which(soil$rootden>0))
  setkey(soil, nl)
  setkey(layer_output, nl)

  layer_output <- layer_output[soil,] # fast join soil hydraulic properties
  # key: nl, which are the SANDBELOW Layers ?

  # Schichtwerte   --------------------------------------------------------------------------------------------------------------
  # berechnen: relative Speicherfuellung, relative nutzbare Wasserverfuegbarkeit jeder Schicht
  layer_output[, SWATI := theta * (1 - gravel) * 100 * thick * 10]
  layer_output[, AWAT := swati - PWP.mm]
 # layer_output[, AWATpsicr := swati - PWPpsicr.mm]
  # layer_output <- layer_output[order(layer_output$yr,layer_output$doy,layer_output$nl),] dont sort, will loose thekey

  # 1.a)Tageswerte  Bodenwasserspeicher ########################

  SWAT.profil <- layer_output[,list(
    mo = mo[1], # Monat mitschleppen
    da = da[1],
    SWAT_prf = round(sum(swati),1),
    SWAT_we = round(sum(swati * (rootden > 0)),1),
    #SWAT_090 =  round(sum(swati * (upper <= 0 & lower >= -0.9)),1),
    SWAT_030 =  round(sum(swati * (upper <= 0 & lower >= -0.3)),1),
    AWAT_we =  round(sum(AWAT * (rootden > 0)),1),
    #AWAT_090 =  round(sum(AWAT * (upper <= 0 & lower >= -0.9)),1),
    AWAT_030 =  round(sum(AWAT * (upper <= 0 & lower >= -0.3)),1),
    RELAWAT_we =  round(sum(AWAT * (rootden > 0)) / sum(nFK.mm * (rootden > 0)),3),
    #RELAWAT_090 =  round(sum(AWAT * (upper <= 0 & lower >= -0.9)) / sum(nFK.mm * (upper <= 0 & lower >= -0.9)),3),
    RELAWAT_030 =  round(sum(AWAT * (upper <= 0 & lower >= -0.3)) / sum(nFK.mm * (upper <= 0 & lower >= -0.3)),3),
    #AWAT40 = round(sum(AWATpsicr * 0.6 * (rootden > 0)),1), #ich kriege nicht ganz dasselbe raus wie miscday.awat40!
    #AWAT40rw = round(sum(AWATpsicr * 0.6 * rootden/sum(rootden),1),
# ACHTUNG FEHLER: durch FK teilen, nicht durch nFK!!!##
    RELSWAT_we =  round(sum(SWATI * (rootden > 0)) / sum(nFK.mm * (rootden > 0)),3),
    #RELSWAT_090 =  round(sum(SWATI * (upper <= 0 & lower >= -0.9)) / sum(nFK.mm * (upper <= 0 & lower >= -0.9)),3),
    RELSWAT_030 =  round(sum(SWATI * (upper <= 0 & lower >= -0.3)) / sum(nFK.mm * (upper <= 0 & lower >= -0.3)),3),
####
    PSIlogmean_we = round(-1*10 ^ (weighted.mean(log10(psimi * -10),
                                                 (ths * (1 - gravel) * 100 * thick * 10) * (rootden > 0)))
                          ,1),
    #PSIlogmean_090 = round(-1 * 10 ^ (weighted.mean(log10(psimi * -10),
    #                                                 (ths * (1 - gravel) * 100 * thick * 10)*(upper <= 0 & lower >= -0.9)))
    #                        ,1),
    PSIlogmean_030 = round(-1 * 10 ^ (weighted.mean(log10(psimi * -10),
                                                   (ths * (1 - gravel) * 100 * thick * 10)*(upper <= 0 & lower >= -0.3)))
                          ,1),
    WaterTableDepth = -min(ifelse( -upper * (wetnes > 0.99) > 0,-upper * (wetnes > 0.99),Inf)),
    vrfl_we = vrfl[nl==root_lower_nl]
),
  by = list(yr, doy)]

  return(SWAT.profil)
}

MvG.swc <- function(psi,
                        alpha,
                        n,
                        ThS,
                        ThR,
                        m = 1-1/n){
  wetness <- 1/((1 + (alpha * psi)^n))^(m)
  theta <- wetness * (ThS-ThR) +ThR
  return(theta)
}
