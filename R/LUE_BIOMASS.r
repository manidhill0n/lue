#' @title Light Use efficiency model to estimate biomass
#' @docType data
#' @usage LUE_BIOMASS(fpar_raster,par,tmin,tmin_min,tmin_max,LUE_optimal)
#' @format A Biomass raster
#' @description Contains LUE_BIOMASS function to estimate aboveground biomass firstly by calculating the apar and then the minimum temperature and actual values of light use efficiency are further used to calculate the biomass of a crop.
#' @param fpar_raster fraction of photosynthetically active radiation (fpar) per day raster with .tif format
#' @param par clear sky surface photosynthetically active radiation (par) per day raster with .nc file format.
#' @param tmin Minimum temperature at 2 metres since previous post-processing per day raster with .nc file format.
#' @param tmin_min minimum value of tmin used for the threshold
#' @param tmin_max maximum value of tmin used for the threshold
#' @param LUE_optimal optical lue value with respect to crop type for example wheat crop LUE_optimal is 3.0 (Djumaniyazova et al., 2010)
#' @import fpar,par,tmin
#' @export
#' @references Djumaniyazova Y, Sommer R, Ibragimov N, Ruzimov J, Lamers J & Vlek P (2010) Simulating water use and N response of winter wheat in the irrigated floodplains of Northwest Uzbekistan. Field Crops Research 116, 239-251.
#' @references Shi Z, Ruecker G R,Mueller M, Conrad C, Ibragimov N, Lamers J P A, Martius C, Strunz G, Dech S & Vlek P L G (2007) Modeling of Cotton Yields in the Amu Darya River Floodplains of Uzbekistan Integrating Multitemporal Remote Sensing and Minimum Field Data. Agronomy Journal 99, 1317-1326
#' @keywords datasets
#' @return Biomass raster
#' @examples \dontrun{
#' ## load the data
#' fpar<-load("~/R/Maninder_eagles/lue/data/fpar.rda")
#' par1<-load("~/R/Maninder_eagles/lue/data/par1.rda")
#' tmin<-load("~/R/Maninder_eagles/lue/data/tmin.rda")
#' LUE_BIOMASS(fpar,par1,tmin,-2,12,3)
#' }
LUE_BIOMASS<-function(fpar_raster,par,tmin,tmin_min,tmin_max,LUE_optimal) {
  #Summing the PAR for a day
      par1<-sum(par1)
      # converting PAR from J*m^-2 to MJ*m^-2
      par1 <- par1/1000000 # convert PAR from J*m^-2 to MJ*m^-2
      #par1 <- projectRaster(pa1r, fpar_raster, method = "bilinear", verbose = TRUE)
      # calculating apar by multipying par with fpar
      apar <- par1 * fpar_raster
      # including tmin with a mean value in a day and making it in degree celsius
      tmin1<-mean(as.vector(tmin))-273.15
      # applying the criteria with diffrent thresholds of tmin for every crop

          if (tmin1 <= tmin_min){
            tmin1 <- 0
                                  }
          else if (tmin1 >= tmin_max){
            tmin1 <- 1
                                        }
          else {
          tmin1<- (tmin1 - tmin_min)* ((1/(tmin_max-tmin_min)))
                }
lue_act <- tmin1 * LUE_optimal
biomass<-apar*lue_act
return(biomass)
}

