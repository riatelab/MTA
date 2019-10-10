#' @title Multiscalar Absolute Synthesis
#' @name mas
#' @description This function sums the total amount of redistributions according to the three absolute 
#' deviations (global, territorial, spatial). 
#' @param x an sf object or a SpatialPolygonsDataFrame including var1 and var2.
#' @param xid identifier field in x (to be used for importing a personal distance 
#' matrix). Default to the first column. 
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if missing, the ratio of reference is the one of 
#' the whole study area (\code{sum(var1) / sum(var2)}).
#' @param key aggregation key field for measuring the deviation (intermediate territorial level).
#' @param order contiguity order.
#' @param dist distance threshold defining the contiguity. The cartesian 
#' distance between units centroids is used by default 
#' (see \code{\link{st_distance}}); use mat to apply different metrics. 
#' @param mat a distance matrix (road distance, travel time...) between x units. 
#' Row and column names must fit xid identifiers. (optional)
#' @return A dataframe including the mass of numerator to redistribue to reach a perfect equilibrium 
#' according to the 3 contexts, expressed in numerator measure unit and as a share of the numerator mass.
#' @export
#' @examples
#' data("GrandParisMetropole")
#' redistr <- mas(x = com, var1 = "INC", var2 = "TH", dist = 5000, key = "EPT")
#' redistr

mas <- function(x, xid, var1, var2, ref, key, order, dist, mat){
  # convert to sf object
  if (methods::is(x, "Spatial")){
    x <- st_as_sf(x)
  }
  
  # Manage id
  if(missing(xid)){xid <- names(x)[1]}
  
  # Convert to dataframe for calculations
  y <- st_set_geometry(x, NULL)
  
  y <- y[, c(xid, var1, var2, key)]
  type = "abs"
  
  y$ratio <- y[,var1] / y[,var2]
  y$gdevabs <- gdev(x = y, var1 = var1, var2 = var2, ref = ref, type = type)
  y$tdevabs <- tdev(x = y, var1 = var1, var2 = var2, key = key, type = type)
  y$sdevabs <- sdev(x = x, xid = xid, var1 = var1, var2 = var2, type = type,
                    order = order, dist = dist, mat = mat)
  
  # Delete missing values
  y <- y[!is.na(y$gdevabs) & !is.na(y$tdevabs) & !is.na(y$sdevabs),]
  
  # Select all the redistribution above 0 on the three deviations and numerator, and sum it
  gdevcon <- y[y$gdevabs>0,"gdevabs"]
  gdevcon <- sum(gdevcon)
 
  tdevcon <- y[y$tdevabs>0,"tdevabs"]
  tdevcon <- sum(tdevcon)
  
  sdevcon <- y[y$sdevabs>0,"sdevabs"]
  sdevcon <- sum(sdevcon)
  
  numtot <- sum(y[,var1])
  
  tmp<-data.frame(matrix(1,4,2))
  rownames(tmp) <- c("General deviation","Territorial deviation","Spatial deviation","Numerator mass")
  colnames(tmp) <- c(var1, paste0("share.",var1))
  tmp[1,1] <- gdevcon
  tmp[2,1] <- tdevcon
  tmp[3,1] <- sdevcon
  tmp[4,1] <- numtot
  
  tmp[1,2] <- gdevcon / numtot * 100
  tmp[2,2] <- tdevcon / numtot * 100
  tmp[3,2] <- sdevcon / numtot * 100
  tmp[4,2] <- numtot / numtot * 100
  
  return(tmp)
}
