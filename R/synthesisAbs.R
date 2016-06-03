#' @title Multiscalar absolute synthesis
#' @name synthesisAbs
#' @details This function sums the total amount of redistributions according to the three absolute 
#' deviations (global, territorial, spatial). 
#' @param spdf Spatial polygon data frame.
#' @param x a dataframe.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame (optional).
#' @param xid identifier field in x, default to the first column 
#' of x (optional).
#' @param var1 name of the numerator variable in x.
#' @param var1 name of the denominator variable in x. 
#' @param ref distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
#' @param key Aggregation key for the territorial deviation. 
#' @param order contiguity order (dist and mat are not used).
#' @param dist distance defining the contiguity (order is not used).
#' @param mat a distance matrix (road distance, travel time...). Row and column 
#' names must fit xid identifiers.
#' @export A dataframe including the mass of numerator to redistribue to reach a perfect equilibrium 
#' according to the 3 contexts, expressed in numerator measure unit and as a share of the numerator mass.
#' @examples 
#' data("GrandParisMetropole")
# synthesisAbs(spdf = com.spdf,
#              x = com,
#              spdfid = "DEPCOM",
#              xid = "DEPCOM",
#              var1 = "INC",
#              var2 = "TH",
#              order = 1,
#              key = "EPT",
#              dist = NULL, 
#              mat = NULL)

synthesisAbs <- function(spdf, x, spdfid = NULL, xid = NULL,
                       var1, var2,
                       ref = NULL,
                       key,
                       order = NULL, dist = NULL, mat = NULL){
  
  # check order & dist
  if(is.null(order) == FALSE & is.null(dist) == FALSE){
    stop("
         Define local deviation either by order or by dist, not by both.",
         call. = FALSE)
  }
  
  # check ids
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(xid)){xid <- names(x)[1]}
  
  x <- x[, c(xid, var1, var2, key)]
  type = "abs"
  
  x$ratio <- x[,var1] / x[,var2]
  x$gdevabs <- globalDev(x = x, var1 = var1, var2 = var2, ref = NULL, type = type)
  x$mdevabs <- mediumDev(x = x, var1 = var1, var2 = var2, key = key, type = type)
  x$ldevabs <- localDev(spdf = spdf, x = x, spdfid = spdfid, xid = xid,
                        var1 = var1, var2 = var2, order = order, dist = dist,
                        mat = mat, type = type)

  # Select all the redistribution above 0 on the three deviations and numerator, and sum it
  gdevcon <- subset(x,  gdevabs > 0, select = c(gdevabs))
  gdevcon <- sum(gdevcon)
  
  mdevcon <- subset(x,  mdevabs > 0, select = c(mdevabs))
  mdevcon <- sum(mdevcon)
  
  ldevcon <- subset(x,  ldevabs > 0, select = c(ldevabs))
  ldevcon <- sum(ldevcon)
  
  numtot <- sum(x[,var1])
  
  tmp<-data.frame(matrix(1,4,2))
  rownames(tmp) <- c("gdev","mdev","ldev","num")
  colnames(tmp) <- c("abs", "abs/num")
  tmp[1,1] <- gdevcon
  tmp[2,1] <- mdevcon
  tmp[3,1] <- ldevcon
  tmp[4,1] <- numtot
  
  tmp[1,2] <- gdevcon / numtot * 100
  tmp[2,2] <- mdevcon / numtot * 100
  tmp[3,2] <- ldevcon / numtot * 100
  tmp[4,2] <- numtot / numtot * 100
  
  tmp
  
  return(tmp)
}