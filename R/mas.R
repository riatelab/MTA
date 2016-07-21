#' @title Multiscalar Absolute Synthesis
#' @name mas
#' @description This function sums the total amount of redistributions according to the three absolute 
#' deviations (global, territorial, spatial). 
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if NULL, the ratio of reference is the one of 
#' the whole study area (\code{sum(var1) / sum(var2)}).
#' @param key aggregation key field.
#' @param order contiguity order.
#' @param dist distance threshold defining the contiguity. The cartesian 
#' distance between units centroids is used by default 
#' (see \code{\link{gDistance}}); use mat to apply different metrics. 
#' @param mat a distance matrix (road distance, travel time...) between x units. 
#' Row and column names must fit xid identifiers. (optional)
#' @param spdf a SpatialPolygonsDataFrame that matches x data frame.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param xid identifier field in x, default to the first column 
#' of x. (optional)
#' @return A dataframe including the mass of numerator to redistribue to reach a perfect equilibrium 
#' according to the 3 contexts, expressed in numerator measure unit and as a share of the numerator mass.
#' @export
#' @examples
#' data("GrandParisMetropole")
#' redistr <- mas(spdf = com.spdf,
#'                        x = com,
#'                        spdfid = "DEPCOM",
#'                        xid = "DEPCOM",
#'                        var1 = "INC",
#'                        var2 = "TH",
#'                        order = 2,
#'                        key = "EPT",
#'                        dist = NULL,
#'                        mat = NULL)
#' redistr
mas <- function(x, var1, var2, ref = NULL, key, 
                          spdf, order = NULL, 
                          dist = NULL,
                          mat = NULL, spdfid = NULL, 
                          xid = NULL){
  
  
  
  # # check order & dist
  # if(is.null(order) == FALSE & is.null(dist) == FALSE){
  #   stop("
  #        Define local deviation either by order or by dist, not by both.",
  #        call. = FALSE)
  # }
  
  # check ids
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(xid)){xid <- names(x)[1]}
  
  x <- x[, c(xid, var1, var2, key)]
  type = "abs"
  
  x$ratio <- x[,var1] / x[,var2]
  x$gdevabs <- gdev(x = x, var1 = var1, var2 = var2, ref = ref, type = type)
  x$tdevabs <- tdev(x = x, var1 = var1, var2 = var2, key = key, type = type)
  x$sdevabs <- sdev(spdf = spdf, x = x, spdfid = spdfid, xid = xid,
                    var1 = var1, var2 = var2, order = order, dist = dist,
                    mat = mat, type = type)
  
  # Select all the redistribution above 0 on the three deviations and numerator, and sum it
  gdevcon <- x[x$gdevabs>0,"gdevabs"]
  gdevcon <- sum(gdevcon)
  
  tdevcon <- x[x$tdevabs>0,"tdevabs"]
  tdevcon <- sum(tdevcon)
  
  sdevcon <- x[x$sdevabs>0,"sdevabs"]
  sdevcon <- sum(sdevcon)
  
  numtot <- sum(x[,var1])
  
  tmp<-data.frame(matrix(1,4,2))
  rownames(tmp) <- c("gdev","tdev","sdev","num")
  colnames(tmp) <- c(var1, paste0("share.",var1))
  tmp[1,1] <- gdevcon
  tmp[2,1] <- tdevcon
  tmp[3,1] <- sdevcon
  tmp[4,1] <- numtot
  
  tmp[1,2] <- gdevcon / numtot * 100
  tmp[2,2] <- tdevcon / numtot * 100
  tmp[3,2] <- sdevcon / numtot * 100
  tmp[4,2] <- numtot / numtot * 100
  
  tmp
  
  return(tmp)
}