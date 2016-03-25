#' Multiscalar typology
#'
#' This function compute a multiscalar typology according to the three relative 
#' positions about contexts (global, medium, local). The elementary units are classified
#'  in eight classes according to their three relative positions.
#'
#' @details xxx
#'
#' @param spdf Spatial polygon data frame
#' @param x h
#' @param spdfid g
#' @param xid dhd
#' @param var1 Numerator
#' @param var2 Denominator
#' @param ref distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
#' @param key Aggregation ky for the medium deviatiopn
#' @param order Threshold of typology
#' @param dist If true, the criterium is "gretter than". If false, the criterium is "lower than"
#' @param mat If true, the typology is ploted
#' @param threshold fh 
#' @param superior hdfsh
#' @export
synthesis3 <- function(spdf, x, spdfid = NULL, xid = NULL,
                       var1, var2,
                       ref = NULL,
                       key,
                       order = NULL, dist = NULL, mat = NULL,
                       threshold, superior = FALSE){
  
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
  type = "rel"
  
  x$ratio <- x[,var1] / x[,var2]
  x$gdevrel <- globalDev(x = x, var1 = var1, var2 = var2, ref = NULL, type = type)
  x$mdevrel <- mediumDev(x = x, var1 = var1, var2 = var2, key = key, type = type)
  x$ldevrel <- localDev(spdf = spdf, x = x, spdfid = spdfid, xid = xid,
                        var1 = var1, var2 = var2, order = order, dist = dist,
                        mat = mat, type = type)
  x$tmp1 <- 0
  x$tmp2 <- 0
  x$tmp3 <- 0
  if (superior == TRUE)
  {
    x[x$gdevrel >= threshold, "tmp1"] <- 1
    x[x$mdevrel >= threshold, "tmp2"] <- 2
    x[x$ldevrel >= threshold, "tmp3"] <- 4
  }
  if (superior == FALSE)
  {
    x[x$gdevrel <= threshold, "tmp1"] <- 1
    x[x$mdevrel <= threshold, "tmp2"] <- 2
    x[x$ldevrel <= threshold, "tmp3"] <- 4
  }
  
  x$synthesis3 <- as.factor(x$tmp1+x$tmp2+x$tmp3)
  colours <- c("#ffffff", "#fdc785","#ffffab","#fba9b0",
               "#addea6","#ffa100","#fff226","#e30020")
  
  value <- as.numeric(levels(x$synthesis3))
  colours <- data.frame (value,colours)
  x <- data.frame(x, "colours"=colours[match(x$synthesis3,colours$value),c("colours")])
  
  return(x)
}