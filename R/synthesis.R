#' @title Multiscalar typology
#' @name synthesis3
#' @description This function compute a multiscalar typology according to the three relative 
#' deviations (global, territorial, spatial). The elementary units are classified
#' in eight classes according to their three relative positions.
#' @param spdf Spatial polygon data frame.
#' @param x a dataframe.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame (optional).
#' @param xid identifier field in x, default to the first column 
#' of x (optional).
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x. 
#' @param ref distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
#' @param key Aggregation key for the territorial deviation. 
#' @param order contiguity order (dist and mat are not used).
#' @param dist distance defining the contiguity (order is not used).
#' @param mat a distance matrix (road distance, travel time...). Row and column 
#' names must fit xid identifiers.
#' @param threshold defined to build the typology (100 is considered as the average).
#' @param superior deviation values must be greater than threshold when criterion is true,
#' it must be lower than threshold when criterion is false.  
#' @return A dataframe including the ratio, the 3 relative deviations, their position according to the 
#' threshold and superior parameters, the resulting typology 8 classes and a proposal of colours for displaying territorial 
#' units on maps is returned.
#' @examples 
#' data("GrandParisMetropole")
#' synthesis <- synthesis3(spdf = com.spdf,
#'                         x = com,
#'                         spdfid = "DEPCOM",
#'                         xid = "DEPCOM",
#'                         var1 = "INC",
#'                         var2 = "TH",
#'                         dist = NULL,
#'                         key = "EPT",
#'                         order = 1,
#'                         mat = NULL,
#'                         threshold = 125,
#'                         superior = TRUE)
#' 
#'if(require('cartography')){
# typoLayer(spdf = com.spdf, df = synthesis3, var = "colours",
#           add = TRUE, 
#           border = "#D9D9D9",
#           lwd=0.25,
#           col = levels(synthesis3$colours),
#           legend.pos = "n")
# 
# plot(ept.spdf,add=TRUE)
# 
# colours <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
#              "#addea6","#ffa100","#fff226","#e30020")
# 
# rVal<-c(" .   .   . ","[X]  .   . ",
#         " .  [X]  . ","[X] [X]  . ",
#         " .   .  [X]","[X]  .  [X]",
#         " .  [X] [X]","[X] [X] [X]")
# 
# legendTypo(col = colours, categ = rVal,
#            title.txt = "Global, territorial and spatial deviations above 125 %",
#            nodata = FALSE)
#' }
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
  value <- c(0,1,2,3,4,5,6,7)
  
  colours <- data.frame (value,colours)
  x <- data.frame(x, "colours"=colours[match(x$synthesis3,colours$value),c("colours")])
  
  return(x)
}