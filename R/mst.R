#' @title Multiscalar Typology
#' @name mst
#' @description This function compute a multiscalar typology according to the three relative 
#' deviations (general: G, territorial: T and spatial: S). The elementary units are classified
#' in eight classes according to their three relative positions.
#' @param spdf a SpatialPolygonsDataFrame that matches x data frame.
#' @param x a dataframe.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param xid identifier field in x, default to the first column 
#' of x. (optional)
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
#' @param threshold defined to build the typology (100 is considered as the average).
#' @param superior if TRUE, deviation values must be greater than threshold. If FALSE, 
#' deviation values must be lower than threshold.  
#' @return A dataframe including the initial dataset, the ratio, the 3 relative 
#' deviations (G, T and S) and the resulting typology. 
#' \itemize{Typology (which deviation is over/under the threshold):
#' \item{0: none}
#' \item{1: G }
#' \item{2: T}
#' \item{3: G and T}
#' \item{4: S}
#' \item{5: G and S}
#' \item{6: T and S}
#' \item{7: G, T and S}
#' }
#' @examples
#' data("GrandParisMetropole")
#' synthesis <- mst(spdf = com.spdf,
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
#' if(require('cartography')){
#'   par(mar = c(0,0,1.2,0))
#'   typoLayer(spdf = com.spdf, df = synthesis, var = "mst",
#'             border = "#D9D9D9",legend.values.order = 0:7, 
#'             col = c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
#'                     "#addea6","#ffa100","#fff226","#e30020"),
#'             lwd = 0.25,
#'             legend.pos = "n")
#'   
#'   plot(ept.spdf,add=TRUE)
#'   
#'   colours <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
#'                "#addea6","#ffa100","#fff226","#e30020")
#'   
#'   rVal<-c(" .     .   . ",
#'           "[X]   .   . ",
#'           " .   [X]  . ",
#'           "[X] [X]  . ",
#'           " .    .   [X]",
#'           "[X]  .   [X]",
#'           " .   [X] [X]",
#'           "[X] [X] [X]")
#'   
#'   legendTypo(col = colours, categ = rVal,
#'              title.txt = "General, territorial and spatial\ndeviations above 125 %
#'              \n       G T S",
#'              nodata = FALSE)
#'   
#'   layoutLayer(title = "Multiscalar Typology",
#'               sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'               author = "MTA")
#' }
#' @export
mst <- function(x, var1, var2, ref = NULL, key, 
                       spdf, order = NULL, 
                       dist = NULL,
                       mat = NULL, spdfid = NULL, 
                       xid = NULL, threshold, 
                       superior = FALSE){
  
  # check order & dist
  # if(is.null(order) == FALSE & is.null(dist) == FALSE){
  #   stop("
  #        Define local deviation either by order or by dist, not by both.",
  #        call. = FALSE)
  # }
  
  # check ids
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(xid)){xid <- names(x)[1]}
  
  x <- x[, c(xid, var1, var2, key)]
  type = "rel"
  
  x$ratio <- x[,var1] / x[,var2]
  x$gdevrel <- gdev(x = x, var1 = var1, var2 = var2, ref = ref, type = type)
  x$tdevrel <- tdev(x = x, var1 = var1, var2 = var2, key = key, type = type)
  x$sdevrel <- sdev(spdf = spdf, x = x, spdfid = spdfid, xid = xid,
                        var1 = var1, var2 = var2, order = order, dist = dist,
                        mat = mat, type = type)
  x$tmp1 <- 0
  x$tmp2 <- 0
  x$tmp3 <- 0
  if (superior == TRUE)
  {
    x[x$gdevrel >= threshold, "tmp1"] <- 1
    x[x$tdevrel >= threshold, "tmp2"] <- 2
    x[x$sdevrel >= threshold, "tmp3"] <- 4
  }
  
  if (superior == FALSE)
  {
    x[x$gdevrel <= threshold, "tmp1"] <- 1
    x[x$tdevrel <= threshold, "tmp2"] <- 2
    x[x$sdevrel <= threshold, "tmp3"] <- 4
  }
  
  x$mst <- (x$tmp1+x$tmp2+x$tmp3)
  # colours <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
  #              "#addea6","#ffa100","#fff226","#e30020")
  # value <- c(0,1,2,3,4,5,6,7)
  # 
  # colours <- data.frame (value,colours)
  # x <- data.frame(x[,c(1:8,12)], "colours"=colours[match(x$mst,colours$value),c("colours")])
  x <- x[,c(1:8,12)]
  return(x)
}



