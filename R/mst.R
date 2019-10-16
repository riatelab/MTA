#' @title Multiscalar Typology
#' @name mst
#' @description Compute a multiscalar typology according to the three relative 
#' deviations (general: G, territorial: T and spatial: S). The elementary units are classified
#' in eight classes according to their three relative positions.
#' @param x a sf object or a SpatialPolygonsDataFrame including var1 and var2.
#' @param xid identifier field in x (to be used for importing a personal distance matrix). Default to the first column. 
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if missing, the ratio of reference is the one of 
#' the whole study area (\code{sum(var1) / sum(var2)}).
#' @param key aggregation key field for measuring the deviation (intermediate territorial level).
#' @param order contiguity order.
#' @param dist distance threshold defining the contiguity. The cartesian 
#' distance between units centroids is used by default 
#' ; use mat to apply different metrics. 
#' @param mat a distance matrix (road distance, travel time...) between x units. 
#' Row and column names must fit xid identifiers. (optional)
#' @param threshold defined to build the typology (100 is considered as the average).
#' @param superior if TRUE, deviation values must be greater than threshold. If FALSE, 
#' deviation values must be lower than threshold.  
#' @return A dataframe including the ratio (var1/var2), the 3 relative 
#' deviations (G, T and S) and the resulting ordered typology (0 to 7). 
#' \itemize{Typology (which deviation is above/under the threshold):
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
#' # load data
#' data("GrandParisMetropole")
#' # Multiscalar typology - Wealthiest territories  
#' row.names(com) <- com$LIBCOM
#' synthesis <- mst(x = com, var1 = "INC", var2 = "TH", key = "EPT",
#'                  order = 1, threshold = 125, superior = TRUE)
#' # Territories avbove 125% for the 3 deviations 
#' synthesis[synthesis$mst == 7, ]
#' 
#' # Multiscalar typology - Poorest territories
#' synthesis <- mst (x = com, var1 = "INC", var2 = "TH", key = "EPT",
#'                   order = 1, threshold = 75, superior = FALSE)
#' # Territories below 75 % for the three deviations
#' synthesis[synthesis$mst == 7, ]
#' @export
mst <- function(x, var1, var2, ref, key, order, dist, mat, xid, threshold, 
                superior = FALSE){
  
  # convert to sf object
  if (unlist(class(x)[1]) == "SpatialPolygonsDataFrame"){
    x <- st_as_sf(x)
  }
  
  # Manage id
  if(missing(xid)){xid <- names(x)[1]}
  
  y <- as.data.frame(x)
  y <- y[, c(xid, var1, var2, key)]
  type = "rel"
  
  y$ratio <- y[,var1] / y[,var2]
  y$gdevrel <- gdev(x = y, var1 = var1, var2 = var2, ref = ref, type = type)
  y$tdevrel <- tdev(x = y, var1 = var1, var2 = var2, key = key, type = type)
  y$sdevrel <- sdev(x = x, xid = xid, var1 = var1, var2 = var2, type = type,
                    order = order, dist = dist, mat = mat)
  
  # Delete missing values
  y <- y[!is.na(y$gdevrel) & !is.na(y$tdevrel) & !is.na(y$sdevrel),]
  
  # Compute final typology
  y$tmp1 <- 0
  y$tmp2 <- 0
  y$tmp3 <- 0
  
  
  if (superior == TRUE)
  {
    y[y$gdevrel >= threshold, "tmp1"] <- 1
    y[y$tdevrel >= threshold, "tmp2"] <- 2
    y[y$sdevrel >= threshold, "tmp3"] <- 4
  }
  
  if (superior == FALSE)
  {
    y[y$gdevrel <= threshold, "tmp1"] <- 1
    y[y$tdevrel <= threshold, "tmp2"] <- 2
    y[y$sdevrel <= threshold, "tmp3"] <- 4
  }
  
  y$mst <- (y$tmp1+y$tmp2+y$tmp3)
  y <- y[,c(1,5:8,12)]
  return(y)
}
