#' @title Spatial Deviation
#' @name sdev
#' @description This function computes the deviation between regional ratios and 
#' local ratios. Local ratios are defined either by a contiguity order or by a 
#' distance measure between regions.
#' Each elementary unit value will be compared to the value of its 
#' neighborhood.
#' @param spdf a SpatialPolygonsDataFrame that matches x data frame.
#' @param x a data frame.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param xid identifier field in x, default to the first column 
#' of x. (optional)
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param order contiguity order.
#' @param dist distance threshold defining the contiguity. The cartesian 
#' distance between units centroids is used by default 
#' (see \code{\link{gDistance}}); use mat to apply different metrics. 
#' @param mat a distance matrix (road distance, travel time...) between x units. 
#' Row and column names must fit xid identifiers. (optional)
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation (see Details).
#' @details 
#' The relative spatial deviation is the ratio between var1/var2 and 
#' var1/var2 in the specified neighborhoud. Values greater than 100 indicate 
#' that the unit ratio is greater than the ratio in its neighborhoud. Values 
#' lower than 100 indicate that the unit ratio is lower than the ratio in its 
#' neighborhoud.\cr
#' The absolute spatial deviation is the amount of numerator that could be 
#' moved to obtain the same ratio in all units of its neighborhoud. 
#' @return A vector is returned.
#' @import sp
#' @examples
#' # load data
#' data("GrandParisMetropole")
#' # compute absolute spatial deviation in a neighborhood defined by a contiguity 
#' # order of 2.
#' com$sdevabs <- sdev(x = com, var1 = "INC", var2 = "TH", 
#'                           type = "abs", spdf = com.spdf, order = 2)
#' 
#' # compute relative spatial deviation in a neighborhood defined within a distance 
#' # of 5km between communes' centroids 
#' com$sdevrel <- sdev(x = com, var1 = "INC", var2 = "TH", type = "rel", 
#'                           spdf = com.spdf, dist = 5000)
#' 
#' # compute absolute spatial deviation in a neighborhood defined within a road 
#' # travel time of 10 minutes by car
#' com$scardevabs <- sdev(x = com, var1 = "INC", var2 = "TH", type = "abs", 
#'                              spdf = com.spdf, dist = 10, mat = cardist)
#' # compute relative spatial deviation in a neighborhood defined within a road 
#' # travel time of 10 minutes by car
#' com$scardevrel <- sdev(x = com, var1 = "INC", var2 = "TH", type = "rel", 
#'                              spdf = com.spdf, dist = 10, mat = cardist)
#' 
#' # map deviations
#' if(require('cartography')){
#'   # set graphical parameters
#'   par(mar = c(0,0,1.2,0))
#'   # set breaks
#'   bks <- c(min(com$scardevrel),50,75,100,125,150,max(com$scardevrel))
#'   bks <- sort(bks)
#'   # set colot palette
#'   cols <- carto.pal(pal1 = "blue.pal", n1 = 3,
#'                     pal2 = "wine.pal", n2 = 3)
#'   # plot a choropleth map of the relative spatial deviation
#'   choroLayer(spdf = com.spdf, df = com, var = "scardevrel",
#'              legend.pos = "topleft",
#'              legend.title.txt = "Relative Deviation",
#'              breaks = bks, border = NA,
#'              col = cols)
#'   # add symbols proportional to the absolute spatial deviation
#'   propSymbolsLayer(spdf = com.spdf, df = com, var = "scardevabs",
#'                    legend.pos = "left",legend.values.rnd = -5,
#'                    legend.title.txt = "Absolute Deviation",
#'                    col = "#ff000050",col2 = "#0000ff50",
#'                    legend.style = "e", inches = 0.2,
#'                    breakval = 0)
#'   # add a layout
#'   layoutLayer(title = "Spatial Deviation (neighborhoud: 10 minutes by car)",
#'               sources = "GEOFLA® 2015 v2.1, impots.gouv.fr", north = TRUE,
#'               author = "MTA")
#' }
#' @export
sdev <- function(x, var1, var2, type = "rel", spdf,
                     order = NULL, dist = NULL, mat = NULL, 
                     spdfid = NULL, xid = NULL){
  
  if(is.null(order) == FALSE & is.null(dist) == FALSE){
    stop("
         Define local deviation either by order or by dist, not by both.",
         call. = FALSE)
  }
  if(is.null(order) == TRUE & is.null(dist) == TRUE){
    stop("
         Define local deviation either by order or by dist.",
         call. = FALSE)
  }
  
  # check ids
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(xid)){xid <- names(x)[1]}
  
  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)
  
  # Selecect spdf units
  spdf <- spdf[stats::na.omit(match(x = x[,xid], table = spdf@data[,spdfid])),]
  # row.names(spdf) <- spdf@data[,xid]
  # contig dev
  if (!is.null(order)){
    mat <- contiguityMat(spdf = spdf)
    mat[mat <= order] <- 1
    mat[mat > order] <- 0
    v <- locmat(mat = mat, x = x, var1 = var1, var2 = var2, type = type)
  }
  
  # dist dev
  if (!is.null(dist)){
    if (is.null(mat)){
      mat <- distanceMat(spdf = spdf)
    }else{
      rnames <- factor(row.names(spdf), levels = row.names(spdf))
      mat <- mat[levels(rnames), levels(rnames)]
    }
    mat[mat <= dist] <- 1
    mat[mat > dist] <- 0
    v <- locmat(mat = mat, x = x, var1 = var1, var2 = var2, type = type)
  }
  v <- v[match(vtot, vpar)]
  return(v)
}