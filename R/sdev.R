#' @title Spatial Deviation
#' @name sdev
#' @description Compute the deviation of each territorial unit as regards  
#' to its geographical neighbourhood. Neighbourhood is defined either
#' by contiguity order, by a distance value or by a personal matrix (travel time...) 
#' @param x a sf object or a SpatialPolygonsDataFrame including var1 and var2. 
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param order contiguity order.
#' @param dist distance threshold defining the contiguity. The cartesian 
#' distance between units centroids is used by default;
#'  use mat to apply different metrics.
#' @param xid identifier field in x (to be used for importing a personal distance matrix). 
#' Default to the first column of x. (optional) 
#' @param mat a distance matrix (road distance, travel time...) between x units. 
#' Row and column names must fit xid identifiers. (optional)
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation (see Details).
#' @details 
#' The relative spatial deviation is the ratio between var1/var2 and 
#' var1/var2 in the specified neighbourhood. Values greater than 100 indicate 
#' that the unit ratio is greater than the ratio in its neighbourhood. Values 
#' lower than 100 indicate that the unit ratio is lower than the ratio in its 
#' neighbourhood.\cr
#' The absolute spatial deviation is the amount of numerator that could be 
#' moved to obtain the same ratio in all units of its neighbourhood. 
#' @return A vector is returned.
#' @import sf
#' @examples
#' library(sf)
#' library(cartography)
#' # load data
#' data("GrandParisMetropole")
#' 
#' # compute absolute spatial deviation in a neighbourhood defined by a contiguity
#' # order of 2.
#' com$sdevabs <- sdev(x = com, var1 = "INC", var2 = "TH", order =2, type = "abs")
#' 
#' #compute relative spatial deviation in a neighbourhood defined within a distance
#' # of 5km between communes' centroids
#' com$sdevrel <- sdev(x = com, var1 = "INC", var2 = "TH", type = "rel", dist = 5000)
#' 
#' # compute absolute spatial deviation in a neighbourhood defined within a road
#' # travel time of 10 minutes by car
#' com$scardevabs <- sdev(x = com, var1 = "INC", var2 = "TH", type = "abs", dist = 10, mat = cardist)
#' 
#' # compute relative spatial deviation in a neighbourhood defined within a road
#' # travel time of 10 minutes by car
#' com$scardevrel <- sdev(x = com, var1 = "INC", var2 = "TH", type = "rel", dist = 10, mat = cardist)
#' 
#' # map deviations
#' # set graphical parameters
#' par(mar = c(0,0,1.2,0))
#' # set breaks
#' bks <- c(min(com$scardevrel),75,100,125,150,max(com$scardevrel))
#' # set colot palette
#' cols <- carto.pal(pal1 = "blue.pal", n1 = 2,
#'                   pal2 = "wine.pal", n2 = 3)
#' # plot a choropleth map of the relative spatial deviation
#' choroLayer(x = com, var = "scardevrel",
#'            legend.pos = "topleft",
#'            legend.title.txt = "Relative Deviation\n(100 = spatial average)",
#'            breaks = bks, border = NA,
#'            col = cols)
#' # add symbols proportional to the absolute spatial deviation
#' com$sign <- ifelse(test = com$scardevabs<0, yes = "negative", no = "positive")
#' propSymbolsTypoLayer(x = com, var = "scardevabs",var2 = "sign",
#'                      legend.var.pos = "left",legend.values.rnd = -2,
#'                      legend.var2.values.order = c("positive", "negative"),
#'                      legend.var.title.txt = "Absolute Deviation\n(Income redistribution)",
#'                      col = c("#ff000050","#0000ff50"),legend.var2.pos = "n",
#'                      legend.var.style = "e", inches = 0.2)
#' # add EPT boundaries
#' plot(st_geometry(ept), add=TRUE)
#' # add a layout
#' layoutLayer(title = "Spatial Deviation (neighbourhoud : 10 minutes by car)",
#'             sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'             north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
#'             author = "MTA")
#' @import sp
#' @export
sdev <- function(x, var1, var2, type = "rel", xid,  
                 order, dist, mat){
  
  # convert to sf object
  if (unlist(class(x)[1]) == "SpatialPolygonsDataFrame"){
    x <- st_as_sf(x)
  }
  
  # Manage id
  if(missing(xid)){xid <- names(x)[1]}
  
  # Convert to dataframe for calculations
  xx <- st_set_geometry(x, NULL)
  
  # Manage dist and order parameters
  if(missing(order) == FALSE & missing(dist) == FALSE){
    stop("
         Define local deviation either by order or by dist, not by both.",
         call. = FALSE)
  }
  if(missing(order) == TRUE & missing(dist) == TRUE){
    stop("
         Define local deviation either by order or by dist.",
         call. = FALSE)
  }
  
  # test for NAs
  vtot <- row.names(x)
  test <- testNAdf(x = xx, var1 = var1, var2 = var2)
  vpar <- row.names(test)
  
  # contig dev
  if (!missing(order)){
    mat <- contiguityMat(x = x, xid = xid)
    mat[mat <= order] <- 1
    mat[mat > order] <- 0
    v <- locmat(mat = mat, x = xx, var1 = var1, var2 = var2, type = type)
  }
  
  # dist dev
  if (!missing(dist)){
    if (missing(mat)){
      mat <- distanceMat(x = x, xid = xid)
    }else{
      rnames <- factor(row.names(x[[xid]]), levels = x[[xid]])
      mat <- mat[levels(rnames), levels(rnames)]
    }
    mat[mat <= dist] <- 1
    mat[mat > dist] <- 0
    v <- locmat(mat = mat, x = xx, var1 = var1, var2 = var2, type = type)
  }
  v <- v[match(vtot, vpar)]
  return(v)
}
