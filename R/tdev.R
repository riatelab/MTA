#' @title Territorial Deviation
#' @name tdev
#' @description Compute the deviation of each territorial unit as regards  
#' to an intermediate territorial level of reference. 
#' @param x a dataframe, a sf object or a SpatialPolygonsDataFrame including var1
#' and var2, and an aggregation key field (territorial belonging).  
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param key aggregation key field for measuring the deviation (intermediate territorial level).
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation (see Details).
#' @details 
#' The relative territorial deviation is the ratio between var1/var2 and 
#' var1/var2 at the aggregated level. Values greater than 100 indicate that the 
#' unit ratio is greater than the ratio at the aggregated level. Values lower 
#' than 100 indicate that the unit ratio is lower than the ratio of the 
#' aggregated level.\cr
#' The absolute territorial deviation is the amount of numerator that could be 
#' moved to obtain the ratio of the aggregated level on all belonging units. 
#' @return A vector is returned.
#' @import sf
#' @examples
#' library(sf)
#' library(cartography)
#' # load data
#' data("GrandParisMetropole")
#' 
#' # compute absolute territorial deviation (EPT level)
#' com$tdevabs <- tdev(x = com, var1 = "INC", var2 = "TH", type = "abs", key = "EPT")
#' # compute relative territorial deviation (EPT level)
#' com$tdevrel <- tdev(x = com, var1 = "INC", var2 = "TH", type = "rel", key = "EPT")
#' 
#' # map deviations
#' # set graphical parameters
#' par(mar = c(0,0,1.2,0))
#' # set breaks
#' bks <- c(min(com$tdevrel),75,100,125,150,max(com$tdevrel))
#' # set color palette
#' cols <- carto.pal(pal1 = "blue.pal", n1 = 2,
#'                   pal2 = "wine.pal", n2 = 3)
#' # plot a choropleth map of the relative territorial deviation
#' choroLayer(x = com, var = "tdevrel",
#'            legend.pos = "topleft",
#'            breaks = bks, border = NA,
#'            legend.title.txt = "Relative Deviation\n(100 = territorial average)",
#'            col = cols)
#' # add symbols proportional to the absolute territorial deviation
#' com$sign <- ifelse(test = com$tdevabs<0, yes = "negative", no = "positive")
#' propSymbolsTypoLayer(com, var = "tdevabs",var2 = "sign",
#'                      legend.var.pos = "left",legend.values.rnd = -2,
#'                      legend.var2.values.order = c("positive", "negative"),
#'                      legend.var.title.txt = "Absolute Deviation\n(Income redistribution)",
#'                      col = c("#ff000050","#0000ff50"),legend.var2.pos = "n",
#'                      legend.var.style = "e", inches = 0.2)
#' # add EPT boundaries
#' plot(st_geometry(ept), add=TRUE)
#' # add a layout
#' layoutLayer(title = "Territorial Deviation (reference : EPT)",
#'             sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'             north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
#'             author = "MTA")
#' @export
tdev <- function(x, var1, var2, type = "rel", key){
  
  # convert to dataframe
  if (unlist(class(x)[1]) == "sf"){
    x <- st_set_geometry(x, NULL)
  }
  if (unlist(class(x)[1]) == "SpatialPolygonsDataFrame"){
    x <- x@data
  }
  
  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)

  # aggregate values by key
  med <- stats::aggregate(x[,c(var1, var2)], by = list(x[,key]), sum)
  med$ratio <- med[,var1] / med[,var2]
  # merge x and aggregate data
  tDev <- data.frame(x[,c(var1,var2)], 
                          med = med[match(x[,key], med$Group.1), 4])
  # relative deviation
  if (type=="rel"){
    v <- ((tDev[,var1] / tDev[,var2]) / tDev$med) * 100
  }
  # absolute deviation
  if (type=="abs"){
    v <- tDev[,var1] - (tDev[,var2] * tDev$med)
  }
  v <- v[match(vtot, vpar)]
  return(v)
}