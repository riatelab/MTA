#' @title General Deviation
#' @name gdev
#' @description This function computes the deviation between regional ratios 
#' and a ratio of reference. 
#' Each elementary unit's value will be compared to a global value.
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if NULL, the ratio of reference is the one of 
#' the whole study area (\code{sum(var1) / sum(var2)}).
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation (see Details).
#' @details 
#' The relative global deviation is the ratio between var1/var2 and ref
#' (\code{100 * (var1 / var2) / ref}). Values greater than 100 indicate that the 
#' unit ratio is greater than the ratio of reference. Values lower than 100 
#' indicate that the unit ratio is lower than the ratio of reference.\cr
#' The absolute global deviation is the amount of numerator that could be moved 
#' to obtain the ratio of reference on all units. 
#' @return A vector is returned.
#' @examples
#' # load data
#' data("GrandParisMetropole")
#' # compute absolute global deviation
#' com$gdevabs <- gdev(x = com, var1 = "INC", var2 = "TH", type = "abs")
#' # compute relative global deviation
#' com$gdevrel <- gdev(x = com, var1 = "INC", var2 = "TH", type = "rel")
#' 
#' # Deviations maps
#' if(require('cartography')){
#'   # set graphical parameters
#'   par(mar = c(0,0,1.2,0))
#'   # set breaks
#'   bks <- c(min(com$gdevrel),50,75,100,125,150,max(com$gdevrel))
#'   cols <- carto.pal(pal1 = "blue.pal", n1 = 3,
#'                     pal2 = "wine.pal", n2 = 3)
#'   # plot a choropleth map of the relative global deviation
#'   choroLayer(spdf = com.spdf, df = com, var = "gdevrel",
#'              legend.pos = "topleft",
#'              legend.title.txt = "Relative Deviation",
#'              breaks = bks, border = NA,
#'              col = cols)
#'   # add symbols proportional to the absolute general deviation
#'   com$sign <- ifelse(test = com$gdevabs<0, yes = "negative", no = "positive")
#'   propSymbolsTypoLayer(spdf = com.spdf, df = com, var = "gdevabs",var2 = "sign",
#'                        legend.var.pos = "left",legend.values.rnd = -2, 
#'                        legend.var2.values.order = c("positive", "negative"),
#'                        legend.var.title.txt = "Absolute Deviation",
#'                        col = c("#ff000050","#0000ff50"),legend.var2.pos = "n",
#'                        legend.var.style = "e", inches = 0.2)
#'   # add EPT boundaries
#'   plot(ept.spdf, add=TRUE)
#'   # add a layout
#'   layoutLayer(title = "General Deviation (reference: Grand Paris Metropole)",
#'               sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr", 
#'               north = TRUE,
#'               author = "MTA")
#' }
#' @export
gdev <- function(x, var1, var2, type = "rel", ref = NULL){
  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)
  
  # no ref value
  if (is.null(ref)){
    ref <- sum(x[,var1]) / sum(x[,var2])
  }
  # relative deviation
  if (type=="rel"){
    v <- ((x[,var1] / x[,var2]) / ref) * 100
  }
  # absolute deviation
  if (type=="abs"){
    v <- x[,var1] - (ref * x[,var2])
  }
  v <- v[match(vtot, vpar)]
  return(v)
}