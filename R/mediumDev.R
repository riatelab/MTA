#' @title Medium Deviation
#' @name mediumDev
#' @description This function computes the deviation between regional ratios 
#' and ratios of an aggregated level.
#' Each elementary unit value will be compared to the value of the upper level 
#' it belongs to.
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param key aggregation key field.
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned.
#' @examples
#' library(cartography)
#' x <- nuts3.df
#' x$gdppps2008 <- x$gdppps2008 * 1000000
#' x$key <- substr(x = x$id, start = 1, stop = 3)
#' x$mdevabs <- mediumDev(x = x, var1 = "gdppps2008", var2 = "pop2008",
#'                        type = "abs", key = "key")
#' x$mdevrel <- mediumDev(x = x, var1 = "gdppps2008",
#'                        var2 = "pop2008",
#'                        type = "rel", key = "key")
#' par(mar = c(0,0,1.2,0))
#' plot(nuts0.spdf[nuts0.spdf$id %in% c("BE", "NL", "LU", "DE", "FR"),], 
#'      border = NA)
#' choroLayer(spdf = nuts3.spdf, df = x, var = "mdevrel",
#'            legend.pos = "left", 
#'            breaks = c(min(x$mdevrel),50,75,100,125,150,max(x$mdevrel)),
#'            col = carto.pal(pal1 = "blue.pal", 3, pal2 = "wine.pal", 3),
#'            border = NA, legend.frame = TRUE, add=TRUE)
#' plot(nuts1.spdf, add=TRUE)
#' propSymbolsLayer(spdf = nuts3.spdf, df = x, var = "mdevabs", l
#'                  egend.pos = "topleft",
#'                  col = "#ff000050",col2 = "#0000ff50", inches = 0.1,
#'                  breakval = 0, legend.values.rnd = -5, legend.frame = TRUE)
#' layoutLayer(title = "Medium Deviation (NUTS1)")
#' @export
mediumDev <- function(x, var1, var2, key, type = "rel"){
  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)
  
  # aggregate values by key
  med <- aggregate(x[,c(var1, var2)], by = list(x[,key]), sum)
  med$ratio <- med[,var1] / med[,var2]
  # merge x and aggregate data
  mediumDev <- data.frame(x[,c(var1,var2)], 
                          med = med[match(x[,key], med$Group.1), 4])
  # relative deviation
  if (type=="rel"){
    v <- ((mediumDev[,var1] / mediumDev[,var2]) / mediumDev$med) * 100
  }
  # absolute deviation
  if (type=="abs"){
    v <- mediumDev[,var1] - (mediumDev[,var2] * mediumDev$med)
  }
  v <- v[match(vtot, vpar)]
  return(v)
}