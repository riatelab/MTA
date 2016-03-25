#' @title Global Deviation
#' @name globalDev
#' @description This function computes the deviation between regional ratios 
#' and a ratio of reference. 
#' Each elementary unit value will be compared to a global value.
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if NULL, the ratio of reference is the one of 
#' the whole study area (data.frame).
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned.
#' @examples 
#' library(cartography)
#' x <- nuts3.df
#' x$gdppps2008 <- x$gdppps2008 * 1000000
#' x$gdevabs <- globalDev(x = x, 
#'                        var1 = "gdppps2008", 
#'                        var2 = "pop2008", 
#'                        type = "abs")
#' x$gdevrel <- globalDev(x = x, 
#'                        var1 = "gdppps2008", 
#'                        var2 = "pop2008", 
#'                        type = "rel")
#' par(mar = c(0,0,1.2,0))
#' choroLayer(spdf = nuts3.spdf, df = x, var = "gdevrel", 
#'            legend.pos = "topright", 
#'            breaks = c(11,50,75,100,125,150,7000), border = NA,
#'            col = carto.pal(pal1 = "blue.pal", n1 = 3, 
#'                            pal2 = "wine.pal", n2 = 3))
#' propSymbolsLayer(spdf = nuts3.spdf, df = x, var = "gdevabs", 
#'                  legend.pos = "right",legend.values.rnd = -5,
#'                  col = "#ff000050",col2 = "#0000ff50", 
#'                  legend.style = "c", inches = 0.1,
#'                  breakval = 0)
#' plot(rgeos::gUnaryUnion(nuts0.spdf), add = TRUE)
#' layoutLayer(title = "Global Deviation")
#' @export
globalDev <- function(x, var1, var2, ref = NULL, type = "rel"){
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