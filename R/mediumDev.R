#' @title Territorial Deviation
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
#' data("GrandParisMetropole")
#' com$mdevabs <- mediumDev(x = com, 
#'                          var1 = "INC", 
#'                          var2 = "TH",
#'                          key = "EPT", 
#'                          type = "abs")
#' com$mdevrel <- mediumDev(x = com, 
#'                          var1 = "INC", 
#'                          var2 = "TH",
#'                          key = "EPT", 
#'                          type = "rel")
#' 
#' if(require('cartography')){
#'   par(mar = c(0,0,1.2,0))
#'   bks <- c(min(com$mdevrel),75,100,125,150,max(com$mdevrel))
#'   choroLayer(spdf = com.spdf, df = com, var = "mdevrel",
#'              legend.pos = "topright",
#'              breaks = bks, border = NA,
#'              col = carto.pal(pal1 = "blue.pal", n1 = 3,
#'                              pal2 = "wine.pal", n2 = 3))
#'   propSymbolsLayer(spdf = com.spdf, df = com, var = "mdevabs",
#'                    legend.pos = "right",legend.values.rnd = -5,
#'                    col = "#ff000050",col2 = "#0000ff50",
#'                    legend.style = "c", inches = 0.1,
#'                    breakval = 0)
#'   plot(ept.spdf, add=TRUE)
#'   layoutLayer(title = "Medium Deviation")
#' }
#' @export
mediumDev <- function(x, var1, var2, type = "rel", key){
  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)
  
  # aggregate values by key
  med <- stats::aggregate(x[,c(var1, var2)], by = list(x[,key]), sum)
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