#' @title Territorial Deviation
#' @name tdev
#' @description Compute the deviation of each territorial unit as regards  
#' to an intermediate territorial level of reference. 
#' @param x a dataframe or a sf object including  var1 and var2 and an 
#' aggregation key field (territorial belonging).  
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param key aggregation key field for measuring the deviation (intermediate 
#' territorial level).
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
#' library(mapsf)
#' # load data
#' mta_get_data()
#' 
#' # compute absolute territorial deviation
#' com$tdevabs <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT", 
#'                     type = "abs")
#' # compute relative territorial deviation
#' com$tdevrel <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT", 
#'                     type = "rel")
#' 
#' # relative deviation map
#' par(mar = c(0,0,1.2,0))
#' # set breaks
#' bks <- c(min(com$tdevrel), 80, 91, 100, 110, 125, max(com$tdevrel))
#' # plot a choropleth map of the relative territorial deviation
#'mf_map(x = com, var = "tdevrel", type = "choro", leg_pos = "topleft",
#'       leg_title = "Relative Deviation\n(100 = general average)",
#'       breaks = bks, border = NA,
#'       pal = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027"))
#'
#'# add EPT boundaries
#'mf_map(x = ept, col = NA, add = TRUE)
#'
#'# layout
#'mf_layout(title = "Territorial Deviation (reference: EPT of belonging)",
#'          credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'                           "\nMTA", packageVersion("MTA")),
#'          arrow = FALSE)
#'
#'
#'# absolute deviation map
#'com$sign <- ifelse(test = com$tdevabs < 0, yes = "Under-Income", no = "Over-Income")
#'mf_map(ept)
#'
#'mf_map(x = com, var = c("tdevabs", "sign"), type = "prop_typo", inches = 0.2,
#'       leg_title = c("Absolute Deviation\n(Income redistribution, euros)",
#'                     "Redistribution direction"), 
#'       leg_pos = c("topleft", "topright"), leg_val_rnd = -2,
#'       val_order = c("Under-Income", "Over-Income"),
#'       pal =  c("#ff0000","#0000ff"), add = TRUE)
#'
#'# layout
#'mf_layout(title = "Territorial Deviation (reference: EPT of belonging)",
#'          credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'                           "\nMTA", packageVersion("MTA")),
#'          arrow = FALSE)
#'
#' @export
tdev <- function(x, var1, var2, type = "rel", key){
  
  # convert to dataframe
  if (methods::is(x, "sf")){
    x <- st_set_geometry(x, NULL)
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
