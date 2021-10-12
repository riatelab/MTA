#' @title General Deviation
#' @name gdev
#' @description Compute the deviation of each territorial unit as regards  
#' to all the study area (or a reference value). 
#' @param x a data.frame or a sf object including var1 and var2.   
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if missing, the ratio of reference is the one of 
#' the whole study area (\code{sum(var1) / sum(var2)}).
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation (see Details).
#' @details 
#' The relative global deviation is the ratio between var1/var2 and ref
#' (\code{100 * (var1 / var2) / ref}). Values greater than 100 indicate that the 
#' unit ratio is greater than the ratio of reference. Values lower than 100 
#' indicate that the unit ratio is lower than the ratio of reference.\cr
#' The absolute global deviation is the amount of numerator that could be moved 
#' to obtain the ratio of reference on all units. (\code{(var1 - (ref * var2)}). 
#' @return A vector is returned.
#' @examples
#' # Load data
#' com <- sf::st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
#' ept <- sf::st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "ept", quiet = TRUE)
#' 
#' # compute absolute global deviation
#' com$gdevabs <- gdev(x = com, var1 = "INC", var2 = "TH", type = "abs")
#' # compute relative global deviation
#' com$gdevrel <- gdev(x = com, var1 = "INC", var2 = "TH", type = "rel")
#' 
#' # relative deviation map
#' library(mapsf)
#' par(mar = c(0,0,1.2,0))
#' # set breaks
#' bks <- c(min(com$gdevrel), 50, 75, 100, 125, 150, max(com$gdevrel))
#' # plot a choropleth map of the relative global deviation
#' mapsf::mf_map(x = com, var = "gdevrel", type = "choro", leg_pos = "topleft",
#'        leg_title = "Relative Deviation\n(100 = general average)",
#'        breaks = bks, border = NA,
#'        pal = c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027"))
#' 
#' # add EPT boundaries
#' mf_map(x = ept, col = NA, add = TRUE)
#' 
#' # layout
#' mf_layout(title = "General Deviation (reference: Grand Paris Metropole)",
#'           credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'                            "\nMTA", packageVersion("MTA")),
#'           arrow = FALSE)
#' 
#' # absolute deviation map
#' com$sign <- ifelse(test = com$gdevabs < 0, yes = "Under-Income", no = "Over-Income")
#' mf_map(ept)
#' 
#' mf_map(x = com, var = c("gdevabs", "sign"), type = "prop_typo", inches = 0.2,
#'        leg_title = c("Absolute Deviation\n(Income redistribution, euros)",
#'                      "Redistribution direction"), 
#'        leg_pos = c("topleft", "topright"), leg_val_rnd = -2,
#'        val_order = c("Under-Income", "Over-Income"),
#'        pal =  c("#ff0000","#0000ff"), add = TRUE)
#' 
#' # layout
#' mf_layout(title = "General Deviation (reference: Grand Paris Metropole)",
#'           credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'                            "\nMTA", packageVersion("MTA")),
#'           arrow = FALSE)
#' @export
gdev <- function(x, var1, var2, type = "rel", ref){
  
  # convert to dataframe
  if (methods::is(x, "sf")){
    x <- st_set_geometry(x, NULL)
  }

  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)

  # no ref value
  if (missing(ref)){
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


