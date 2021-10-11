#' @title Map Multiscalar Typology (3 deviations)
#' @name map_mst
#' @description Compute the multiscalar typology (3 deviations) and propose 
#' labels and colours for mapping the results.  
#' @param x a sf object or a dataframe including 3 pre-calculated deviations. 
#' @param gdevrel name of the general relative deviation variable in x.
#' @param tdevrel name of the territorial relative deviation variable in x.  
#' @param sdevrel name of the the spatial relative deviation variable in x.
#' @param threshold defined to build the typology (100 is considered as the 
#' average)
#' @param superior if TRUE, deviation values must be greater than threshold. If 
#' FALSE, deviation values must be lower than threshold.
#' @param xid identifier field in x. Default the first column. 
#' @return A list including a ordered sf object for mapping mst column (geom), a 
#' vector of suggested colors (cols) and a vector of adatped labels for the 
#' legend (leg_labels). 
#' \itemize{Typology labels (which deviation is above/under the threshold):
#' \item{0: none (mst value = 0)}
#' \item{G: general only (mst value = 1) }
#' \item{T: territorial only (mst value = 2)}
#' \item{G-T: general and Territorial (mst value = 3)}
#' \item{S: spatial only (mst value = 4)}
#' \item{G-S: general and Spatial (mst value = 5)}
#' \item{T-S: territorial and Spatial (mst value = 6)}
#' \item{G-T-S: all deviations (mst value = 7)}
#' }
#' @examples
#' # Prerequisites - Compute the 3 deviations
#' com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
#' com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")
#' com$sdev <- sdev(x = com, var1 = "INC", var2 = "TH", order = 1)
#' 
#' #Example 1 - Wealthiest territorial units
#' # Compute map_mst
#' mst <- map_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev",
#' threshold = 125, superior = TRUE)
#' 
#' # Unlist outputs of the function
#' com <- mst$geom
#' cols <- mst$cols
#' leg_val <- mst$leg_val
#' 
#' # Cartography
#' library(mapsf)
#' par(mar = c(0,0,1.2,0))
#' mf_map(x = com, var = "mst", type = "typo", border = "white", lwd = 0.2,
#'        pal = cols, val_order = unique(com$mst), leg_pos = "n")
#' mf_map(ept, col = NA, border = "black", lwd = 1, add = TRUE)
#' 
#' mf_legend(type = "typo", val = leg_val, pal = cols, 
#'           title = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",
#'           pos = "topleft")
#' 
#' mf_label(x = com[com$mst == 7,], var = "LIBCOM",
#'          cex = 0.6, halo = TRUE, overlap = FALSE)
#' 
#' mf_layout(title = "3-Deviations synthesis : Territorial units above index 125",
#'           credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'                            "\nMTA", packageVersion("MTA")),
#'           arrow = FALSE)
#' 
#' 
#' # Example 2 - Lagging territorial units
#' # Compute map_mst
#' mst <- map_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev",
#'                threshold = 75, superior = FALSE)
#' 
#' # Unlist resulting function
#' com <- mst$geom
#' cols <- mst$cols
#' leg_val <- mst$leg_val
#' 
#' # Cartography
#' par(mar = c(0,0,1.2,0))
#' mf_map(x = com, var = "mst", type = "typo", border = "white", lwd = 0.2,
#'        pal = cols, val_order = unique(com$mst), leg_pos = "n")
#' mf_map(ept, col = NA, border = "black", lwd = 1, add = TRUE)
#' 
#' mf_legend(type = "typo", val = leg_val, pal = cols, 
#'           title = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",
#'           pos = "topleft")
#' 
#' mf_label(x = com[com$mst == 7,], var = "LIBCOM",
#'          cex = 0.6, halo = TRUE, overlap = FALSE)
#' 
#' mf_layout(title = "3-Deviations synthesis : Territorial units below index 75",
#'           credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'                            "\nMTA", packageVersion("MTA")),
#'           arrow = FALSE)
#' 
#' 
#' @importFrom cartography typoLayer legendTypo
#' @export
map_mst <- function(x, gdevrel, tdevrel, sdevrel, threshold, superior = FALSE, xid = NULL){
  
  # Convert sf object in dataframe
  y <- x
  if(class(y)[1] == "sf"){
    y <- st_set_geometry(y, NULL)
  }  
  
  # Delete missing values
  y <- y[!is.na(y[,gdevrel]) & !is.na(y[,tdevrel]) & !is.na(y[,sdevrel]),]
  
  # Compute final typology
  y$tmp1 <- 0
  y$tmp2 <- 0
  y$tmp3 <- 0
  
  if (superior == TRUE)
  {
    y[y[,gdevrel] >= threshold, "tmp1"] <- 1
    y[y[,tdevrel] >= threshold, "tmp2"] <- 2
    y[y[,sdevrel] >= threshold, "tmp3"] <- 4
  }
  
  if (superior == FALSE)
  {
    y[y[,gdevrel] <= threshold, "tmp1"] <- 1
    y[y[,tdevrel] <= threshold, "tmp2"] <- 2
    y[y[,sdevrel] <= threshold, "tmp3"] <- 4
  }
  
  y$mst <- (y$tmp1+y$tmp2+y$tmp3)
  
  if(is.null(xid)) {
    xid <- colnames(y)[1]
  }
  
  # Manage cols and labels for cartography
  coldf <- data.frame(colvec = c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
                                 "#addea6","#ffa100","#fff226","#e30020"),
                      mst = seq(0,7,1),
                      leg_val = c("0","G","T","G-T","S", "G-S", "T-S", "G-T-S"),
                      stringsAsFactors = FALSE)
  
  xx <- st_set_geometry(x, NULL)
  xx <- coldf[coldf$mst %in% y[,"mst"],]
  cols <- xx$colvec 
  leg_val <- as.vector(xx$leg_val)
  
  # Check if mst has already be calculated
  if("mst" %in% colnames(x)){
    x <- x[,!names(x) %in% "mst"]
  }
  
  # Merge with input layer and order it for cartography
  x <- merge(x, y[,c(xid, "mst")], by = xid)
  x <- x[order(match(x[,"mst", drop = TRUE], coldf$mst)),]
  
  return(list("geom" = x, "cols" = cols,  "leg_val" = leg_val))
}
