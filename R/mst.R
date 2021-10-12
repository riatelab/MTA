#' @title Multiscalar Typology (3 deviations)
#' @name mst
#' @description Compute a multiscalar typology according to the three relative 
#' deviations (general: G, territorial: T and spatial: S). The elementary units are classified
#' in eight classes according to their three relative positions.
#' @param x a sf object or a dataframe including gdev, tdev and sdev columns. 
#' @param gdevrel name of the general relative deviation variable in x.
#' @param tdevrel name of the territorial relative deviation variable in x.  
#' @param sdevrel name of the spatial relative deviation variable in x.
#' @param threshold defined to build the typology (100 is considered as the average).
#' @param superior if TRUE, deviation values must be greater than threshold. If FALSE, deviation values must be lower than threshold.
#' @return a vector in x including the mst typology. Values are classified in 
#' 8 classes following their respective position above/under the threshold:
#' \itemize{Typology (which deviation is above/under the threshold):
#' \item{0: none}
#' \item{1: G }
#' \item{2: T}
#' \item{3: G and T}
#' \item{4: S}
#' \item{5: G and S}
#' \item{6: T and S}
#' \item{7: G, T and S}
#' }
#' @examples
#' # Load data
#' com <- sf::st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
#' 
#' # Prerequisite  - Compute the 3 deviations
#' com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
#' com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")
#' com$sdev <- sdev(x = com, var1 = "INC", var2 = "TH", order = 1)
#' 
#' # Multiscalar typology - wealthiest territorial units
#' # Compute mst
#' com$mstW <- mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", 
#'                 threshold = 125, superior = TRUE)
#' 
#' #Multiscalar typology - lagging territorial units
#' # Compute mst
#' com$mstP <- mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", 
#'                 threshold = 75, superior = FALSE)
#' @export
mst <- function(x, gdevrel, tdevrel, sdevrel, threshold, superior = FALSE){
  
  # convert to dataframe
  if (methods::is(x, "sf")){
    x <- st_set_geometry(x, NULL)
  }
  
  # Delete missing values
  y <- x[!is.na(x[,gdevrel]) & !is.na(x[,tdevrel]) & !is.na(x[,sdevrel]),]
  
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
  y <- y[,"mst"]
  return(y)
}
