#' @title Multiscalar Absolute Synthesis
#' @name mas
#' @description This function sums the total amount of redistributions according to the three absolute 
#' deviations (global, territorial, spatial). 
#' @param x a sf object or a dataframe including gdevabs, tdevabs, sdevabs and num columns. 
#' @param gdevabs name of the general absolute deviation variable in x.
#' @param tdevabs name of the territorial absolute deviation variable in x.  
#' @param sdevabs name of the spatial absolute deviation variable in x.
#' @param num name of the numerator used for computing the 3 absolute deviations in x.
#' @return A dataframe including the mass of numerator to redistribue to reach a perfect equilibrium 
#' according to the 3 contexts, expressed in numerator measure unit and as a share of the numerator mass.
#' @export
#' @examples
#' data("GrandParisMetropole")
#' # general absolute deviation 
#' com$gdevabs <- gdev(x = com, var1 = "INC",var2 = "TH", type = "abs")
#' 
#' # Territorial absolute deviation calculation
#' com$mdevabs <- tdev(x = com,  var1 = "INC", var2 = "TH", type = "abs",
#'                     key = "LIBEPT")
#' 
#' # Local absolute deviation calculation redistribution 
#' com$ldevabs <- sdev(x = com, xid = "DEPCOM", var1 = "INC", var2 = "TH",
#'                     order = 1, type = "abs")
#' 
#' # Compute the synthesis DataFrame (absolute deviations)
#' mas(x = com, 
#'     gdevabs = "gdevabs", 
#'     tdevabs = "mdevabs",
#'     sdevabs = "ldevabs",
#'     num = "INC") 




mas <- function(x, gdevabs, tdevabs, sdevabs, num){
  
  # convert to dataframe
  if (methods::is(x, "sf")){
    x <- st_set_geometry(x, NULL)
  }
  
  # Delete missing values
  x <- x[!is.na(x[,gdevabs]) & !is.na(x[,tdevabs]) & !is.na(x[,sdevabs]),]

  # Select all the redistribution above 0 on the three deviations and numerator, and sum it
  gdevcon <- x[x[,gdevabs] > 0, gdevabs]
  gdevcon <- sum(gdevcon)
 
  tdevcon <- x[x[,tdevabs] > 0, tdevabs]
  tdevcon <- sum(tdevcon)
  
  sdevcon <- x[x[,sdevabs] > 0, sdevabs]
  sdevcon <- sum(sdevcon)
  
  numtot <- sum(x[,num])
  
  y <- data.frame(matrix(1,4,2))
  rownames(y) <- c("General redistribution", "Territorial redistribution",
                   "Spatial redistribution", "Overall Numerator mass")
  colnames(y) <- c("Numerator to be transfered", "Share of the total (%)")
  
  y[,1] <- c(gdevcon, tdevcon, sdevcon, numtot)
  y[,2] <- round(y[,1]/ y[4,1] * 100, 2)

  return(y)
}
