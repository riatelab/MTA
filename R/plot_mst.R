#' @title Plot Multiscalar Typology (3 deviations)
#' @name plot_mst
#' @description Vizualizing mst for selected territorial units.  
#' @param x a sf object or a dataframe including 3 pre-calculated deviations.  
#' @param gdevrel name of the general relative deviation variable in x.
#' @param tdevrel name of the territorial relative deviation variable in x.  
#' @param sdevrel name of the the spatial relative deviation variable in x.
#' @param lib.var column name of x including territorial units name/code we
#' want to display on the plot.
#' @param lib.val a vector of territorial units included in lib.label we want
#' to display on the plot.
#' @param legend.lab label for explaining the plot (default = "G: general, 
#' T: territorial, S: spatial (relative deviations, average = 100)".
#' @param cex.lab size of the axis legend label text (default = 1).
#' @param cex.axis size of the tick label numbers (default = 0.8).
#' @param cex.names size of the territorial units labels if selected 
#' (default 0.8).
#' @return A barplot displaying the position for selected territorial units
#' on three territorial deviation. Y axis is expressed in logarithm 
#' (25 % above the average corresponding to index 125 and 25 % below the average
#' being index 80).
#' @examples
#' # load data
#' mta_get_data()
#' 
#' # Prerequisite  - Compute the 3 relative deviations
#' com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
#' com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")
#' com$sdev <- sdev(x = com, var1 = "INC", var2 = "TH", order = 1)
#' 
#' # Synthesis barplot (3 territorial units)
#' plot_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", lib.var = "LIBCOM",
#'          lib.val = c("Neuilly-sur-Seine", "Clichy-sous-Bois", "Les Lilas"))
#' @importFrom graphics barplot mtext
#' @export
plot_mst <- function (x, gdevrel, tdevrel, sdevrel, legend.lab = NULL,
                      lib.var,  lib.val, cex.lab = 1, cex.axis = .8, 
                      cex.names = .8){
  
  # Convert sf object in dataframe
  if(class(x)[1] == "sf"){
    x <- sf::st_set_geometry(x, NULL)
  }  
  
  # Select values of interest
  x <- x[x[,lib.var] %in%  lib.val,]
  
  if (nrow(x) != length(lib.val)) {
    stop("Check if lib.val (territorial units names) are correctly set or exist")}
  
  # Select symetric thresholds for the plot according to min/max values (log)
  maxV <- max(x[,c(gdevrel, tdevrel, sdevrel)])
  minV <- min(x[,c(gdevrel, tdevrel, sdevrel)])
  
  maxV <- ifelse(maxV/100 > 100/minV, maxV/100, 100/minV)
  minV <- round((100/maxV), -1) - 10
  maxV <- round(maxV * 100, -1) + 10
  
  # Prepare Y-Axis
  if (minV < 50 & maxV > 200) {
    yVal <- c(minV, 50, 67, 80, 100, 125, 200, maxV)
  } else {
    yVal <- c(50, 67, 80, 100, 125, 200)
    minV <- 50
    maxV <- 200
  } 

  # Transform in matrix and reorder columns according to user selection
  row.names(x) <- x[,lib.var]
  x <- t(as.matrix(x[,c(gdevrel, tdevrel, sdevrel)]))
  x <- x[, lib.val]
  
  # Manage colors
  cols <- ifelse(as.vector(x) >= 100, "#fac5c5", "#c5d9fa")
  
  # Manage space between bars
  space <- rep(c(0, 0, .3),length(lib.val))
  space <- space[1:(length(space)-1)]
  space <- c(0,space)
  
  # barplot
  bplot <- barplot(log(x) - log(100),
                   beside = TRUE,
                   ylim = c(log(minV) - log(100), log(maxV) - log(100)),
                   yaxt = "n",
                   col = cols,
                   border = "white",
                   names.arg = colnames(x),
                   space = space,
                   cex.axis = cex.axis,
                   cex.names = cex.names)
  
  # YLab values
  axis(2, at = log(yVal) - log(100), labels = yVal, cex.axis = cex.axis)
  abline(h = log(yVal) - log(100), col = "lightgrey", lty = 3, lwd = .5)
  
  # Deviation text
  text(y = 0.05, x = bplot, labels = rep(c("G", "T", "S"), length(lib.val)), 
       cex = 0.8, adj = 0.5, font = 2)
  abline(h = 0, lwd = 1, col = "black")
  
  if (is.integer(ncol(x))) {
  abline(v = seq(3.15, 3.15 * ncol(x), by = 3.3), col = "black")}
  
  if(is.null(legend.lab)){
    mtext (text = "G: general, T: territorial, S: spatial (relative deviations, 100 = average)",
           side = 3, adj = 0, line = .2, cex = .8, font = 3) 
    
  } else{
    mtext(text = legend.lab, side = 3, adj = 0, line = .2, cex = .8, font = 3) 
  }
  
}