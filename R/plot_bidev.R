#' @title Plot Multiscalar Typology (2 deviations)
#' @name plot_bidev
#' @description Vizualizing bidev and select some territorial units on it.  
#' @param x a sf object or a dataframe including 2 pre-calculated deviations.  
#' @param dev1 column name of the first relative deviation in x.
#' @param dev2 coumn name of the second relative deviation in x.
#' @param breaks distance to the index 100 (average of the context), in
#'  percentage. A vector of three values. Defaut c(25,50,100). 25 % corresponds
#'  to indexes 80 and 125. 50 % to indexes 67 and 150 and 100 % to indexes 50 
#'  and 200.
#' @param dev1.lab label to be put in x-axis of the scatter plot (default: NULL).
#' @param dev2.lab label to be put in y-axis of the scatter plot (default: NULL).
#' @param lib.var column name of x including territorial units name/code we
#' want to display on the plot.
#' @param lib.val a vector of territorial units included in lib.label we want
#' to display on the plot.
#' @param cex.lab size of the axis label text (default = 1).
#' @param cex.axis size of the tick label numbers (default = 0.7).
#' @param cex.pt size of the dot used for extract specific territorial units 
#' (default 0.5).
#' @param cex.names size of the territorial units labels if selected 
#' (default 0.8).
#' @param pos.names position of territorial units labels (default 4, to the right).
#' @return A scatter-plot displaying the 13 bidev categories, which are the synthesis
#' of the position of territorial units according to 2 deviations and their
#' respective distance to the average. X-Y axis are expressed in logarithm 
#'(25 % above the average corresponding to index 125 and 25 % below the average
#' being index 80). 
#' \itemize{bidev typology values :
#' \item{ZZ: Near the average for the two selected deviation, in grey}
#' \item{A1: Above the average for dev1 and dev2, distance to the avarage : +,
#' in light red}
#' \item{A2: Above the average for dev1 and dev2, distance to the avarage : ++,
#' in red}
#' \item{A3: Above the average for dev1 and dev2, distance to the avarage : +++,
#' in dark red}
#' \item{B1: Above the average for dev1 and below for dev2, distance to the
#'  avarage : +, in light yellow}
#' \item{B2: Above the average for dev1 and below for dev2, distance to the
#'  avarage : ++, in yellow}
#' \item{B3: Above the average for dev1 and below for dev2, distance to the
#'  avarage : +++, in dark yellow}
#' \item{C1: Below the average for dev1 and dev2, distance to the avarage : +,
#' in light blue}
#' \item{C2: Below the average for dev1 and dev2, distance to the avarage : ++,
#' in blue}
#' \item{C3: Below the average for dev1 and dev2, distance to the avarage : +++,
#' in dark blue}
#' \item{D1: Below the average for dev1 and above for dev2, distance to the
#'  avarage : +, in light green}
#' \item{D2: Below the average for dev1 and above for dev2, distance to the
#'  avarage : ++, in green}
#' \item{D3: Below the average for dev1 and above for dev2, distance to the
#'  avarage : +++, in dark green}
#' }
#' @examples
#' # Load data
#' library(sf)
#' com <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
#' 
#' # Prerequisite  - Compute 2 deviations
#' com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
#' com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")
#' 
#' # EX1 standard breaks with four labels
#' plot_bidev(x = com, 
#'            dev1 = "gdev", 
#'            dev2 = "tdev",
#'            dev1.lab = "General deviation (MGP Area)",
#'            dev2.lab = "Territorial deviation (EPT of belonging)",
#'            lib.var = "LIBCOM",
#'            lib.val =  c("Marolles-en-Brie", "Suresnes", 
#'                         "Clichy-sous-Bois", "Les Lilas"))
#' 
#' # EX2, change breaks, enlarge breaks 
#' plot_bidev(x = com,
#'            breaks = c(75, 150, 300),
#'            dev1 = "gdev", 
#'            dev2 = "tdev", 
#'            dev1.lab = "General deviation (MGP Area)",
#'            dev2.lab = "Territorial deviation (EPT of belonging)")
#' @importFrom graphics par axis polygon abline points text
#' @export
plot_bidev <- function (x, dev1, dev2, breaks = c(25, 50, 100), 
                        dev1.lab = NULL, dev2.lab = NULL,  lib.var = NULL, 
                        lib.val = NULL, cex.lab = 1, cex.axis = .7, cex.pt = .5, 
                        cex.names =.8, pos.names = 4){
  
  # Breaks management
  # Convert in logarithm scale
  d <- breaks
  d <- (d/100) + 1
  
  dVal <- c(100 * 1/d[3], 100 * 1/d[3], 100 * 1/d[2], 100 * 1/d[1], 100,
            100 * d[1], 100 * d[2], 100 * d[3], 100 * d[3])
  
  # Check if min and max are below / above pre-defined breaks
  minVal <- min(c(min(x[[dev1]], na.rm = TRUE), min(x[[dev2]], na.rm = TRUE))) 
  maxVal <- max(c(max(x[[dev1]], na.rm = TRUE), max(x[[dev2]], na.rm = TRUE)))
  
  if(minVal < dVal[1]){
    dVal[1] <- minVal
  }  
  
  if(maxVal > dVal[9]){
    dVal[9] <- maxVal
  }  
  
  # Color management
  colvec <- c("lightgrey", "#e58989","#dd5858","#b20202", "#edd296",
              "#e5bd65", "#e5a413", "#7295d3", "#3b69bc", "#033793",
              "#8ac198","#318c49","#025b15")
  
  # Scatter-plot
  opar <- par(mar = c(4,4,4,4),  pty = "s")  
  on.exit(par(opar), add = TRUE)
  # Standard scale
  plot((dVal[1] - 0.1) : (dVal[9] + 0.5),
       (dVal[1] - 0.1) : (dVal[9] + 0.5),
       xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i",
       type = "n", xlab = dev1.lab, ylab = dev2.lab,
       cex.lab = cex.lab, log = "xy")
  
  axis(1, at = round(c(minVal,dVal, maxVal), 0), cex.axis = cex.axis)
  axis(2, at = round(c(minVal, dVal,maxVal), 0), cex.axis = cex.axis)
  
  polygon(x = c(dVal[4], 100, dVal[6] ,100), 
          y = c(100, dVal[4], 100, dVal[6]), col = colvec[1], border = NA) # Type ZZ
  
  polygon(x = c(100, 100, dVal[7], dVal[6]),
          y = c(dVal[6], dVal[7], 100, 100), col = colvec[2], border = NA) # Type A1
  polygon(x = c(100, 100, dVal[8], dVal[7]),
          y = c(dVal[7], dVal[8], 100, 100), col = colvec[3], border = NA) # Type A2
  polygon(x = c(100, 100,  dVal[9], dVal[9], dVal[8]), 
          y = c(dVal[8], dVal[9], dVal[9], 100, 100), col = colvec[4], border = NA) # Type A3
  
  polygon(x = c(dVal[6], dVal[7], 100, 100),
          y = c(100, 100, dVal[3], dVal[4]), col = colvec[5], border = NA) # Type B1
  polygon(x = c(dVal[7], dVal[8], 100, 100),
          y = c(100, 100,dVal[2], dVal[3]), col = colvec[6], border = NA) # Type B2
  polygon(x = c(dVal[8], dVal[9], dVal[9], 100, 100), 
          y = c(100, 100, dVal[1], dVal[1], dVal[2]), col = colvec[7], border = NA) # Type B3
  
  polygon(x = c(dVal[3], dVal[4], 100, 100),
          y = c(100, 100, dVal[4], dVal[3]), col = colvec[8], border = NA) # Type C1
  polygon(x = c(dVal[3], dVal[2], 100, 100),
          y = c(100, 100, dVal[2], dVal[3]), col = colvec[9], border = NA) # Type C2
  polygon(x = c(dVal[2], dVal[1], dVal[1], 100, 100), 
          y = c(100, 100, dVal[1], dVal[1], dVal[2]), col = colvec[10], border = NA) # Type C3
  
  polygon(x = c(dVal[4], dVal[3], 100, 100),
          y = c(100, 100, dVal[7], dVal[6]), col = colvec[11], border = NA) # Type D1
  polygon(x = c(dVal[3], dVal[2], 100, 100),
          y = c(100, 100, dVal[8], dVal[7]), col = colvec[12], border = NA) # Type D2
  polygon(x = c(dVal[2], dVal[1], dVal[1], 100, 100), 
          y = c(100, 100, dVal[9], dVal[9], dVal[8]), col = colvec[13], border = NA) # Type D3
  
  abline (h = dVal, col = "white", lwd = 0.25, lty = 3)
  abline (v = dVal, col = "white", lwd = 0.25, lty = 3)
  abline (h = 100, col = "white", lwd = 1, lty = 1)
  abline (v = 100, col = "white", lwd = 1, lty = 1)
  
  # Add points
  if(class(x)[1] == "sf"){
    x <- st_set_geometry(x, NULL)
  }  
  
  # Add all points
  points(x = x[,dev1], y= x[,dev2], type="p", pch = 16, col = "black", cex = cex.pt)
  
  # Outline specific point
  if(!is.null(lib.var)) {
    y <- x[x[,lib.var] %in%  lib.val,]
    
    points(x = y[,dev1], y= y[,dev2], type="p", pch = 23, bg= "white", col = "red", cex= cex.pt*3.5)
    text(x = y[,dev1], y= y[,dev2], labels = y[,lib.var], cex = cex.names, pos = pos.names, col = "white")
  }
  
}
