#' @title Multiscalar Typology (2 deviations)
#' @name bidev
#' @description Compute a multiscalar typology according to two relative 
#' deviations relative. 
#' @param x a sf object including 2 deviations among gdev, tdev and sdev. 
#' @param dev1 column name of the first relative deviation in x.
#' @param dev2 coumn name of the second relative deviation in x.
#' @param breaks Distance to the index 100 (average of the context), in
#'  percentage. A vector of three values. Defaut c(25,50,100). 25 % corresponds
#'  to indexes 80 and 125. 50 % to indexes 67 and 150 and 100 % to indexes 50 
#'  and 200.
#' @param xid identifier field in x. Default the first column. 
#' @return a column in x including a bidev column delivering the result of 
#' the typology. Values are classified in 13 classes according to their 
#' respective position on the two selected deviations and their
#'  distance to the average.
#' \itemize{bidev typology values :
#' \item{ZZ: Near the average for the two selected deviation}
#' \item{A1: Above the average for dev1 and dev2, distance to the avarage : +}
#' \item{A2: Above the average for dev1 and dev2, distance to the avarage : ++}
#' \item{A3: Above the average for dev1 and dev2, distance to the avarage : +++}
#' \item{B1: Above the average for dev1 and below for dev2, distance to the
#'  avarage : +}
#' \item{B2: Above the average for dev1 and below for dev2, distance to the
#'  avarage : ++}
#' \item{B3: Above the average for dev1 and below for dev2, distance to the
#'  avarage : +++}
#' \item{C1: Below the average for dev1 and dev2, distance to the avarage : +}
#' \item{C2: Below the average for dev1 and dev2, distance to the avarage : ++}
#' \item{C3: Below the average for dev1 and dev2, distance to the avarage : +++}
#' \item{D1: Below the average for dev1 and above for dev2, distance to the
#'  avarage : +}
#' \item{D2: Below the average for dev1 and above for dev2, distance to the
#'  avarage : ++}
#' \item{D3: Below the average for dev1 and above for dev2, distance to the
#'  avarage : +++}
#' }
#' @import sf
#' @examples
#' # bidev synthesis on general and territorial deviation (income data)
#' # Load data
#' library(sf)
#' com <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
#' 
#' # Prerequisite  - Compute 2 deviations
#' com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
#' com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")
#' 
#' # Compute bidev
#' com$bidev <- bidev(x = com, dev1 = "gdev", dev2 = "tdev")
#' @export
bidev <- function (x, dev1, dev2, breaks = c(25, 50, 100), xid = NULL) {
  
  # Breaks management
  # Convert in logarithm scale breaks
  d <- breaks
  d <- (d/100) + 1
  
  dVal <- log(c(100 * 1/d[3], 100 * 1/d[3], 100 * 1/d[2], 100 * 1/d[1], 100,
                100 * d[1], 100 * d[2], 100 * d[3], 100 * d[3]))
  
  # Check if bidev has already by calculated
  if("bidev" %in% colnames(x)){
    x <- x[,!names(x) %in% "bidev"]
  }
  
  # Convert in logarithm sale dev1 and dev2
  pt <- x
  pt <- st_set_geometry(pt, NULL)
  pt[,dev1] <- log(pt[,dev1])
  pt[,dev2] <- log(pt[,dev2])
  
  
  # Check if min and max are below / above pre-defined breaks
  minVal <- min(c(min(pt[[dev1]], na.rm = TRUE), min(pt[[dev2]], na.rm = TRUE))) 
  maxVal <- max(c(max(pt[[dev1]], na.rm = TRUE), max(pt[[dev2]], na.rm = TRUE)))
  
  if(minVal < dVal[1]){
    dVal[1] <- minVal}  
  if(maxVal > dVal[9]){
    dVal[9] <- maxVal}  
  
  # Create quadrants
  ZZ <- st_polygon(list(cbind(c(dVal[4], dVal[5], dVal[6] ,dVal[5], dVal[4]),
                              c(dVal[5], dVal[4], dVal[5], dVal[6], dVal[5]))))
  A1 <- st_polygon(list(cbind(c(dVal[5], dVal[5], dVal[7], dVal[6],dVal[5]),
                              c(dVal[6], dVal[7], dVal[5], dVal[5], dVal[6]))))
  A2 <- st_polygon(list(cbind(c(dVal[5], dVal[5], dVal[8], dVal[7], dVal[5]),
                              c(dVal[7], dVal[8], dVal[5], dVal[5], dVal[7]))))
  A3 <- st_polygon(list(cbind(c(dVal[5], dVal[5], dVal[9], dVal[9], dVal[8], dVal[5]),
                              c(dVal[8], dVal[9], dVal[9], dVal[5], dVal[5], dVal[8]))))
  B1 <- st_polygon(list(cbind(c(dVal[6], dVal[7], dVal[5], dVal[5], dVal[6]),
                              c(dVal[5], dVal[5], dVal[3], dVal[4], dVal[5]))))
  B2 <- st_polygon(list(cbind(c(dVal[7], dVal[8], dVal[5], dVal[5], dVal[7]),
                              c(dVal[5], dVal[5],dVal[2], dVal[3], dVal[5]))))
  B3 <- st_polygon(list(cbind(c(dVal[8], dVal[9], dVal[9], dVal[5], dVal[5], dVal[8]),
                              c(dVal[5], dVal[5], dVal[1], dVal[1], dVal[2], dVal[5]))))
  C1 <- st_polygon(list(cbind(c(dVal[3], dVal[4], dVal[5], dVal[5], dVal[3]),
                              c(dVal[5], dVal[5], dVal[4], dVal[3], dVal[5]))))
  C2 <- st_polygon(list(cbind(c(dVal[3], dVal[2], dVal[5], dVal[5], dVal[3]),
                              c(dVal[5], dVal[5], dVal[2], dVal[3], dVal[5]))))
  C3 <- st_polygon(list(cbind(c(dVal[2], dVal[1], dVal[1], dVal[5], dVal[5], dVal[2]),
                              c(dVal[5], dVal[5], dVal[1], dVal[1], dVal[2], dVal[5]))))
  D1 <- st_polygon(list(cbind(c(dVal[4], dVal[3], dVal[5], dVal[5], dVal[4]),
                              c(dVal[5], dVal[5], dVal[7], dVal[6], dVal[5]))))
  D2 <- st_polygon(list(cbind(c(dVal[3], dVal[2], dVal[5], dVal[5], dVal[3]),
                              c(dVal[5], dVal[5], dVal[8], dVal[7], dVal[5]))))
  D3 <- st_polygon(list(cbind(c(dVal[2], dVal[1], dVal[1], dVal[5], dVal[5], dVal[2]),
                              c(dVal[5], dVal[5], dVal[9], dVal[9], dVal[8], dVal[5]))))
  
  # Union of quadrants and fix categories
  quad <- st_sfc(ZZ, A1, A2, A3, B1, B2, B3,C1, C2, C3, D1, D2, D3)
  quad2 <- data.frame(bidev = c("ZZ", "A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", 
                                "C3", "D1", "D2", "D3"), stringsAsFactors = FALSE)
  quad <- st_sf(quad, quad2)
  st_agr(quad) <- "constant"
  
  # Position of observations according to quadrants
  pt <- st_as_sf(pt, coords = c(dev1, dev2))
  st_agr(pt) <- "constant"
  pt <- st_intersection(pt, quad)
  
  # Delete legend and colors labels out of the quadrants 
  pt <- st_set_geometry(pt, NULL)
  
  # Detect id 
  if(is.null(xid)) {
    xid <- colnames(x)[1]}
  
  # Merge with input layer and order it for cartography
  x <- merge(x, pt[,c(xid, "bidev")], by = xid)
  x <- st_set_geometry(x, NULL)
  x <- x[,c("bidev")]
  
  return(x)
}