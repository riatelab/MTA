#' @title Global Deviation
#' @name globalDev
#' @description This function compute the global deviation from two 
#' count data according to the HyperAtlas Methodolgy (methodo), see details.
#' @details The general context may be the whole chosen study area. 
#' In such a case, the associated map will be the same as the associated 
#' map to the ratio itself. So, the user may choose another general context 
#' or a reference value. For instance, in the example of the EU, even if 
#' the study area is the 29 potential countries, it may be of interest 
#' to observe the spatial differentiations according to another global 
#' reference, for instance the global value associated to EU15. For this 
#' level, the user may also exogenously enter a value. By default, this value 
#' has first been set to the value of the global area. 
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if NULL the mean ratio.
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned
#' @examples 
#' library(cartography)
#' x <- nuts3.df
#' x$gdppps2008 <- x$gdppps2008 * 1000000
#' x[substr(x = x$id, start = 1, stop = 2) == "DE", "pop2008"] <- NA
#' x$gdevabs <- globalDev(x = x, 
#'                        var1 = "gdppps2008", 
#'                        var2 = "pop2008", 
#'                        type = "abs")
#' x$gdevrel <- globalDev(x = x, 
#'                        var1 = "gdppps2008", 
#'                        var2 = "pop2008", 
#'                        type = "rel")
#' par(mar = c(0,0,1.1,0))
#' choroLayer(spdf = nuts3.spdf, df = x, var = "gdevrel", 
#'            legend.pos = "topright", 
#'            breaks = c(11,50,75,100,125,150,7000), 
#'            col = carto.pal(pal1 = "blue.pal", n1 = 3, 
#'                            pal2 = "wine.pal", n2 = 3))
#' propSymbolsLayer(spdf = nuts3.spdf, df = x, var = "gdevabs", 
#'                  legend.pos = "right",
#'                  col = "#ff000050",col2 = "#0000ff50", 
#'                  k = 0.001, legend.style = "e",
#'                  breakval = 0)
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

#' @title Medium Deviation
#' @name mediumDev
#' @description This function compute the medium deviation from two count 
#' data according to the HyperAtlas Methodolgy.
#' @details The territorial context, on the other hand, has to be a 
#' geographical zoning that is an aggregation of the “elementary zoning” 
#' that was previously chosen.
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param key aggregation key field.
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned
#' @examples
#' library(cartography)
#' x <- nuts3.df
#' x$gdppps2008 <- x$gdppps2008 * 1000000
#' x[substr(x = x$id, start = 1, stop = 2) == "BE", "pop2008"] <- NA
#' x$key <- substr(x = x$id, start = 1, stop = 3)
#' x$mdevabs <- mediumDev(x = x, var1 = "gdppps2008", var2 = "pop2008", 
#'                        type = "abs", key = "key")
#' x$mdevrel <- mediumDev(x = x, var1 = "gdppps2008", 
#'                        var2 = "pop2008", 
#'                        type = "rel", key = "key")
#' par(mar = c(0,0,0,0))
#' plot(nuts0.spdf[nuts0.spdf$id %in% c("BE", "NL", "LU", "DE", "FR"),] )
#' choroLayer(spdf = nuts3.spdf, df = x, var = "mdevrel", 
#'            legend.pos = "topright", breaks = c(11,50,75,100,125,150,7000), 
#'            col = carto.pal(pal1 = "blue.pal", 3, pal2 = "wine.pal", 3), 
#'            border = NA, add=TRUE)
#' plot(nuts1.spdf, add=TRUE)
#' propSymbolsLayer(spdf = nuts3.spdf, df = x, var = "mdevabs", 
#'                  col = "#ff000050",col2 = "#0000ff50", k = 0.001, 
#'                  breakval = 0)
#' @export
mediumDev <- function(x, var1, var2, key, type = "rel"){
  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)
  
  # aggregate values by key
  med <- aggregate(x[,c(var1, var2)], by = list(x[,key]), sum)
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

#' @title Local deviation
#' @name localDev
#' @description This function compute the local deviation from two count 
#' data according to the MyperAtlas Methodolgy.
#' @details The spatial context shows which proximity relation will be 
#' the basis of the neighborhood’s definition for each elementary unit. 
#' That is usually “contiguity”, but it may also be a relationship based 
#' on distances since they have been introduced in the hyp file 
#' (units that are less than X kilometers far from), or time-distances. 
#' Then, each elementary unit value will be compared to the value of its 
#' neighborhood.
#' @param spdf a SpatialPolygonsDataFrame.
#' @param x a data frame.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame. (optional)
#' @param xid identifier field in x, default to the first column 
#' of x (optional)
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param order contiguity order.
#' @param dist distance defining the contiguity.
#' @param mat use an extern distance matrix (road km, time...).
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned.
#' @export
localDev<-function(spdf, x, spdfid = NULL, xid = NULL, var1, var2, 
                   order = NULL, dist = NULL, mat = NULL, type = "rel"){
  
  if(is.null(order) == FALSE & is.null(dist) == FALSE){
    stop("
Define local deviation either by order or by dist, not by both.",
         call. = FALSE)
  }
  
  # check ids
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(xid)){xid <- names(x)[1]}
  
  # test for NAs
  vtot <- row.names(x)
  x <- testNAdf(x = x, var1 = var1, var2 = var2)
  vpar <- row.names(x)
  
  # Selecect spdf units
  spdf <- spdf[na.omit(match(x = x[,xid], table = spdf@data[,spdfid])),]
  
  # contig dev
  if (!is.null(order)){
    mat <- contiguityMat(spdf = spdf)
    mat[mat <= order] <- 1
    mat[mat > order] <- 0
    v <- locmat(mat = mat, x = x, var1 = var1, var2 = var2, type = type)
  }
  
  # dist dev
  if (!is.null(dist)){
    if (is.null(mat)){
      mat <- distanceMat(spdf = spdf)
    }else{
      rnames <- factor(row.names(spdf), levels = row.names(spdf))
      mat <- mat[levels(rnames), levels(rnames)]
    }
    mat[mat <= dist] <- 1
    mat[mat > dist] <- 0
    v <- locmat(mat = mat, x = x, var1 = var1, var2 = var2, type = type)
  }
  v <- v[match(vtot, vpar)]
  return(v)
}

#' @title Contiguity Matrix
#' @name contiguityMat 
#' @description This function compute a contiguity matrix from a 
#' SpatialPolygonsDataFrame, topologic distance matrix
#' @param spdf a SpatialPolygonsDataFrame.
#' @return A topological distance matrix is returned
#' @noRd
contiguityMat <- function(spdf){
  mat <- rgeos::gIntersects(spdf, byid = TRUE, prepared=TRUE)
  g <- igraph::graph.adjacency(adjmatrix = mat, weighted = NULL)
  x <- igraph::shortest.paths(g, mode="out")
  return(x)  
}

#' @title Distance Matrix
#' @name distanceMat 
#' @description This function compute a distance matrix from a 
#' SpatialPolygonsDataFrame.
#' @param spdf a SpatialPolygonsDataFrame.
#' @return A distance matrix is returned.
#' @noRd
distanceMat <- function(spdf){
  spdf.pt <- rgeos::gCentroid(spdf, byid = TRUE, id = NULL)
  x <- rgeos::gDistance(spdf.pt, byid = TRUE)
  return(x)  
}

#' @title Local Divergence
#' @name locmat
#' @description This function compute a the local divergence
#' @param mat a boolean matrix
#' @param x a data.frame
#' @param var1 a variable name in x.
#' @param var2 a variable name in x.
#' @param type type of divergence.
#' @return A vector of divergence is outputed
#' @noRd
locmat <-function(mat, x, var1, var2, type){
  mvar1 <- mat * x[,var1]
  mvar2 <- mat * x[,var2]
  if (type == "rel"){
    v <- ((x[,var1] / x[,var2]) / (colSums(mvar1) / colSums(mvar2))) * 100
  }
  if (type == "abs"){
    v <- x[,var1] - (x[,var2] * (colSums(mvar1) / colSums(mvar2)))
  }
  return(v)
}

#' @title Test NA Values in data.frame
#' @name testNAdf
#' @description This function extract row without any NA values.
#' @param x a data.frame.
#' @param var1 a variable name in x.
#' @param var2 a variable name in x.
#' @noRd
testNAdf <- function(x, var1, var2){
  x <- x[!is.na(x[,var1]) & !is.na(x[,var2]),]
  return(x)
}


#' Multiscalar typology
#'
#' This function compute a multiscalar typology according to the three relative positions about contexts (global, medium, local). The elementary units are classified in eight classes according to their three relative positions.
#'
#' @details xxx
#'
#' @param spdf Spatial polygon data frame
#' @param x h
#' @param spdfid g
#' @param xid dhd
#' @param var1 Numerator
#' @param var2 Denominator
#' @param ref distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
#' @param key Aggregation ky for the medium deviatiopn
#' @param order Threshold of typology
#' @param dist If true, the criterium is "gretter than". If false, the criterium is "lower than"
#' @param mat If true, the typology is ploted
#' @param threshold fh 
#' @param superior hdfsh
#' @export
synthesis3 <- function(spdf, x, spdfid = NULL, xid = NULL,
                      var1, var2,
                      ref = NULL,
                      key,
                      order = NULL, dist = NULL, mat = NULL,
                      threshold, superior = FALSE){

  # check order & dist
  if(is.null(order) == FALSE & is.null(dist) == FALSE){
    stop("
         Define local deviation either by order or by dist, not by both.",
         call. = FALSE)
  }

  # check ids
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(xid)){xid <- names(x)[1]}

  x <- x[, c(xid, var1, var2, key)]
  type = "rel"

  x$ratio <- x[,var1] / x[,var2]
  x$gdevrel <- globalDev(x = x, var1 = var1, var2 = var2, ref = NULL, type = type)
  x$mdevrel <- mediumDev(x = x, var1 = var1, var2 = var2, key = key, type = type)
  x$ldevrel <- localDev(spdf = spdf, x = x, spdfid = spdfid, xid = xid,
                  var1 = var1, var2 = var2, order = order, dist = dist,
                  mat = mat, type = type)
  x$tmp1 <- 0
  x$tmp2 <- 0
  x$tmp3 <- 0
  if (superior == TRUE)
  {
    x[x$gdevrel >= threshold, "tmp1"] <- 1
    x[x$mdevrel >= threshold, "tmp2"] <- 2
    x[x$ldevrel >= threshold, "tmp3"] <- 4
  }
  if (superior == FALSE)
  {
    x[x$gdevrel <= threshold, "tmp1"] <- 1
    x[x$mdevrel <= threshold, "tmp2"] <- 2
    x[x$ldevrel <= threshold, "tmp3"] <- 4
  }

  x$synthesis3 <- as.factor(x$tmp1+x$tmp2+x$tmp3)
  colours <- c("#ffffff", "#fdc785","#ffffab","#fba9b0",
               "#addea6","#ffa100","#fff226","#e30020")

  value <- as.numeric(levels(x$synthesis3))
  colours <- data.frame (value,colours)
  x <- data.frame(x, "colours"=colours[match(x$synthesis3,colours$value),c("colours")])

  return(x)
}
# 
# 
# 
#   cartography::typoLayer(spdf = spdf, df = x, spdfid = spdfid, dfid = xid, 
#                          var = "t", col = colours, 
#                          legend.pos = "n")
#   
#   rVal<-c(" .   .   . ","[X]  .   . ",
#           " .  [X]  . ","[X] [X]  . ",
#           " .   .  [X]","[X]  .  [X]",
#           " .  [X] [X]","[X] [X] [X]")
#   cartography::legendTypo(col = colours, categ = rVal)
#   return(invisible(x))
# }







# 
# # TYPOLOGY (3) ----------------------
# 
# #' Multiscalar typology
# #'
# #' This function compute a multiscalar typology according to the three relative positions about contexts (global, medium, local). The elementary units are classified in eight classes according to their three relative positions.
# #' 
# #' @details xxx
# #' 
# #' @param spdf Spatial polygon data frame
# #' @param id Unique identifier of the data frame (character)
# #' @param stock1 Numerator
# #' @param stock2 Denominator
# #' @param dist distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
# #' @param key Aggregation ky for the medium deviatiopn
# #' @param threshold Threshold of typology
# #' @param superior If true, the criterium is "gretter than". If false, the criterium is "lower than" 
# #' @param map If true, the typology is ploted
# #' 
# #' @examples
# #' # synthesis (without plot)
# #' typology <-synthesis(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",distance=100000,key="nuts0",threshold=125,superior=TRUE,map=FALSE)
# #' 
# #' # synthesis (with plot)
# #' typology <-synthesis(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",distance=100000,key="nuts0",threshold=125,superior=TRUE,map=TRUE)
# #' 
# #' @return dataframe
# #' 
# #' @export
# 
# 
# 
# synthesis <- function(spdf,id,stock1,stock2,distance=0,key,threshold=100,superior=TRUE,map=FALSE){
#   
# synthesis <- function(spdf, x, spdfid = NULL, xid = NULL, 
#                       var1, var2, 
#                       ref = NULL, 
#                       key,
#                       order = NULL, dist = NULL, mat = NULL, 
#                       threshold = 100, superior = FALSE){
#   
#   # check order & dist
#   if(is.null(order) == FALSE & is.null(dist) == FALSE){
#     stop("
# Define local deviation either by order or by dist, not by both.",
#          call. = FALSE)
#   }
# 
#   # check ids
#   if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
#   if (is.null(xid)){xid <- names(x)[1]}
#   
#   x <- x[, c(xid, var1, var2, key)]
#   type = "rel"
# 
#   x$r <- x[,var1] / x[,var2]
#   x$g <- globalDev(x = x, var1 = var1, var2 = var2, ref = ref, type = type)
#   x$m <- mediumDev(x = x, var1 = var1, var2 = var2, key = key, type = type)
#   x$l <- localDev(spdf = spdf, x = x, spdfid = spdfid, xid = xid, 
#                   var1 = var1, var2 = var2, order = order, dist = dist, 
#                   mat = mat, type = type)
#   x$tmp1 <- 0
#   x$tmp2 <- 0
#   x$tmp3 <- 0
#   if (superior == TRUE)
#   {
#     x[x$g >= threshold, "tmp1"] <- 1
#     x[x$m >= threshold, "tmp2"] <- 2
#     x[x$l >= threshold, "tmp3"] <- 4
#   }
#   if (superior == FALSE)
#   {
#     x[x$g <= threshold, "tmp1"] <- 1
#     x[x$m <= threshold, "tmp2"] <- 2
#     x[x$l <= threshold, "tmp3"] <- 4
#   }
# 
#   x$t <- as.factor(x$tmp1+x$tmp2+x$tmp3)
#   colours <- c("#ffffff", "#fdc785","#ffffab","#fba9b0",
#                 "#addea6","#ffa100","#fff226","#e30020")
#   colours <- colours[as.numeric(levels(x$t))+1]
#   cartography::typoLayer(spdf = spdf, df = x, spdfid = spdfid, dfid = xid, 
#                          var = "t", col = colours, 
#                          legend.pos = "n")
# 
#   rVal<-c(" .   .   . ","[X]  .   . ",
#           " .  [X]  . ","[X] [X]  . ",
#           " .   .  [X]","[X]  .  [X]",
#           " .  [X] [X]","[X] [X] [X]")
#   cartography::legendTypo(col = colours, categ = rVal)
#   return(invisible(x))
# }
    # typology@data$typo<-typology@data$tmp1+typology@data$tmp2+typology@data$tmp3
#   typology@data$ratio <- typology@data[,stock1]/typology@data[,stock2]
#   typology@data <- typology@data[,c(id,stock1,stock2,"ratio","global","medium","local","typo")]
#   typology@data$typo<-as.factor(typology@data$typo)
#   
#   # colors
#   colours<-c("#ffffff","#fdc785","#ffffab","#fba9b0","#addea6","#ffa100","#fff226","#e30020")
#   typology@data$col<-colours[1]
#   typology@data$col[typology@data$typo==1]<-colours[2]
#   typology@data$col[typology@data$typo==2]<-colours[3]
#   typology@data$col[typology@data$typo==3]<-colours[4]
#   typology@data$col[typology@data$typo==4]<-colours[5]
#   typology@data$col[typology@data$typo==5]<-colours[6]
#   typology@data$col[typology@data$typo==6]<-colours[7]
#   typology@data$col[typology@data$typo==7]<-colours[8]
#   
#   # PLOT
#   if(map==T){
#     cols <- typology@data$col
#     rVal<-c(" .   .   . ","[X]  .   . "," .  [X]  . ","[X] [X]  . "," .   .  [X]","[X]  .  [X]"," .  [X] [X]","[X] [X] [X]")
#     if (superior==T){symbol = ">="}else{symbol = "<="}
#     if (distance==0){mydist = "contiguity"}else {mydist = paste(distance," units",sep="")}
#     plot(typology, col=cols,lwd=0.2,border="black")
#     
#     # ICI, ploter le niveau intermédiaire
#     buff<-gBuffer(spdf, byid=TRUE, id=NULL, width=1, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
#     mediumLayer<-gUnaryUnion(buff,id = buff@data[,key])
#     plot(mediumLayer,col="#FFFFFF00",lwd=1,add=T)
#     
#     title(main=paste("Multiscalar typology\n(",stock1," / ",stock2,")",sep=""),sub=paste("params: local deviation = ",mydist, " medium deviation = ",key),cex.sub=0.7)
#     legend("topright",legend=rVal, fill=colours,bty="n",pt.cex=1,cex=0.7)
#     ,title=paste("Deviation ",symbol," ",threshold,"\n[glo] [med] [loc]",sep=""))
#   }
#   
#   return(typology)
# 
