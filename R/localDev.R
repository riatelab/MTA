#' @title Local Deviation
#' @name localDev
#' @description This function computes the deviation between regional ratios and 
#' local ratios. Local ratios are defined either by a contiguity order or by a 
#' distance measure between regions.
#' Each elementary unit value will be compared to the value of its 
#' neighborhood.
#' @param spdf a SpatialPolygonsDataFrame.
#' @param x a data frame.
#' @param spdfid identifier field in spdf, default to the first column 
#' of the spdf data frame (optional).
#' @param xid identifier field in x, default to the first column 
#' of x (optional)
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param order contiguity order (dist and mat are not used).
#' @param dist distance defining the contiguity (order is not used).
#' @param mat a distance matrix (road distance, travel time...). Row and column 
#' names must fit xid identifiers.
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned.
#' @import sp
#' @export
#' 
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
  # row.names(spdf) <- spdf@data[,xid]
  
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