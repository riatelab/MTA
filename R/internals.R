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



