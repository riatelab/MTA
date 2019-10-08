#' @title Contiguity Matrix
#' @name contiguityMat 
#' @description This function compute a contiguity matrix from a 
#' sf object, topologic distance matrix
#' @param x a sf object.
#' @return A topological distance matrix is returned
#' @noRd
contiguityMat <- function(x, xid){
  mat <- sf::st_intersects(x, sparse = FALSE, prepared=TRUE)
  colnames(mat) <- x[[xid]]
  rownames(mat) <- x[[xid]] 
  g <- igraph::graph.adjacency(adjmatrix = mat, weighted = NULL)
  x <- igraph::shortest.paths(g, mode="out")
  return(x)  
}



#' @title Distance Matrix
#' @name distanceMat 
#' @description This function compute a distance matrix from a 
#' sf object.
#' @param spdf a sf object.
#' @return A distance matrix is returned.
#' @noRd
distanceMat <- function(x, xid){
  st_agr(x) = "constant"
  x.pt <- sf::st_centroid(x, of_largest_polygon = TRUE)
  x <- sf::st_distance(x.pt, by_element = FALSE)
  colnames(x) <- x.pt[[xid]]
  rownames(x) <- x.pt[[xid]]
  attr(x, "units") <- NULL
  class(x) <- setdiff(class(x),"units")
  return(x)  
}
?setdiff


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
### BE CAREFUL / CHANGE
locmat <- function(mat, x, var1, var2, type){
  # if (unlist(class(x)[1]) == "sf"){
  #   x <- st_set_geometry(x, NULL)
  # }
  # if (unlist(class(x)[1]) == "SpatialPolygonsDataFrame"){
  #   x <- x@data
  # }
  mvar1 <- mat * x[,var1]
  mvar2 <- mat * x[,var2]
  if (type == "rel"){
    v <- ((x[,var1] / x[,var2]) / (colSums(mvar1, na.rm = TRUE) / colSums(mvar2, na.rm = TRUE))) * 100
  }
  if (type == "abs"){
    v <- x[,var1] - (x[,var2] * (colSums(mvar1, na.rm = TRUE) / colSums(mvar2, na.rm = TRUE)))
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
