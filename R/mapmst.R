#' @title Map MultiScalar Typology
#' @name mapmst
#' @description Map the multiscalar typology according to the three relative 
#' deviations (general: G, territorial: T and spatial: S). The elementary units are classified
#' in eight classes according to their three relative positions and they are maped with appropriate colors. 
#' @param x a sf object or a SpatialPolygonsDataFrame including var1 and var2.
#' @param xid identifier field in x (to be used for importing a personal distance matrix). Default to the first column. 
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if NULL, the ratio of reference is the one of 
#' the whole study area (\code{sum(var1) / sum(var2)}).
#' @param key aggregation key field for measuring the deviation (intermediate territorial level).
#' @param order contiguity order.
#' @param dist distance threshold defining the contiguity. The cartesian 
#' distance between units centroids is used by default 
#' ; use mat to apply different metrics. 
#' @param mat a distance matrix (road distance, travel time...) between x units. 
#' Row and column names must fit xid identifiers. (optional)
#' @param threshold defined to build the typology (100 is considered as the average).
#' @param superior if TRUE, deviation values must be greater than threshold. If FALSE, 
#' deviation values must be lower than threshold.  
#' @param colNA no data color
#' @param border color of the polygons border
#' @param lwd borders width
#' @param upborder color of the intermediate territorial level border
#' @param uplwd intermediate territorial level border width
#' @param add whether to add the layer to an existing plot (TRUE) or not (FALSE)
#' @return A typology map with colours and a sf object including the ratio (var1/var2), the 3 relative 
#' deviations (G, T and S) and the resulting ordered typology (0 to 7). 
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
#' library(sf)
#' library(cartography)
#' data("GrandParisMetropole")
#' 
#' # Map wealthiest territories
#' synthesis <- mapmst(x = com, var1 = "INC", var2 = "TH", key = "EPT",
#'                     order = 1,threshold = 125, superior = TRUE)
#' # add a layout and adapted legend
#' layoutLayer(title = "Multiscalar synthesis - Income per household 2013",
#'             sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'             north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
#'             author = "MTA
#' 100: Deviation average
#' G: Situation as compared to the global context (Grand Paris Area)
#' T: Situation as compared to the territorial context (EPT of belonging)
#' S: Sitation as compared to the neigbourhood context (contiguity order 1)")
#' # add label territorial objects above 125% for all the deviations
#' labelLayer(x = synthesis[ which(synthesis$mst == 7),], txt = "LIBCOM", cex = 0.6, 
#'            halo = TRUE, overlap = FALSE)
#' 
#' 
#' 
#' # Map poorest territories 
#' synthesis <- mapmst(x = com, var1 = "INC", var2 = "TH", key = "EPT",
#'                     order = 1,threshold = 75, superior = FALSE)
#' # add a layout
#' layoutLayer(title = "Multiscalar synthesis - Income per household 2013",
#'             sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
#'             north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
#'             author = "MTA
#' 100: Deviation average
#' G: Situation as compared to the global context (Grand Paris Area) 
#' T: Situation as compared to the territorial context (EPT of belonging) 
#' S: Sitation as compared to the neigbourhood context (contiguity order 1)")
#' 
#' # add labels for territorial objects under 75 % for all the deviations
#' labelLayer(x = synthesis[ which(synthesis$mst == 7),], txt = "LIBCOM", cex = 0.6,
#'            halo = TRUE)
#' @importFrom cartography typoLayer legendTypo
#' @export
mapmst <- function(x, var1, var2, ref, key, order, dist, mat, xid, threshold, 
                   superior = FALSE, colNA = "white", border = "grey80", 
                   lwd = 0.2, upborder = "black", uplwd = "1", add = FALSE){
  
  
  # convert to sf object
  if (unlist(class(x)[1]) == "SpatialPolygonsDataFrame"){
    x <- st_as_sf(x)
  }
  
  # Manage id
  if(missing(xid)){xid <- names(x)[1]}
  
  # a dataframe with all the deviations and typology value
  mst <- mst(x = x, var1 = var1, var2 = var2, ref = ref, key = key, dist = dist,
             mat = mat, xid = xid, order = order, threshold = threshold, superior = superior)
  
  # Fix adapted colors for the typology
  colvec <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
              "#addea6","#ffa100","#fff226","#e30020")
  coldf <- data.frame(mst = 0:7, col = colvec)
  x <- merge (x = x, y = mst, by.x = xid, by.y = xid, all.x = TRUE)
  x <- merge(x = x, y = coldf, by.x = "mst", by.y = "mst", all.x = TRUE)
  
  x <- x[order(factor(x$col)), ]
  # x <- x %>%
  #   arrange(mst) %>%   # rearrange the df in the order we want (1,2,3,4,5)
  #   mutate(col = factor(col, unique(col))) # this line reorders the factor in the same order
  cols <- levels(x$col)
  
  # Map mst values
  typoLayer(x = x, var = "mst", border = border,
            col = cols, lwd = lwd, legend.pos = "n", add = add, colNA = colNA)
  
  # Plot intermediate level geometries
  #aggreg <- x %>% group_by_at(key) %>% summarize()
  # x <- com
# x <- com
# key <- "EPT"
  . <- split(x = x,f = x[[key]])
  . <- lapply(., st_union) 
  . <- do.call(c, .) 
  aggreg <- st_cast(.)
  
  
  plot(st_geometry(aggreg), col = NA,  lwd = uplwd, border = upborder, add = TRUE)
  
  # Fill automatically the legend (text)
  deviation <- NULL
  if (superior == TRUE) { deviation <- "above"}
  if (superior == FALSE) { deviation <- "under"}
  
  # Create reading grid
  rVal<-c(" .     .     . ",
          "[X]   .     . ",
          " .   [X]    . ",
          "[X] [X]    . ",
          " .     .   [X]",
          "[X]  .   [X]",
          " .   [X] [X]",
          "[X] [X] [X]")
  
  # Plot legend
  legendTypo(col = colvec, categ = rVal,
             title.txt = paste0("General, territorial and spatial\ndeviations ",
             deviation, " ", threshold," %\n\n           G   T   S"),
             nodata = FALSE)
  return(x)
}