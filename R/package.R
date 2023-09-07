#' @title Multiscalar Territorial Analysis
#' @name MTA
#' @description 
#' Build multiscalar territorial analysis based on various contexts for a given ratio defined by a numerator and a denominator.\cr
#' Main functions : 
#' \itemize{
#' \item{\code{\link{gdev}}: general deviation of each territorial unit as regards  
#' to all the study area (or a reference value).}
#' \item{\code{\link{tdev}}: territorial deviation of each territorial unit as regards  
#' to an intermediate territorial level of reference.}
#' \item{\code{\link{sdev}}}: spatial deviation of each territorial unit as regards  
#' to its geographical neighborhood.
#' #' \item{\code{\link{bidev}}}: multiscalar typology based on 2 deviations.
#' \item{\code{\link{mst}}}: multiscalar typology based on the 3 deviations.
#' \item{\code{\link{mas}}}: multiscalar absolute synthesis, total amount of 
#' redistributions based on the three deviations. 
#' \item{\code{\link{map_bidev}}}: creating bidev and parameters for producing 
#' a map based on it. 
#' \item{\code{\link{map_mst}}}: creating mst and parameters for producing 
#' a map based on it.
#' \item{\code{\link{plot_bidev}}}: creating a plot for visualizing bidev results. 
#' \item{\code{\link{plot_mst}}}: creating a plot adapted for visualzing mst results.
#' }
#' 
#' @references GRASLAND C., YSEBAERT R., ZANIN C., LAMBERT N., Spatial disparities in Europe  (Chapter 4)  
#' in GLOERSEN E., DUBOIS A. (coord.), 2007, Regional disparities and cohesion: What Strategies for the future?, DG-IPOL – European Parliament.
#' 
#' @docType package
"_PACKAGE"



#' @title Grand Paris Metropole EPTs
#' @name ept
#' @description sf object. Grand Paris Metropole EPTs. 
#' EPTs (Etablissements Publics Territoriaux) are groups of communes.\cr
#' @format 
#' \describe{
#' \item{EPT}{EPT identifiers}
#' \item{LIBEPT}{EPT names}
#' }
#' @source
#' Atelier parisien d'urbanisme, Grand Paris communal composition (2015-12-17):\cr
#' \url{https://www.apur.org/fr/nos-travaux/composition-12-territoires-metropole-grand-paris}
#' @docType data
#' @examples
#' library(sf)
#' ept <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "ept", quiet = TRUE)
NULL

#' @title Grand Paris Metropole Communes Data
#' @name com
#' @description Data on the Grand Paris Metropole communes, included in a sf object. 
#' @format A data frame with 150 rows and 10 variables:
#' \describe{
#' \item{DEPCOM}{Commune identifiers}
#' \item{LIBCOM}{Commune names}
#' \item{EPT}{EPT identifiers of the commune}
#' \item{LIBEPT}{EPT names of the commune}
#' \item{DEP}{Identifiers of the departement}
#' \item{INC}{Amount of income tax reference (in euros)}
#' \item{TH}{Number of tax households}
#' \item{geometry}{Commune geometry}
#' }
#' @source
#' Direction générale des finances publiques, income tax 2014 (2013 incomes):\cr
#' \url{https://www.impots.gouv.fr/statistiques}\cr
#' Atelier parisien d'urbanisme, Grand Paris communal composition (2015-12-17):\cr
#' \url{https://www.apur.org/fr/nos-travaux/composition-12-territoires-metropole-grand-paris}
#' @docType data
#' @examples
#' library(sf)
#' com <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
NULL


#' @title Time Distance Matrix Between Communes
#' @name cardist
#' @description Travel time between Grand Paris Metropole communes' centroids by car, in minutes.\cr
#' Row names and column names match the DEPCOM field in \link{com}.
#' @source
#' The matrix is computed using the osrm package (\url{https://cran.r-project.org/package=osrm}). \cr
#' Data (c) OpenStreetMap contributors, ODbL 1.0. http://www.openstreetmap.org/copyright \cr
#' Routes: OSRM. http://project-osrm.org/ \cr
#' 
#' @docType data
#' @examples
#' cardist <- read.table(system.file("cardist.txt", package = "MTA"), check.names = FALSE)
#' cardist <- as.matrix(cardist)
#' cardist[1:10, 1:10]
NULL