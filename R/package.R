#' @title Multiscalar Territorial Analysis
#' @name MTA
#' @description 
#' Build multiscalar territorial analysis based on various contexts. \cr
#' Main functions : 
#' \itemize{
#' \item{\code{\link{gdev}}: general deviation between regional ratios and a 
#' ratio of reference.}
#' \item{\code{\link{tdev}}: territorial deviation between regional ratios and 
#' ratios of an aggregated level.}
#' \item{\code{\link{sdev}}}: spatial deviation between regional ratios and ratios
#' of neigborhing regions.
#' \item{\code{\link{mst}}}: multiscalar typology based on the three deviations.
#' \item{\code{\link{mas}}}: multiscalar absolute synthesis, total amount of 
#' redistributions based on the three deviations. 
#' }
#' 
#' @references YSEBAERT R. (et al.), 2011, HyperAtlas, un outil scientifique au 
#' service du débat politique - Application à la politique de cohésion de l’Union Européenne, congrès CIST, Collège International des Sciences du Territoire (Paris).
#' 
#' @docType package
NULL


#' @title Grand Paris Metropole Communes SpatialPolygonsDataFrame
#' @name com.spdf
#' @description SpatialPolygonsDataFrame of the Grand Paris Metropole communes.\cr 
#' @format 
#' \describe{
#' \item{DEPCOM}{Commune identifiers}
#' }
#' @source 
#' Institut national de l’information géographique et forestière (IGN), GEOFLA® 
#' 2015 v2.1 Communes France Métropolitaine: \cr
#' \url{http://professionnels.ign.fr/geofla}\cr
#' Atelier parisien d'urbanisme, Grand Paris communal composition (2015-12-17):\cr
#' \url{http://www.apur.org/article/composition-12-territoires-metropole-grand-paris}
#' @docType data
#' @examples
#' data(GrandParisMetropole)
#' sp::plot(com.spdf)
NULL


#' @title Grand Paris Metropole EPTs SpatialPolygonsDataFrame
#' @name ept.spdf
#' @description SpatialPolygonsDataFrame of Grand Paris Metropole EPTs. 
#' EPTs (Etablissements Publics Territoriaux) are groups of communes.\cr
#' @format 
#' \describe{
#' \item{EPT}{EPT identifiers}
#' \item{LIBEPT}{EPT names}
#' }
#' @source
#' Atelier parisien d'urbanisme, Grand Paris communal composition (2015-12-17):\cr
#' \url{http://www.apur.org/article/composition-12-territoires-metropole-grand-paris}
#' @docType data
#' @examples
#' data(GrandParisMetropole)
#' sp::plot(ept.spdf)
NULL

#' @title Grand Paris Metropole Communes Data
#' @name com
#' @description Data on the Grand Paris Metropole communes.
#' @format A data frame with 150 rows and 10 variables:
#' \describe{
#' \item{DEPCOM}{Commune identifiers}
#' \item{LIBCOM}{Commune names}
#' \item{EPT}{EPT identifiers of the commune}
#' \item{LIBEPT}{EPT names of the commune}
#' \item{DEP}{Identifiers of the departement}
#' \item{INC}{Amount of income tax reference (in euros)}
#' \item{TH}{Number of tax households}
#' }
#' @source
#' Direction générale des finances publiques, income tax 2014 (2013 incomes):\cr
#' \url{http://www.impots.gouv.fr/portal/dgi/public/statistiques.impot?espId=-4&pageId=stat_donnees_detaillees&sfid=4503}\cr
#' Atelier parisien d'urbanisme, Grand Paris communal composition (2015-12-17):\cr
#' \url{http://www.apur.org/article/composition-12-territoires-metropole-grand-paris}
#' @docType data
#' @examples
#' data(GrandParisMetropole)
#' head(com)
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
#' data(GrandParisMetropole)
#' cardist[1:10,1:10]
NULL





