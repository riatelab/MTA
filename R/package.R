#' @title Multiscalar Territorial Analysis
#' @name MTA
#' @description 
#' Build multiscalar territorial analysis based on various contexts. \cr
#' Main functions : 
#' \itemize{
#' \item{\code{\link{globalDev}}: deviation between a regional ratio and a ratio 
#' of reference.}
#' \item{\code{\link{mediumDev}}: deviation between a regional ratio and ratios 
#' of an aggregated level.}
#' \item{\code{\link{localDev}}}: deviation between regional ratios and local 
#' ratios.
#' }
#' 
#' @references RONNNNNAAAAAANNNNN, A Great Paper Made With Love,
#' \emph{and in English}, 2023
#' 
#' @docType package
NULL


#' @title Grand Paris Metropole Communes SpatialPolygonsDataFrame
#' @name com.spdf
#' @description SpatialPolygonsDataFrame of the Grand Paris Metropole communes.\cr 
#' Fields: \cr
#' \itemize{
#' \item{DEPCOM: Commune identifiers}
#' }
#' @references
#' \url{http://professionnels.ign.fr/geofla#tab-3} for communes.
#' \url{http://www.apur.org/article/composition-12-territoires-metropole-grand-paris} 
#' for Grand Paris communal composition.
#' @docType data
#' @examples
#' data(GrandParisMetropole)
#' sp::plot(com.spdf)
NULL


#' @title Grand Paris Metropole EPTs SpatialPolygonsDataFrame
#' @name ept.spdf
#' @description SpatialPolygonsDataFrame Grand Paris Metropole EPTs. 
#' EPTs (etablissements publics territoriaux) are groups of communes.\cr
#' Fields: \cr
#' \itemize{
#' \item{EPT: EPT identifiers}
#' \item{LIBEPT: EPT names}
#' }
#' @references
#' \url{http://professionnels.ign.fr/geofla#tab-3} for communes.
#' \url{http://www.apur.org/article/composition-12-territoires-metropole-grand-paris} 
#' for Grand Paris communal composition.
#' @docType data
#' @examples
#' data(GrandParisMetropole)
#' sp::plot(ept.spdf)
NULL

#' @title Grand Paris Metropole Communes Data
#' @name com
#' @description Data.frame of Grand Paris Metropole communes.\cr
#' Fields: \cr
#' \itemize{
#' \item{DEPCOM: Commune identifiers}
#' \item{LIBCOM: Commune names}
#' \item{EPT: EPT identifiers of the commune}
#' \item{LIBEPT: EPT names of the commune}
#' \item{DEP: identifiers of the departement}
#' \item{INC: Amount of income tax reference (in euros)}
#' \item{TH: Number of tax households}
#' }
#' @references
#' \url{http://professionnels.ign.fr/geofla#tab-3} for communes.
#' \url{http://www.apur.org/article/composition-12-territoires-metropole-grand-paris} 
#' for Grand Paris communal composition.
#' \url{http://www.impots.gouv.fr/portal/dgi/public/statistiques.impot?espId=-4&pageId=stat_donnees_detaillees&sfid=4503} 
#' for tax data.
#' @docType data
#' @examples
#' data(GrandParisMetropole)
#' head(com)
NULL


