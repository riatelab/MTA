#' @title Get the 'mta' dataset
#' @name mf_get_mtq
#' @description Import the mta example dataset (municipalities and EPCI of the Metropole du Grand Paris area).
#' @return two sf objects on Metropole du Grand Paris (communes and EPCI)
#' @export
#' @details This a wrapper around
#' @importFrom sf st_read
#' @examples
#' mta_get_data()
mta_get_data <- function() {
  com <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
  ept <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "ept", quiet = TRUE)
}