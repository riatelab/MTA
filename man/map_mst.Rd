% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_mst.R
\name{map_mst}
\alias{map_mst}
\title{Map Multiscalar Typology (3 deviations)}
\usage{
map_mst(x, gdevrel, tdevrel, sdevrel, threshold, superior = FALSE, xid = NULL)
}
\arguments{
\item{x}{a sf object or a dataframe including 3 pre-calculated deviations.}

\item{gdevrel}{name of the general relative deviation variable in x.}

\item{tdevrel}{name of the territorial relative deviation variable in x.}

\item{sdevrel}{name of the the spatial relative deviation variable in x.}

\item{threshold}{defined to build the typology (100 is considered as the 
average)}

\item{superior}{if TRUE, deviation values must be greater than threshold. If 
FALSE, deviation values must be lower than threshold.}

\item{xid}{identifier field in x. Default the first column.}
}
\value{
A list including a ordered sf object for mapping mst column (geom), a 
vector of suggested colors (cols) and a vector of adatped labels for the 
legend (leg_labels). 
\itemize{Typology labels (which deviation is above/under the threshold):
\item{0: none (mst value = 0)}
\item{G: general only (mst value = 1) }
\item{T: territorial only (mst value = 2)}
\item{G-T: general and Territorial (mst value = 3)}
\item{S: spatial only (mst value = 4)}
\item{G-S: general and Spatial (mst value = 5)}
\item{T-S: territorial and Spatial (mst value = 6)}
\item{G-T-S: all deviations (mst value = 7)}
}
}
\description{
Compute the multiscalar typology (3 deviations) and propose 
labels and colours for mapping the results.
}
\examples{
# Load data
library(sf)
com <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
ept <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "ept", quiet = TRUE)

# Prerequisites - Compute the 3 deviations
com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")
com$sdev <- sdev(x = com, var1 = "INC", var2 = "TH", order = 1)

#Example 1 - Wealthiest territorial units
# Compute map_mst
mst <- map_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev",
threshold = 125, superior = TRUE)

# Unlist outputs of the function
com <- mst$geom
cols <- mst$cols
leg_val <- mst$leg_val

if(require(mapsf)){
# Cartography
mf_map(x = com, var = "mst", type = "typo", border = "white", lwd = 0.2,
       pal = cols, val_order = unique(com$mst), leg_pos = NA)
mf_map(ept, col = NA, border = "black", lwd = 1, add = TRUE)

mf_legend(type = "typo", val = leg_val, pal = cols, 
          title = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",
          pos = "topleft")

mf_label(x = com[com$mst == 7,], var = "LIBCOM",
         cex = 0.6, halo = TRUE, overlap = FALSE)

mf_layout(title = "3-Deviations synthesis : Territorial units above index 125",
          credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nMTA", packageVersion("MTA")),
          arrow = FALSE)
}


# Example 2 - Lagging territorial units
# Compute map_mst
mst <- map_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev",
               threshold = 75, superior = FALSE)

# Unlist resulting function
com <- mst$geom
cols <- mst$cols
leg_val <- mst$leg_val

if(require(mapsf)){
# Cartography
mf_map(x = com, var = "mst", type = "typo", border = "white", lwd = 0.2,
       pal = cols, val_order = unique(com$mst), leg_pos = NA)
mf_map(ept, col = NA, border = "black", lwd = 1, add = TRUE)

mf_legend(type = "typo", val = leg_val, pal = cols, 
          title = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",
          pos = "topleft")

mf_label(x = com[com$mst == 7,], var = "LIBCOM",
         cex = 0.6, halo = TRUE, overlap = FALSE)

mf_layout(title = "3-Deviations synthesis : Territorial units below index 75",
          credits = paste0("Sources: GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nMTA", packageVersion("MTA")),
          arrow = FALSE)

}
}
