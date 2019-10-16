## ----input, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# load packages
library(MTA)
library(cartography)
library(sp)
library(ineq)
library(reshape2)
library(sf)

# load dataset
data("GrandParisMetropole", package = "MTA")
# set row names to municipalities names
row.names(com) <- com$LIBCOM

## ----plot_zonings, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
# set margins
par(mar = c(0, 0, 1.2, 0))

# label management
com$LIBEPT2 <- com$LIBEPT
com[com$LIBEPT == "Val de Bievres - Seine Amond - Grand Orly", "LIBEPT2"] <-
  "Val de Bievres -\nSeine Amond - Grand Orly"
com[com$LIBEPT == "Plaine Centrale - Haut Val-de-Marne - Plateau Briard", "LIBEPT2"] <-
  "Plaine Centrale -\nHaut Val-de-Marne - Plateau Briard"
com[com$LIBEPT == "Association des Communes de l'Est Parisien", "LIBEPT2"] <-
  "Association des Communes\nde l'Est Parisien"

# label order
epts <- c("Paris",                                                
          "Est Ensemble",  
          "Grand-Paris Est",   
          "Territoire des aeroports",   
          "Plaine Commune",  
          "Boucle Nord 92",  
          "La Defense", 
          "Grand Paris Sud Ouest",   
          "Sud Hauts-de-Seine",  
          "Val de Bievres -\nSeine Amond - Grand Orly",
          "Plaine Centrale -\nHaut Val-de-Marne - Plateau Briard",
          "Association des Communes\nde l'Est Parisien")

# colors
cols <- c("#bc60b7", carto.pal("blue.pal", 8)[c(2, 3, 4, 5)], 
          carto.pal("green.pal", 8)[c(2, 3, 4, 5)], 
          carto.pal("red.pal", 8)[c(3, 4, 5)])

# zoning
typoLayer(x = com,
          var="LIBEPT2", legend.values.order = epts,
          legend.pos = "left", 
          col = cols,
          lwd = 0.2,
          border = "white",
          legend.title.txt = "EPT of belonging")

plot(st_geometry(ept), col = NA, border = "black", add = TRUE)

# layout 
layoutLayer(title = "Territorial Zoning of the MGP",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE,
            theme = "red.pal", author = "Ronan Ysebaert, RIATE, 2019")


## ----INCDeINC_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# layout
par(mfrow = c(1, 2), mar = c(0, 0, 1.2, 0))

# numerator map
com$INCM <- com$INC / 1000000
plot(st_geometry(com),  col = "peachpuff",  border = NA)
plot(st_geometry(ept),  col = NA,  border = "black", add = TRUE)

propSymbolsLayer(x = com,
                 var = "INCM", 
                 symbols = "circle", col =  "#F6533A",
                 inches = 0.15,
                 border = "white",
                 legend.pos = "topleft", legend.values.rnd = 0,
                 legend.title.txt = "Amount of income taxe reference\n(millions of euros)",
                 legend.style = "c")
# layout
layoutLayer(title = "Numerator - Amount of income tax reference",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = FALSE, scale = FALSE, tabtitle = TRUE, frame = F, theme = "red.pal",
            author = "Ronan Ysebaert, RIATE, 2019")


# denominator map
plot(st_geometry(com),  col = "peachpuff",  border = NA)
plot(st_geometry(ept),  col = NA,  border = "black", add = TRUE)
propSymbolsLayer(x = com,
                 var = "TH", 
                 symbols = "circle", col =  "#515FAA",
                 border = "white",
                 inches = 0.15,
                 legend.pos = "topleft", legend.values.rnd = -2,
                 legend.title.txt = "Number of tax households",
                 legend.style = "c")


# layout
layoutLayer(title = "Denominator - Tax households",
            sources = "",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE,
            theme = "red.pal", author = "")

## ----ratio_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
par(mar = c(0, 0, 1.2, 0))

# ratio
com$ratio <- com$INC / com$TH

# ratio map
choroLayer(x = com,
           var = "ratio",
           breaks = c(min(com$ratio, na.rm = TRUE), 20000, 30000, 40000, 50000, 60000,
                      max(com$ratio, na.rm = TRUE)),
           col = carto.pal(pal1 = "red.pal", n1 = 6),
           border = "white",
           lwd=0.2,
           legend.pos = "topleft",
           legend.title.txt = "Average amount of income tax\nreference per households\n(in euros)",
           legend.values.rnd = 0)

# EPT borders
plot(st_geometry(ept), border = "black",add = TRUE)

# layout 
layoutLayer(title = "Ratio - Income per tax households, 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
            author = "Ronan Ysebaert, RIATE, 2019")

## ----gdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# general relative deviation
com$gdevrel <- gdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "rel")

# general absolute deviation 
com$gdevabs <- gdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "abs")

# general deviation in million Euros
com$gdevabsmil <- com$gdevabs / 1000000

# margins
par(mar = c(0, 0, 1.2, 0))

# Plot territories
plot(st_geometry(com), col = "grey70", border = "#EDEDED", lwd = 0.25)
plot(st_geometry(ept), border = "#1A1A19", lwd = 1, add = TRUE)

# Global deviation (relative and absolute) cartography
propSymbolsChoroLayer(x = com,
                      var = "gdevabsmil", var2 = "gdevrel",
                      add = TRUE,
                      inches = 0.3,
                      col = carto.pal(pal1 = "blue.pal", n1 = 3,
                                      pal2 = "wine.pal", n2 = 3),
                      breaks = c(min(com$gdevrel, na.rm = TRUE),
                                 75, 90, 100, 111, 133,
                                 max(com$gdevrel, na.rm = TRUE)),
                      border = "#f0f0f0",
                      lwd = 0.25,
                      legend.var.pos = "left", legend.var2.pos = "topleft",
                      legend.var.title.txt = "Redistribution (Million euros)",
                      legend.var2.title.txt = "Deviation to the global context (100 = Metropole du Grand Paris average)",
                      legend.var.style = "e",
                      legend.var.values.rnd = 0,
                      legend.var2.values.rnd = 0)

# layout 
layoutLayer(title = "Global deviation - Tax income per households",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
            author = "Ronan Ysebaert, RIATE, 2019")


## ----gdev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# general deviation - Top 10 of the potential contributors in regard 
# Drop geometries
df <- st_set_geometry(com, NULL)

# to their total amount of income
df$gdevabsPerc <- df$gdevabs / df$INC * 100
df <- df[order(df$gdevabsPerc, decreasing = TRUE), ]
df[1:10, c("gdevabsmil","gdevabsPerc")]

# general deviation - Top 10 of the potential receivers in regard to 
# their total amount of income
df <- df[order(df$gdevabsPerc, decreasing = FALSE), ]
df[1:10, c("gdevabsmil", "gdevabsPerc")]

## ----lorenz_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----

# Lorenz Curve
par(mar=c(4, 4, 4, 4))
par(pty = "s")

plot(Lc(com$INC, com$TH),                 
     panel.first = grid(10, 10),
     main = "Lorenz Curve - Income distribution",
     xlab = "Percent of income (cumulative)",
     ylab = "Percent of households",
     ylim = c(0,1),
     xlim = c(0,1),
     asp = 1)

# Inequality indexes (Gini, standard deviation and cofficient of variation)
Gini(com$ratio)
sd(com$ratio, na.rm = TRUE)
sd(com$ratio, na.rm = TRUE)/mean(com$ratio, na.rm = TRUE) * 100

## ----mdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# Territorial relative deviation calculation
com$mdevrel <- tdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "rel",
                    key = "LIBEPT")

# Territorial absolute deviation calculation
com$mdevabs <- tdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "abs",
                    key = "LIBEPT")

# Territorial deviation in million Euros
com$mdevabsmil <- com$mdevabs / 1000000

# Cartography
# Plot layout
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Plot territories
plot(st_geometry((com)), col = "grey70", border = "#EDEDED",lwd = 0.25)
plot(st_geometry((ept)), border = "#1A1A19", lwd = 1, add = T)

# Territorial deviation (relative and absolute) cartography
propSymbolsChoroLayer(x = com,
                      var = "mdevabsmil", var2 = "mdevrel",
                      add = TRUE,
                      inches = 0.3,
                      col = carto.pal(pal1 = "blue.pal", n1 = 3,
                                      pal2 = "wine.pal", n2 = 3),
                      breaks = c(min(com$mdevrel, na.rm = TRUE),
                                 75, 90, 100, 111, 133,
                                 max(com$mdevrel, na.rm = TRUE)),
                      border = "#f0f0f0",
                      lwd = 0.25,
                      legend.var.pos = "left", legend.var2.pos = "topleft",
                      legend.var.title.txt = "Redistribution (Million euros)",
                      legend.var2.title.txt = "Deviation to the territorial context (100 = EPT average)",
                      legend.var.style = "e",
                      legend.var.values.rnd = 0,
                      legend.var2.values.rnd = 0)


# layout 
layoutLayer(title = "Territorial deviation - Tax income per households, 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
            author = "Ronan Ysebaert, RIATE, 2019")


## ----mdev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# general deviation - Top 10 of the potential contributors in regard 
# Drop geometries
df <- st_set_geometry(com, NULL)

# Territorial deviation - Top 10 of the potential contributors as regards to their total amount of income
df$mdevabsPerc <- df$mdevabs / df$INC * 100
df <- df[order(df$mdevabsPerc, decreasing = TRUE), ]
df[1:10, c("mdevabsmil", "mdevabsPerc")]

# Territorial deviation - Top 10 of the potential receivers as regards to their total amount of income
df <- df[order(df$mdevabsPerc, decreasing = FALSE), ]
df[1:10, c("mdevabsmil", "mdevabsPerc")]

## ----mdev_boxplot, fig.width=7, fig.height=6, warning = FALSE, cache = FALSE, fig.align = 'center'----

par(cex.lab = 1)
par(cex.axis = 0.75)
par(mar = c(4, 4, 2, 2))

# Drop geometries
df <- st_set_geometry(com, NULL)

# Reorder EPT according to gdev value
df$EPT <- with(df, reorder(EPT, gdevrel, mean, na.rm = TRUE))

# Colors management
col <- carto.pal(pal1 = "red.pal", n1 = (nlevels(df$EPT) / 2), 
                 pal2 = "green.pal", n2 = (nlevels(df$EPT) / 2),
                 middle = FALSE, transparency = TRUE)

# Boxplot
boxplot(df$gdevrel ~ df$EPT,
        col = col,
        ylab = "Global deviation",
        xlab = "Territorial deviation",
        varwidth = TRUE,
        range = 1,
        outline = TRUE,
        las = 1) 

# Horizontal Ablines
abline (h = seq(40, 300, 10), col = "#00000060", lwd = 0.5, lty = 3)

# Plot mean values
xi<- tapply(df$gdevrel, df$EPT, mean, na.rm = TRUE)
points(xi, col = "#7C0000", pch = 19)

# Legend for the boxplot
df$EPTName<- as.factor(df$LIBEPT)
df$EPTName <- with(df, reorder(EPTName, gdevrel, mean, na.rm = TRUE))
legend("topleft",
       legend = levels(df$EPTName),
       pch = 15,
       col = col,
       cex = 0.6,
       pt.cex = 1,
       title = "Territorial contexts (ordered by mean value of global deviation)")


## ----localdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----

# Spatial relative deviation calculation
com$ldevrel <- sdev(x = com, xid = "DEPCOM", var1 = "INC", var2 = "TH",
                    order = 1, type = "rel")


# Spatial absolute deviation calculation
com$ldevabs <- sdev(x = com, xid = "DEPCOM", var1 = "INC", var2 = "TH",
                    order = 1, type = "abs")

# Spatial deviation in million Euros
com$ldevabsmil <- com$ldevabs / 1000000

# Cartography
# Plot layout
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Plot territories
plot(st_geometry(com), col = "grey70", border = "#EDEDED",lwd = 0.25)
plot(st_geometry(ept), border = "#1A1A19",lwd = 1, add = T)

# Territorial deviation (relative and absolute) cartography
propSymbolsChoroLayer(x = com,
                      var = "ldevabsmil", var2 = "ldevrel",
                      add = TRUE,
                      inches = 0.3,
                      col = carto.pal(pal1 = "blue.pal", n1 = 3,
                                      pal2 = "wine.pal", n2 = 3),
                      breaks = c(min(com$ldevrel, na.rm = TRUE),
                                 75, 90, 100, 111, 133,
                                 max(com$ldevrel, na.rm = TRUE)),
                      border = "#f0f0f0",
                      lwd = 0.25,
                      legend.var.pos = "left", legend.var2.pos = "topleft",
                      legend.var.title.txt = "Redistribution (Million euros)",
                      legend.var2.title.txt = "Deviation to the spatial context 
                      (100 = average of the contiguous territorial units - order 1)",
                      legend.var.style = "e",
                      legend.var.values.rnd = 0,
                      legend.var2.values.rnd = 0)

# layout 
layoutLayer(title = "Spatial deviation - Tax income per households, 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
            author = "Ronan Ysebaert, RIATE, 2019")


## ----ldev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# Drop geometries
df <- st_set_geometry(com, NULL)

# Spatial deviation - Top 10 of the potential contributors as regards to their total amount of income
df$ldevabsPerc<- df$ldevabs / df$INC * 100
df<-df[order(df$ldevabsPerc, decreasing = TRUE), ]
df[1:10, c("ldevabsmil","ldevabsPerc")]

# Spatial deviation - Top 10 of the potential receivers as regards to their total amount of income
df<-df[order(df$ldevabsPerc, decreasing = FALSE), ]
df[1:10, c("ldevabsmil","ldevabsPerc")]


## ----spat_autocorr_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----

par(cex.lab = 1)
par(cex.axis = 0.75)
par(mar = c(4, 4, 2, 2))

# Drop geometries
df <- st_set_geometry(com, NULL)

# label order
df$LIBEPT <- as.factor(df$LIBEPT)
df$LIBEPT <- ordered(df$LIBEPT, 
                     levels = c("Paris",                                                
                                "Est Ensemble",  
                                "Grand-Paris Est",   
                                "Territoire des Aéroports",   
                                "Plaine Commune",  
                                "Boucle Nord 92",  
                                "La Defense", 
                                "Grand Paris Sud Ouest",   
                                "Sud Hauts-de-Seine",  
                                "Val de Bievres - Seine Amond - Grand Orly",
                                "Plaine Centrale - Haut Val-de-Marne - Plateau Briard",
                                "Association des municipalities de l'Est Parisien"))

# colors
df$col <- as.factor(df$LIBEPT)
levels(df$col) <- cols

# Spatial autocorrelation
lm <- summary.lm(lm(ldevrel ~ gdevrel, df))

# Plot spatial autocorrelation
plot(df$gdevrel, df$ldevrel,
     ylab = "Local deviation",
     ylim = c(50,260),
     xlab = "Global deviation",
     xlim = c(50,260),
     pch = 20,
     col = as.vector(df$col),
     asp = 1)
abline((lm(df$ldevrel ~ df$gdevrel)), col = "red", lwd =1)

# Specify linear regression formula and R-Squared of the spatial autocorrelation on the plot
text(140,65, pos = 4, cex = 0.8, 
     labels = (paste("Local Deviation =", round(lm$coefficients["gdevrel","Estimate"], digits = 3),
                     "* (Global Deviation) +", round(lm$coefficients["(Intercept)","Estimate"], 
                                                     digits = 3))))
text(140,55, pos = 4, cex = 0.8, 
     labels = (paste("R-Squared =", round(summary(lm(ldevrel~gdevrel, com ))$r.squared, digits = 2))))

abline (h = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (h = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)
abline (v = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)

# Legend for territorial level
legend("topleft",
       legend = levels(df$LIBEPT),
       pch = 20,
       col = cols,
       cex = 0.6,
       pt.cex = 1,
       title = "Territorial context")


## ----spat_autocor_res_plt, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----

par(cex.lab = 1)
par(cex.axis = 0.75)
par(mar = c(4, 4, 2, 2))

# Standardized residual calculation
lm <- lm(ldevrel ~ gdevrel, df)
res.standard <- rstandard(lm)

#risk alpha = 0.1
alpha <- 0.1

# Calculation of the threshold using T-Student at (n-p-1) degrees of freedom
seuil.standard <- qt(1 - alpha / 2, nrow(com) - 1)

# Plot residuals
plot(df$ldevrel, res.standard,
     xlab = "Local deviation", cex.lab = 0.8,
     xlim = c(60, 180),
     ylab = "Standardized residuals of spatial autocorrelation", cex.lab = 0.8,
     pch = 20,
     col = as.vector(df$col),
     cex.axis = 0.8)

# Adding thresholds
abline(h = -seuil.standard, col = "red")
abline(h = +seuil.standard, col = "red")
abline(h = 0, col = "red")

# Detecting exceptional values and labeling them on the plot
ab.standard <- df[res.standard < -seuil.standard | res.standard > +seuil.standard,]

for (i in 1 : nrow(ab.standard)){
  # Take the territorial units listing below and above the threshold
  municipalities <- row.names(ab.standard)[i]
  # Plot the residual names
  text(com[municipalities,"ldevrel"], res.standard[municipalities], municipalities, cex = 0.5, pos = 4)
}

abline (v = seq(50, 200, 10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50, 200, 50), col = "gray0", lwd = 1, lty = 1)

# Plot the legend (territorial zoning)
legend("topleft",
       legend = levels(ab.standard$LIBEPT),
       pch = 20,
       col = cols,
       cex = 0.6,
       pt.cex = 1,
       title = "Territorial context")

## ----effect_distance, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----

par(cex.lab = 1)
par(cex.axis = 0.75)
par(mar = c(4, 4, 2, 2))

# Set the targeted distances for measuring spatial deviation
dfSpa <- data.frame(dist = seq(0, 35000, by = 500), ineq = NA) 

# Calculation of spatial deviations in absolute terms for all the distances retained
spat <- function(x){
  ldevabs <- sdev(x = com,  xid = "DEPCOM",  var1 = "INC", var2 = "TH",
                  dist = x[1], type = "abs")
  ldevabspos <- ldevabs[ldevabs>0]
  sum(ldevabspos)
}

dfSpa$ineq <- apply(dfSpa, 1, spat)

# Convert results in million Euros and km
dfSpa$ineq <- dfSpa$ineq / 1000000
dfSpa$dist <- dfSpa$dist / 1000
dfSpa[is.na(dfSpa)] <- 0

# Set the targeted contiguity order for measuring spatial deviation
dfOrder <- data.frame(order = seq(0, 20, by = 1), ineq = NA) 

# Calculation of spatial deviations in absolute terms for all the contiguities retained
spat <- function(x){
  ldevabs <- sdev(x = com, xid = "DEPCOM", var1 = "INC", var2 = "TH", order = x[1],
                  type = "abs")
  
  ldevabspos <- ldevabs[ldevabs>0]
  sum(ldevabspos)
}

dfOrder$ineq <- apply(dfOrder, 1, spat)

# Convert results in million Euros
dfOrder$ineq <- dfOrder$ineq / 1000000
dfOrder[is.na(dfOrder)] <- 0

# Combined plot - Plot 1: Euclidian Distance
plot.new()
plot.window(xlim = c(0,22500), ylim = c(0,35), xaxs = "i", yaxs = "i")
axis(1, pos = 0, xlim = c(0,22500))
axis(2, col = '#488b37', pos = 0, ylim = c(0,35))
abline (v = seq(0, 22000, 1000), col = "gray30", lwd = 0.25)
abline (h = seq(0, 35, 5), col = "#488b37", lwd = 0.25)
lines(dfSpa$dist ~ dfSpa$ineq, type = 'b', col = '#488b37', lwd = 0.25, lty = 1, pch = 20)
title(xlab = "Mass of numerator to redistribute required to reach the equilibrium (million Euros)")
mtext("neighborhood: Euclidian distance (km)", col = "#488b37", side = 2, line = 2)

# Adding plot 2 : contiguity
plot.window(xlim = c(0, 22500), ylim = c(0,20), xaxs = "i", yaxs = "i")
axis(4, col ='#7a378b')
abline (h = seq(0, 20, 5), col = "#7a378b", lwd = 0.25, lty = 1)
lines(dfOrder$order ~ dfOrder$ineq, type = 'b', col = '#7a378b', lwd = 0.25, lty = 1, pch = 20)
mtext("neighborhood: Contiguity order", side = 4, col = "#7a378b")

#title for the graph
title(main = "Numerator redistribution as regards to spatial and contiguity parameters")
box(which = "plot")
 

## ----synth7_1, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
# Compute the synthesis DataFrame (relative deviations)
synthesis <- mst(x = com, var1 = "INC", var2 = "TH", key = "EPT", order = 1, 
                 threshold = 125, superior = TRUE) 
subset(synthesis, mst == 7, select = c(ratio, gdevrel, tdevrel, sdevrel, mst))
           

## ----synthesishigh, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

mapsynthesis <-mapmst(x = com, var1 = "INC", var2 = "TH", key = "EPT",
                      order = 1,threshold = 125, superior = TRUE)

# add a layout
layoutLayer(title = "Multiscalar synthesis - Income per household 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
            author = "Ronan Ysebaert, RIATE, 2019
100: Deviation average
G: Situation as compared to the global context (Grand Paris Area) 
T: Situation as compared to the territorial context (EPT of belonging) 
S: Sitation as compared to the neigbourhood context (contiguity order 1)")

# add labels for territorial objects above 125 % for all the deviations
labelLayer(x = mapsynthesis[ which(mapsynthesis$mst == 7),], txt = "LIBCOM", 
           cex = 0.5, halo = TRUE, overlap = FALSE)

## ----synthesis125_class7, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, eval = TRUE----
# municipalities in favorable situation for the three contexts
subset(synthesis, mst == 7, select = c(gdevrel, tdevrel, sdevrel, mst))
# municipalities in favorable situation in a global context or in a global and in an territorial contexts
subset(synthesis, mst == 1 | mst == 3, select = c(gdevrel, tdevrel, sdevrel, mst))
# municipalities in favorable situation in a spatial context or in a spatial and a territorial context 
subset(synthesis,  mst == 4 | mst == 6, select = c(gdevrel, tdevrel, sdevrel, mst))

## ----synthesislow, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, eval = TRUE----

par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

mapsynthesis <- mapmst(x = com, var1 = "INC", var2 = "TH", key = "EPT",
                       order = 1,threshold = 80, superior = FALSE)

# add a layout
layoutLayer(title = "Multiscalar synthesis - Income per household 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, tabtitle = TRUE, frame = FALSE, theme = "red.pal",
            author = "Ronan Ysebaert, RIATE, 2019
100: Deviation average
G: Situation as compared to the global context (Grand Paris Area) 
T: Situation as compared to the territorial context (EPT of belonging) 
S: Sitation as compared to the neigbourhood context (contiguity order 1)")

# add labels for territorial objects above 125 % for all the deviations
labelLayer(x = mapsynthesis[ which(mapsynthesis$mst == 7),], txt = "LIBCOM", 
           cex = 0.6, halo = TRUE, overlap = FALSE)


## ----synthesis80_class7, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, eval = TRUE----
synthesis <- mst(x = com, var1 = "INC", var2 = "TH", key = "EPT", order = 1, threshold = 125, superior = TRUE) 
# municipalities in favorable situation for the three contexts
subset(synthesis, mst == 7, select = c(gdevrel, tdevrel, sdevrel, mst))
# municipalities in favorable situation in a global context or in a global and in an territorial contexts
subset(synthesis, mst == 1 | mst == 3, select = c(gdevrel, tdevrel, sdevrel, mst))
# municipalities in favorable situation in a spatial context or in a spatial and a territorial context 
subset(synthesis,  mst == 4 | mst == 6, select = c(gdevrel, tdevrel, sdevrel, mst))


## ----synthesisabs, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, eval = TRUE----

# Compute the synthesis DataFrame (absolute deviations)
mas(x = com, xid = "DEPCOM", var1 = "INC", var2 = "TH", key = "EPT", order = 1) 

