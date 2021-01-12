## ----input, warning = FALSE, cache = FALSE------------------------------------
# load packages
library(MTA)
library(cartography)
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
colsEPT <- c("#bc60b7", carto.pal("blue.pal", 8)[c(2, 3, 4, 5)], 
          carto.pal("green.pal", 8)[c(2, 3, 4, 5)], 
          carto.pal("red.pal", 8)[c(3, 4, 5)])

# zoning
typoLayer(x = com,
          var="LIBEPT2", legend.values.order = epts,
          legend.pos = "left", 
          col = colsEPT,
          lwd = 0.2,
          border = "white",
          legend.title.txt = "EPT of belonging")

plot(ept$geometry, col = NA, border = "black", add = TRUE)

# layout 
layoutLayer(title = "Territorial Zoning of the MGP",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

## ----INCDeINC_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# layout
par(mfrow = c(1, 2), mar = c(0, 0, 1.2, 0))

# numerator map
com$INCM <- com$INC / 1000000
plot(com$geometry,  col = "peachpuff",  border = NA)
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
            north = FALSE, scale = FALSE, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")


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
            sources = "", north = TRUE, scale = 5, 
            col = "white", coltitle = "black")

## ----ratio_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
par(mar = c(0, 0, 1.2, 0), mfrow = c(1,1))

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
plot(ept$geometry, border = "black",add = TRUE)

# Label min and max
labelLayer(x = com[which.min(com$ratio),], txt = "LIBCOM", col= "black", cex = 0.6, font = 2,
           halo = TRUE, bg = "white", r = 0.05)
labelLayer(x = com[which.max(com$ratio),], txt = "LIBCOM", col= "black", cex = 0.6, font = 2,
           halo = TRUE, bg = "white", r = 0.05)

# layout 
layoutLayer(title = "Ratio - Income per tax households, 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

## ----gdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# general relative deviation
com$gdevrel <- gdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "rel")

# margins
par(mar = c(0, 0, 1.2, 0))

# Global colors for deviation
cols <- carto.pal(pal1 = "blue.pal", n1 = 3,  pal2 = "wine.pal", n2 = 3)

# Global deviation mapping
choroLayer(x = com,
           var = "gdevrel",
           col = cols,
           breaks = c(min(com$gdevrel, na.rm = TRUE),
                      67, 91, 100, 125, 150,
                      max(com$gdevrel, na.rm = TRUE)),
           border = "#f0f0f0",
           lwd = 0.25,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the global context (100 = Metropole du Grand Paris average)",
                      legend.values.rnd = 0)

# Plot EPT layer
plot(ept$geometry, border = "#1A1A19", lwd = 1, add = TRUE)

# layout 
layoutLayer(title = "Global deviation - Tax income per households",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

## ----lorenz_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
library(ineq)
# Gini index
ind_gini <- Gini(com$INC, com$TH)

#  Concentration of X as regards to concentration of Y
Lc.p <- Lc(com$INC, com$TH)
Lp <- data.frame(cumX = 100 * Lc.p$L, cumY = 100 * Lc.p$p)

# Plot concentrations
par(mar = c(4,4,1.2,4), xaxs = "i", yaxs = "i", pty = "s")
plot(Lp$cumY,
     Lp$cumX,
     type = "l",
     col = "red",
     lwd = 2,
     panel.first = grid(10,10),
     ylab = "Income (cumulative percentage)",  
     cex.axis = 0.8,
     cex.lab = 0.9,
     xlab = "Households (cumulative percentage)",
     ylim = c(0,100),
     xlim = c(0,100))  

lines(c(0,100), c(0,100), lwd = 2)

# Ease plot reading
xy1 <- Lp[which.min(abs(50 - Lp$cumX)),]
xy2 <- Lp[which.min(abs(50 - Lp$cumY)),]

xy <- rbind(xy1, xy2)

points(y = xy[,"cumX"], 
       x = xy[,"cumY"], 
       pch = 21,  
       cex = 1.5,
       bg = "red")

text(y = xy[,"cumX"], 
     x = xy[,"cumY"],
     label = paste(round(xy[,"cumX"],0), round(xy[,"cumY"],0), sep = " , "),  
     pos = 2,
     cex = 0.6)

text(y = 95, 
     x = 2,
     label = paste0("Gini index = ", round(ind_gini,3)),  
     pos = 4,
     cex = 0.8, font = 2)

## ----mdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# Territorial relative deviation calculation
com$mdevrel <- tdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "rel",
                    key = "LIBEPT")

# Cartography
# Plot layout
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Territorial deviation mapping
choroLayer(x = com,
           var = "mdevrel",
           col = cols,
           breaks = c(min(com$mdevrel, na.rm = TRUE), 67, 91, 100, 125, 150,
                      max(com$mdevrel, na.rm = TRUE)),
           border = "#f0f0f0",
           lwd = 0.25,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the territorial context (100 = EPT average)",
           legend.values.rnd = 0)

# Plot EPT layer
plot(ept$geometry, border = "#1A1A19", lwd = 1, add = TRUE)

# Labels to ease comment location
labelLayer(x = com[com$LIBCOM %in% c("Le Raincy", "Rungis", "Sceaux",
                                     "Marnes-la-Coquette") ,],
           txt = "LIBCOM", col= "black", cex = 0.6, font = 2,
           halo = TRUE, bg = "white", r = 0.05)

# layout 
layoutLayer(title = "Territorial deviation - Tax income per households, 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

## ----mdev_boxplot, fig.width=7, fig.height=6, warning = FALSE, cache = FALSE, fig.align = 'center'----
par(mar = c(4, 4, 1.2, 2))

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
        cex.lab = 0.9,
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
com$ldevrel <- sdev(x = com, 
                    xid = "DEPCOM", 
                    var1 = "INC", 
                    var2 = "TH",
                    order = 1,
                    type = "rel")

# Cartography
# Plot layout
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Territorial deviation (relative and absolute) cartography
choroLayer(x = com,
           var = "ldevrel",
           col = cols,
           breaks = c(min(com$ldevrel, na.rm = TRUE), 67, 91, 100, 110, 125,
                                 max(com$ldevrel, na.rm = TRUE)),
           border = "#f0f0f0",
           lwd = 0.25,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the spatial context\n(100 = average of the contiguous territorial units - order 1)",
           legend.values.rnd = 0)

# Plot EPT
plot(ept$geometry, border = "#1A1A19",lwd = 1, add = T)

# Labels to ease comment location
labelLayer(x = com[com$LIBCOM %in% c("Le Raincy", "Vaucresson", "Sceaux",
                                     "Marnes-la-Coquette", "Saint-Maur-des-Fosses",
                                     "Puteaux", "Saint-Ouen", "Bagneux",
                                     "Clichy-sous-Bois", "Clichy") ,],
           txt = "LIBCOM", col= "black", cex = 0.6, font = 2,
           halo = TRUE, bg = "white", r = 0.05, overlap = FALSE)


# layout 
layoutLayer(title = "Spatial deviation - Tax income per households, 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

## ----spat_autocorr_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
par(cex.lab = 1, cex.axis = 0.75, mar = c(4, 4, 1.2, 2))

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
levels(df$col) <- colsEPT

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
text(110,60, pos = 4, cex = 0.7, 
     labels = (paste("Local Deviation =", round(lm$coefficients["gdevrel","Estimate"], digits = 3),
                     "* (Global Deviation) +", round(lm$coefficients["(Intercept)","Estimate"], 
                                                     digits = 3))))
text(110,55, pos = 4, cex = 0.7, 
     labels = (paste("R-Squared =", round(summary(lm(ldevrel~gdevrel, com ))$r.squared, digits = 2))))

abline (h = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (h = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)
abline (v = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)

# Legend for territorial level
legend("topleft",
       legend = levels(df$LIBEPT),
       pch = 20,
       col = colsEPT,
       cex = 0.6,
       pt.cex = 1,
       title = "Territorial context")

## ----spat_autocor_res_plt, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
par(cex.lab = 1, cex.axis = 0.75, mar = c(4, 4, 2, 2))

# Standardized residual calculation
lm <- lm(ldevrel ~ gdevrel, df)
df$res <- rstandard(lm)

#risk alpha (0.1 usually)
alpha <- 0.055

# Calculation of the threshold using T-Student at (n-p-1) degrees of freedom
thr <- qt(1 - alpha / 2, nrow(com) - 1)

# Plot residuals
plot(df$ldevrel, df$res,
     xlab = "Local deviation", cex.lab = 0.8,
     ylim = c(-3.5, 3.5),
     xlim = c(40, 200),
     ylab = "Standardized residuals of spatial autocorrelation", 
     cex.lab = 0.8,
     cex.axis = 0.8,
     pch = 20,
     col = as.vector(df$col))

# Adding thresholds
abline(h = - thr, col = "red")
abline(h = + thr, col = "red")
abline(h = 0, col = "red")

# Detecting exceptional values and labeling them on the plot
ab <- df[df$res < -thr | df$res > thr,]

# Plot residual labels
text(x = ab[,"ldevrel"], y = ab[,"res"], ab[,"LIBCOM"], cex = 0.5, pos = 4)

abline (v = seq(50, 200, 10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50, 200, 50), col = "gray0", lwd = 1, lty = 1)

# Plot the legend (territorial zoning)
legend("topleft",
       legend = levels(ab$LIBEPT),
       pch = 20,
       col = colsEPT,
       cex = 0.6,
       pt.cex = 1,
       title = "Territorial context")

## ----redistributions, fig.width=7, fig.height=5-------------------------------
# general absolute deviation 
com$gdevabs <- gdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "abs")

# Territorial absolute deviation calculation
com$mdevabs <- tdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "abs",
                    key = "LIBEPT")

# Transform the values in million Euros
com$gdevabsmil <- com$gdevabs / 1000000
com$mdevabsmil <- com$mdevabs / 1000000

# Deviation orientation
com$gdevsign <- ifelse(com$gdevabsmil> 0, "Income surplus", "Income deficit")
com$mdevsign <- ifelse(com$mdevabsmil > 0, "Income surplus", "Income deficit")

# Deviation maps 
par(mar = c(0, 0, 1.2, 0), mfrow = c(1,2))

# General deviation
# Plot territories
plot(st_geometry(com), col = "grey70", border = "#EDEDED", lwd = 0.25)
plot(st_geometry(ept), border = "#1A1A19", lwd = 1, add = TRUE)

propSymbolsTypoLayer(x = com, var = "gdevabsmil", var2 = "gdevsign",
                     symbols = "circle",
                     inches = 0.15,
                     col = c("#F6533A","#515FAA"),
                     legend.var.pos = "n",
                     legend.var2.pos = "n",
                     fixmax = max(abs(com$gdevabsmil)))

# Labels to ease comment location
labelLayer(x = com[com$LIBCOM %in% c("Paris 7e Arrondissement",
                                     "Neuilly-sur-Seine", "Aubervilliers") ,],
           txt = "LIBCOM", col= "black", cex = 0.6, font = 2,
           halo = TRUE, bg = "white", r = 0.05, overlap = FALSE)

# Layout map 1
layoutLayer(title = "General deviation (Metrople du Grand Paris)", 
            tabtitle = TRUE, col = "white", 
            coltitle = "black", postitle = "center", scale = FALSE,
            author = "Ronan Ysebaert, RIATE, 2021")

# Territorial deviation
plot(st_geometry(com), col = "grey70", border = "#EDEDED", lwd = 0.25)
plot(st_geometry(ept), border = "#1A1A19", lwd = 1, add = TRUE)

propSymbolsTypoLayer(x = com, var = "mdevabsmil", var2 = "mdevsign",
                     symbols = "circle",
                     inches = 0.15,
                     col = c("#F6533A","#515FAA"),
                     legend.var.pos = "n",
                     legend.var2.pos = "n",
                     fixmax = max(abs(com$gdevabsmil)))

# Labels to ease comment location
labelLayer(x = com[com$LIBCOM %in% c("Marnes-la-Coquette",
                                     "Nanterre", "Clichy-sous-Bois") ,],
           txt = "LIBCOM", col= "black", cex = 0.6, font = 2,
           halo = TRUE, bg = "white", r = 0.05, overlap = FALSE)

# Legend cirles
legendCirclesSymbols(pos = "topleft", inches = 0.15, col = "grey",
                     var = c(500, 2000, max(com$gdevabsmil)),
                     title.txt = "Income redistribution\n(million Euros)")

# Legend typo
legendTypo(pos = "bottomleft", title.txt = "Redistribution nature", col = c("#F6533A","#515FAA"),
           categ = c("Income surplus", "Income deficit"), nodata = FALSE)

# Layout map 2
layoutLayer(title = "Territorial deviation (EPT of belonging)", 
            tabtitle = TRUE, col = "white", 
            coltitle = "black", postitle = "center", scale = 5)

## ----gdev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# general deviation - Top 10 of the potential contributors in regard 
# Drop geometries
df <- st_set_geometry(com, NULL)
row.names(df) <- df$LIBCOM

# to their total amount of income
df$gdevabsPerc <- df$gdevabs / df$INC * 100
df <- df[order(df$gdevabsPerc, decreasing = TRUE), ]
df[1:10, c("gdevabsmil","gdevabsPerc")]

# general deviation - Top 10 of the potential receivers in regard to 
# their total amount of income
df <- df[order(df$gdevabsPerc, decreasing = FALSE), ]
df[1:10, c("gdevabsmil", "gdevabsPerc")]

## ----mdev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# general deviation - Top 10 of the potential contributors in regard 
# Drop geometries
df <- st_set_geometry(com, NULL)
row.names(df) <- df$LIBCOM

# Territorial deviation - Top 10 of the potential contributors as regards to their total amount of income
df$mdevabsPerc <- df$mdevabs / df$INC * 100
df <- df[order(df$mdevabsPerc, decreasing = TRUE), ]
df[1:10, c("mdevabsmil", "mdevabsPerc")]

# Territorial deviation - Top 10 of the potential receivers as regards to their total amount of income
df <- df[order(df$mdevabsPerc, decreasing = FALSE), ]
df[1:10, c("mdevabsmil", "mdevabsPerc")]

## ----bidev_plot, fig.width=7, fig.height=8------------------------------------
par(mar = c(0, 0, 0, 0), mfrow = c(1,1))

# Prerequisite  - Compute 2 deviations
com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")

# EX1 standard breaks with four labels
plot_bidev(x = com, 
           dev1 = "gdev", 
           dev2 = "tdev",
           dev1.lab = "General deviation (MGP Area)",
           dev2.lab = "Territorial deviation (EPT of belonging)",
           lib.var = "LIBCOM",
           lib.val =  c("Marolles-en-Brie", "Suresnes", 
                        "Clichy-sous-Bois", "Les Lilas"))

## ----bidev_map, fig.width=7, fig.height=5-------------------------------------
par(mfrow = c(1,2), mar = c(0,4,0,0))

bidev <- map_bidev(x = com, dev1 = "gdev", dev2 = "tdev",
                   breaks = c(50, 100, 200))

# Unlist resulting function
com <- bidev$geom
cols <- bidev$cols

# Cartography
typoLayer(x = com, var = "bidev", border = "grey50",
          col = cols, lwd = 0.2, legend.pos = "n")

plot(st_geometry(ept), border = "#1A1A19", lwd = 1, add = TRUE)

 # Label territories in the C3 category
labelLayer(x = com[com$bidev == "C3",], txt = "LIBCOM", halo = TRUE)

layoutLayer(title = "2-Deviations synthesis : Situation as regards to general and territorial contexts",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

plot_bidev(x = com,  dev1 = "gdev",  dev2 = "tdev", 
           dev1.lab = "General deviation (MGP Area)",
           dev2.lab = "Territorial deviation (EPT of belonging)",
           breaks = c(50, 100, 200),
           lib.var = "LIBCOM", lib.val = "Clichy-sous-Bois", cex.lab = 0.8)

## ----synth7_1, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
# Prerequisite  - Compute the 3 deviations
com$gdev <- gdev(x = com, var1 = "INC", var2 = "TH")
com$tdev <- tdev(x = com, var1 = "INC", var2 = "TH", key = "EPT")
com$sdev <- sdev(x = com, var1 = "INC", var2 = "TH", order = 1)


# Multiscalar typology - wealthiest territorial units
# Row names = municipality labels
row.names(com) <- com$LIBCOM

# Compute mst
com$mstW <- mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", 
                threshold = 125, superior = TRUE)

subset(com, mstW == 7, select = c(ratio, gdev, tdev, sdev, mstW), drop = T)

## ----map_mst1, fig.width=7, fig.height=5--------------------------------------
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Compute mapmst
mst <- map_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", 
              threshold = 125, superior = TRUE)

# Unlist resulting function
com <- mst$geom
cols <- mst$cols
leg_val <- mst$leg_val


# Cartography
library(cartography)
typoLayer(x = com, var = "mst", border = "grey50",
          col = cols, lwd = 0.2, legend.pos = "n")

plot(st_geometry(ept), border = "#1A1A19", lwd = 1, add = TRUE)

legendTypo(col = cols, categ = leg_val,
           title.txt = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",
           nodata = FALSE, pos = "topleft")

labelLayer(x = com[com$mst == 7,], txt = "LIBCOM",
           cex = 0.6, halo = TRUE, overlap = FALSE)

layoutLayer(title = "3-Deviations synthesis : Territorial units above index 125",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

## ----map_mst2, fig.width=7, fig.height=5--------------------------------------
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Compute mapmst
mst <- map_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", 
              threshold = 80, superior = FALSE)

# Unlist resulting function
com <- mst$geom
cols <- mst$cols
leg_val <- mst$leg_val


# Cartography
library(cartography)
typoLayer(x = com, var = "mst", border = "grey50",
          col = cols, lwd = 0.2, legend.pos = "n")

plot(st_geometry(ept), border = "#1A1A19", lwd = 1, add = TRUE)

legendTypo(col = cols, categ = leg_val,
           title.txt = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",
           nodata = FALSE, pos = "topleft")

labelLayer(x = com[com$mst == 7,], txt = "LIBCOM",
           cex = 0.6, halo = TRUE, overlap = FALSE)

layoutLayer(title = "3-Deviations synthesis : Territorial units under index 80",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            north = TRUE, scale = 5, col = "white", coltitle = "black",
            author = "Ronan Ysebaert, RIATE, 2021")

## ----synthesis80_class7-------------------------------------------------------
# Multiscalar typology - Lagging territorial units
# Row names = municipality labels
row.names(com) <- com$LIBCOM

# Compute mst
com$mstP <- mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", 
                threshold = 80, superior = FALSE)

# municipalities in lagging situation for the three contexts
subset(com, mstP == 7, select = c(ratio, gdev, tdev, sdev, mstP), drop = T)

## ----synthesis80_class3-------------------------------------------------------
# municipalities in lagging situation in the global and territorial contexts
subset(com, mstP == 3, select = c(ratio, gdev, tdev, sdev, mstP), drop = T)
# municipalities in favorable situation in a spatial context or in a spatial and a territorial context 
subset(com, mstP == 4 | mstP == 6, select = c(ratio, gdev, tdev, sdev, mstP), drop = T)

## ----plot_mst, fig.width = 7, fig.height=6------------------------------------
par(mfrow = c(1, 1), mar = c(4, 6, 4, 4))

# Synthesis barplot
plot_mst(x = com, gdevrel = "gdev", tdevrel = "tdev", sdevrel = "sdev", lib.var = "LIBCOM", 
         lib.val = c("Neuilly-sur-Seine", "Clichy-sous-Bois", "Suresnes", "Les Lilas"))

## ----synthesisabs, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, eval = TRUE----
# Local redistribution (not yet calculated)
com$ldevabs <- sdev(x = com, xid = "DEPCOM", var1 = "INC", var2 = "TH",
                    order = 1, type = "abs")
com$ldevabsmil <- com$ldevabs / 1000000

# Compute the synthesis DataFrame (absolute deviations)
mas(x = com, 
    gdevabs = "gdevabsmil", 
    tdevabs = "mdevabsmil",
    sdevabs = "ldevabsmil",
    num = "INCM") 

