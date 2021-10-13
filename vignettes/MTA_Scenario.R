## ----input, warning = FALSE, cache = FALSE------------------------------------
# load packages
library(MTA)
library(mapsf)
library(ineq)
library(sf)

# load dataset
com <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "com", quiet = TRUE)
ept <- st_read(system.file("metroparis.gpkg", package = "MTA"), layer = "ept", quiet = TRUE)

# set row names to municipalities names
row.names(com) <- com$LIBCOM

## ----plot_zonings, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
# set margins
par(mar = c(0, 0, 1.2, 0))

# label / colors management
LIBEPT <- c("Paris", "Est Ensemble", "Grand-Paris Est", "Territoire des aeroports",   
          "Plaine Commune", "Boucle Nord 92", "La Defense","Grand Paris Sud Ouest",   
          "Sud Hauts-de-Seine", "Val de Bievres - Seine Amond - Grand Orly",
          "Plaine Centrale - Haut Val-de-Marne - Plateau Briard",
          "Association des Communes de l'Est Parisien")

# colors
cols <- c("#cfcfcf", # Grey(Paris)
          "#92C8E0", "#7BB6D3", "#64A4C5", "#458DB3", # Blues (dept 93)
          "#A6CC99", "#8CBB80", "#71A966", "#4E9345", # Greens (dept 92)
          "#F38F84", "#EF6860", "#EA3531") # Reds (dept 94)

colEpt <- data.frame(LIBEPT, cols)

# zoning
mf_map(x = com, var="LIBEPT", type = "typo",
       val_order = LIBEPT, pal = cols, lwd = 0.2, border = "white",
       leg_pos = "left", leg_title = "EPT of belonging")

mf_map(ept, col = NA, border = "black", add = TRUE)

# layout 
mf_layout(title = "Territorial Zoning of the MGP",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"))

## ----INCDeINC_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# layout
par(mfrow = c(1, 2), mar = c(0, 0, 1.2, 0))

# numerator map
com$INCM <- com$INC / 1000000
mf_map(com,  col = "peachpuff",  border = NA)
mf_map(ept,  col = NA,  border = "black", add = TRUE)

mf_map(x = com, var = "INCM", type = "prop",
       col =  "#F6533A", inches = 0.15, border = "white",
       leg_pos = "topleft", leg_val_rnd =  0,
       leg_title = "Amount of income taxe reference\n(millions of euros)")

# layout
mf_layout(title = "Numerator - Amount of income tax reference",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)


# denominator map
mf_map(com,  col = "peachpuff",  border = NA)
mf_map(ept,  col = NA,  border = "black", add = TRUE)

mf_map(x = com, var = "TH", type = "prop",
       col =  "#515FAA", inches = 0.15, border = "white",
       leg_pos = "topleft", leg_val_rnd =  -2,
       leg_title = "Number of tax households", add = TRUE)

# layout
mf_layout(title = "Denominator - Tax households",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

## ----ratio_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
par(mar = c(0, 0, 1.2, 0), mfrow = c(1,1))

# ratio
com$ratio <- com$INC / com$TH

# ratio map
mf_map(x = com, var = "ratio", type = "choro",
       breaks = c(min(com$ratio, na.rm = TRUE), 20000, 30000, 40000, 50000, 60000,
                      max(com$ratio, na.rm = TRUE)),
       pal = c("#FCDACA", "#F6A599", "#F07168", "#E92B28", "#C70003", "#7C000C"),
       border = "white", lwd = 0.2, leg_pos = "topleft", leg_val_rnd = 0,
       leg_title = "Average amount of income tax\nreference per households\n(in euros)") 

# EPT borders
mf_map(ept, col = NA, border = "black", add = TRUE)

# Label min and max
mf_label(x = com[which.min(com$ratio),], var = "LIBCOM", cex = 0.6, font = 2,
         halo = TRUE, r = 0.05)
mf_label(x = com[which.max(com$ratio),], var = "LIBCOM", cex = 0.6, font = 2,
         halo = TRUE, r = 0.05)

# layout 
mf_layout(title = "Ratio - Income per tax households, 2013",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

## ----gdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# general relative deviation
com$gdevrel <- gdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "rel")

# margins
par(mar = c(0, 0, 1.2, 0))

# Colors for deviations
devpal <-  c("#4575B4", "#91BFDB", "#E0F3F8", "#FEE090", "#FC8D59", "#D73027")

# Global deviation mapping
mf_map(x = com, var = "gdevrel", type = "choro",
       breaks = c(min(com$gdevrel, na.rm = TRUE), 67, 91, 100, 125, 150,
                  max(com$gdevrel, na.rm = TRUE)),
       pal = devpal, border = "white", lwd = 0.2, 
       leg_pos = "topleft", leg_val_rnd = 0,
       leg_title = paste0("Deviation to the global context",
                          "\n(100 = Metropole du Grand Paris average)")) 

# Plot EPT layer
mf_map(ept, col = NA, border = "#1A1A19", lwd = 1, add = TRUE)

# layout 
mf_layout(title = "Global deviation - Tax income per households",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

## ----lorenz_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
library(ineq)
#  Concentration of X as regards to concentration of Y
Lc.p <- Lc(com$INC, com$TH)
Lp <- data.frame(cumX = 100 * Lc.p$L, cumY = 100 * Lc.p$p)

# Plot concentrations
par(mar = c(4,4,1.2,4), xaxs = "i", yaxs = "i", pty = "s")
plot(Lp$cumY, Lp$cumX,  type = "l", col = "red", lwd = 2,
     panel.first = grid(10,10), xlab = "Households (cumulative percentage)",
     ylab = "Income (cumulative percentage)", 
     cex.axis = 0.8, cex.lab = 0.9, ylim = c(0,100), xlim = c(0,100))  

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

## ----mdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# Territorial relative deviation calculation
com$tdevrel <- tdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "rel",
                    key = "LIBEPT")

# Cartography
# Plot layout
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Territorial deviation mapping
mf_map(x = com, var = "tdevrel", type = "choro",
       breaks = c(min(com$tdevrel, na.rm = TRUE), 67, 91, 100, 125, 150,
                  max(com$tdevrel, na.rm = TRUE)),
       pal = devpal, border = "white", lwd = 0.2, 
       leg_pos = "topleft", leg_val_rnd = 0,
       leg_title = paste0("Deviation to the territorial context",
                          "\n(100 = EPT average)")) 

# Plot EPT layer
mf_map(ept, col = NA, border = "#1A1A19", lwd = 1, add = TRUE)

# Labels to ease comment location
mf_label(x = com[com$LIBCOM %in% c("Le Raincy", "Rungis", "Sceaux",
                                   "Marnes-la-Coquette") ,],
         var = "LIBCOM", cex = 0.6, font = 2, halo = TRUE, r = 0.05)

# layout 
mf_layout(title = "Territorial deviation - Tax income per households, 2013",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

## ----mdev_boxplot, fig.width=7, fig.height=6, warning = FALSE, cache = FALSE, fig.align = 'center'----
par(mar = c(4, 4, 1.2, 2))

# Drop geometries
df <- st_set_geometry(com, NULL)

# Reorder EPT according to gdev value
df$EPT <- with(df, reorder(EPT, gdevrel, mean, na.rm = TRUE))

# Colors management
col <- aggregate(x = df[,"gdevrel"], by = list(LIBEPT = df$LIBEPT),
                 FUN = mean)
col <- merge(col, colEpt, by = "LIBEPT")
col <- col[order(col$x),]
cols <- as.vector(col$cols)

# Drop inexisting levels
df <- droplevels(df)

# Boxplot
bp <- boxplot(df$gdevrel ~ df$EPT, col = cols, ylab = "Global deviation",
              xlab = "Territorial deviation", cex.lab = 0.9,
              varwidth = TRUE, range = 1.5,  outline = TRUE,  las = 1) 

# Horizontal Ablines
abline (h = seq(40, 300, 10), col = "#00000060", lwd = 0.5, lty = 3)

# Plot mean values
xi<- tapply(df$gdevrel, df$EPT, mean, na.rm = TRUE)
points(xi, col = "#7C0000", pch = 19)

# Legend for the boxplot
legend("topleft", legend = rev(as.vector(col$LIBEPT)), pch = 15,
       col = rev(as.vector(col$cols)), cex = 0.8, pt.cex = 1.5)

## ----localdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE----
# Spatial relative deviation calculation
com$sdevrel <- sdev(x = com, 
                    xid = "DEPCOM", 
                    var1 = "INC", 
                    var2 = "TH",
                    order = 1,
                    type = "rel")

# Cartography
# Plot layout
par(mfrow = c(1, 1), mar = c(0, 0, 1.2, 0))

# Territorial deviation (relative and absolute) cartography
mf_map(x = com, var = "sdevrel", type = "choro",
       breaks = c(min(com$sdevrel, na.rm = TRUE), 67, 91, 100, 125, 150,
                  max(com$sdevrel, na.rm = TRUE)),
       pal = devpal, border = "white", lwd = 0.2, 
       leg_pos = "topleft", leg_val_rnd = 0,
       leg_title = paste0("Deviation to the spatial context",
                          "\n(100 = average of the contiguous territorial units - order 1)")) 

# Plot EPT
mf_map(ept, col = NA, border = "#1A1A19",lwd = 1, add = T)

# Labels to ease comment location
mf_label(x = com[com$LIBCOM %in% c("Le Raincy", "Vaucresson", "Sceaux", "Bagneux",
                                   "Marnes-la-Coquette", "Saint-Maur-des-Fosses",
                                   "Puteaux", "Saint-Ouen", "Clichy-sous-Bois", 
                                   "Clichy"),],
         var = "LIBCOM", cex = 0.6, font = 2, halo = TRUE, , r = 0.05,
         overlap = FALSE)

# layout 
mf_layout(title = "Spatial deviation - Tax income per households, 2013",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

## ----spat_autocorr_plot, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
par(cex.lab = 1, cex.axis = 0.75, mar = c(4, 4, 1.2, 2))

# Drop geometries
df <- st_set_geometry(com, NULL)

# Spatial autocorrelation
lm <- summary.lm(lm(sdevrel ~ gdevrel, df))

# Equation 
eq <- paste("Spatial Deviation =", round(lm$coefficients["gdevrel","Estimate"], digits = 3),
            "* (Global Deviation) +", round(lm$coefficients["(Intercept)","Estimate"], 
                                            digits = 3))
rsq <-paste("R-Squared =", round(summary(lm(sdevrel ~ gdevrel, com ))$r.squared, digits = 2))

# Plot spatial autocorrelation
plot(df$gdevrel, df$sdevrel,
     ylab = "Local deviation",
     ylim = c(50,260),
     xlab = "Global deviation",
     xlim = c(50,260),
     pch = 20,
     col = as.vector(df$col),
     asp = 1)
abline((lm(df$sdevrel ~ df$gdevrel)), col = "red", lwd =1)

# Specify linear regression formula and R-Squared of the spatial autocorrelation on the plot
text(110,60, pos = 4, cex = 0.7, labels = eq)
text(110,55, pos = 4, cex = 0.7, labels = rsq)

abline (h = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (h = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)
abline (v = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)

# Legend for territorial level
legend("topleft", legend = rev(as.vector(colEpt$LIBEPT)), pch = 15,
       col = rev(as.vector(colEpt$cols)), cex = 0.6, pt.cex = 1.5)

## ----spat_autocor_res_plt, fig.width=7, fig.height=5, warning = FALSE, cache = FALSE, fig.align = 'center'----
par(cex.lab = 1, cex.axis = 0.75, mar = c(4, 4, 2, 2))

# Standardized residual calculation
lm <- lm(sdevrel ~ gdevrel, df)
df$res <- rstandard(lm)

#risk alpha (0.1 usually)
alpha <- 0.055

# Calculation of the threshold using T-Student at (n-p-1) degrees of freedom
thr <- qt(1 - alpha / 2, nrow(com) - 1)

# Plot residuals
plot(df$sdevrel, df$res,
     xlab = "Local deviation", cex.lab = 0.8,
     ylim = c(-3.5, 3.5),
     xlim = c(40, 200),
     ylab = "Standardized residuals of spatial autocorrelation", 
     cex.lab = 0.8,
     cex.axis = 0.8,
     pch = 20,
     col = as.vector(df$cols))

# Adding thresholds
abline(h = - thr, col = "red")
abline(h = + thr, col = "red")
abline(h = 0, col = "red")

# Detecting exceptional values and labeling them on the plot
ab <- df[df$res < -thr | df$res > thr,]

# Plot residual labels
text(x = ab[,"sdevrel"], y = ab[,"res"], ab[,"LIBCOM"], cex = 0.5, pos = 4)

abline (v = seq(50, 200, 10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50, 200, 50), col = "gray0", lwd = 1, lty = 1)

# Plot the legend (territorial zoning)
legend("topleft", legend = rev(as.vector(colEpt$LIBEPT)), pch = 15,
       col = rev(as.vector(colEpt$cols)), cex = 0.6, pt.cex = 1.5)

## ----redistributions, fig.width=7, fig.height=5-------------------------------
# general absolute deviation 
com$gdevabs <- gdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "abs")

# Territorial absolute deviation calculation
com$tdevabs <- tdev(x = com, 
                    var1 = "INC", 
                    var2 = "TH", 
                    type = "abs",
                    key = "LIBEPT")

# Transform the values in million Euros
com$gdevabsmil <- com$gdevabs / 1000000
com$tdevabsmil <- com$tdevabs / 1000000

# Deviation orientation
com$gdevsign <- ifelse(com$gdevabsmil> 0, "Income surplus", "Income deficit")
com$tdevsign <- ifelse(com$tdevabsmil > 0, "Income surplus", "Income deficit")

# Deviation maps 
par(mar = c(0, 0, 1.2, 0), mfrow = c(1,2))

# General deviation
# Plot territories
mf_map(com, col = "peachpuff", border = "white", lwd = 0.25)
mf_map(ept, col = NA, border = "#1A1A19", lwd = 1, add = TRUE)

mf_map(x = com, var = c("gdevabsmil", "gdevsign"), type = "prop_typo",
       leg_title = c("Absolute Deviation\n(Income redistribution, euros)",
                    "Redistribution direction"),
       leg_pos = c("topleft", "n"), leg_val_rnd = -2,
       val_order = c("Income deficit", "Income surplus"),
       pal =  c("#ff0000","#0000ff"), add = TRUE)
       
       
# Labels to ease comment location
mf_label(x = com[com$LIBCOM %in% c("Paris 7e Arrondissement",
                                   "Neuilly-sur-Seine", "Aubervilliers") ,],
         var = "LIBCOM", cex = 0.6, font = 2,
         halo = TRUE, r = 0.05, overlap = FALSE)

# Layout map 1
mf_layout(title = "General deviation (Metrople du Grand Paris)",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

# Territorial deviation
mf_map(com, col = "peachpuff", border = "white", lwd = 0.25)
mf_map(ept, col = NA, border = "#1A1A19", lwd = 1, add = TRUE)

mf_map(x = com, var = c("tdevabsmil", "tdevsign"), type = "prop_typo",
       leg_title = c("Absolute Deviation\n(Income redistribution, euros)",
                    "Redistribution direction"),
       leg_pos = c("n", "topleft"), leg_val_rnd = -2,
       val_order = c("Income deficit", "Income surplus"),
       val_max = max(abs(com$gdevabsmil)),
       pal =  c("#ff0000","#0000ff"), add = TRUE)
       

# Labels to ease comment location
mf_label(x = com[com$LIBCOM %in% c("Marnes-la-Coquette",
                                   "Nanterre", "Clichy-sous-Bois") ,],
         var = "LIBCOM", cex = 0.6, font = 2,
         halo = TRUE, r = 0.05, overlap = FALSE)

# Layout map 2
mf_layout(title = "Territorial deviation (EPT of belonging)",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

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
df$tdevabsPerc <- df$tdevabs / df$INC * 100
df <- df[order(df$tdevabsPerc, decreasing = TRUE), ]
df[1:10, c("tdevabsmil", "tdevabsPerc")]

# Territorial deviation - Top 10 of the potential receivers as regards to their total amount of income
df <- df[order(df$tdevabsPerc, decreasing = FALSE), ]
df[1:10, c("tdevabsmil", "tdevabsPerc")]

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
mf_map(x = com, var = "bidev", type = "typo", 
       pal = cols, lwd = 0.2, border = "white", leg_pos = "n",
       val_order = unique(com$bidev))

mf_map(ept, col = NA, border = "#1A1A19", lwd = 1, add = TRUE)

# Label territories in the C3 category
mf_label(com[com$bidev == "C3",],
         var = "LIBCOM", halo = TRUE)

mf_layout(title = "2-Deviations synthesis: General and territorial contexts",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

#Associated plot
plot_bidev(x = com,  dev1 = "gdev",  dev2 = "tdev", 
           dev1.lab = "General deviation (MGP Area)",
           dev2.lab = "Territorial deviation (EPT of belonging)",
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
mf_map(x = com, var = "mst", type = "typo", 
       border = "white", lwd = 0.2,
       pal = cols, val_order = unique(com$mst), leg_pos = "n")
mf_map(ept, col = NA, border = "black", lwd = 1, add = TRUE)
mf_legend(type = "typo", pos = "topleft", val = leg_val, pal = cols, 
          title = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",)
mf_layout(title = "3-Deviations synthesis: Territorial units above index 125",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

# Add labels for mst = 7
mf_label(x = com[com$mst == 7,], var = "LIBCOM",  halo = TRUE, overlap = FALSE, 
         cex = 0.7)

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
mf_map(x = com, var = "mst", type = "typo", 
       border = "white", lwd = 0.2,
       pal = cols, val_order = unique(com$mst), leg_pos = "n")
mf_map(ept, col = NA, border = "black", lwd = 1, add = TRUE)
mf_legend(type = "typo", pos = "topleft", val = leg_val, pal = cols, 
          title = "Situation on General (G)\nTerrorial (T) and\nSpatial (S) contexts",)
mf_layout(title = "3-Deviations synthesis: Territorial units under index 80",
          credits = paste0("Sources : GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
                           "\nRonan Ysebaert, RIATE, 2021"), 
          arrow = FALSE)

# Add labels for mst = 7
mf_label(x = com[com$mst == 7,], var = "LIBCOM",  halo = TRUE, overlap = FALSE, 
         cex = 0.7)

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
com$sdevabs <- sdev(x = com, xid = "DEPCOM", var1 = "INC", var2 = "TH",
                    order = 1, type = "abs")
com$sdevabsmil <- com$sdevabs / 1000000

# Compute the synthesis DataFrame (absolute deviations)
mas(x = com, 
    gdevabs = "gdevabsmil", 
    tdevabs = "tdevabsmil",
    sdevabs = "sdevabsmil",
    num = "INCM") 

