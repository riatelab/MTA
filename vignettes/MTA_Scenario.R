## ----input, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE-----
# load packages
library(MTA)
library(cartography)
library(ineq)
library(reshape2)

# load dataset
data("GrandParisMetropole", package = "MTA")
# set row names to communes names
row.names(com) <- com$LIBCOM

## ----plot_zonings, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, fig.align = 'center'----
# set margins
par(mar = c(0,0,1.2,0))

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
cols <- c("purple", carto.pal("blue.pal", 8)[c(1,3,6,8)], 
          carto.pal("green.pal", 8)[c(1,3,6,8)], 
          carto.pal("red.pal", 8)[c(3,5,8)])

# zoning
typoLayer(spdf = com.spdf, df = com,
          var="LIBEPT2", legend.values.order = epts,
          legend.pos = "left", 
          col = cols,
          legend.title.txt = "EPT")

# layout 
layoutLayer(title = "Territorial Zoning of the MGP",
            sources = "GEOFLA® 2015 v2.1, Apur",
            author = "RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            south = FALSE)


## ----INCDeINC_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----
# layout
par(mfrow = c(1,2), mar = c(0,0,1.2,0))

# numerator map
com$INCM <- com$INC / 1000000
plot(com.spdf,  col = "peachpuff",  border = "grey20",  lwd = 0.2)
propSymbolsLayer(spdf = com.spdf, df = com,
                 var = "INCM", 
                 symbols = "circle", col =  "#F6533A",
                 inches = 0.15,
                 border = "#25252570",
                 legend.pos = "topleft", legend.values.rnd = 0,
                 legend.title.txt = "Amount of income taxe reference\n(millions of euros)",
                 legend.style = "c")
# layout
layoutLayer(title = "Numerator - Amount of income tax reference",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            author = "RIATE, 2016",
            scale = NULL,
            frame = FALSE,
            col = "black",
            coltitle = "white")

# denominator map
plot(com.spdf, col = "peachpuff",border = "grey20",lwd=0.2)
propSymbolsLayer(spdf = com.spdf, df = com,
                 var = "TH", 
                 symbols = "circle", col =  "#515FAA",
                 border = "#25252570",
                 inches = 0.15,
                 legend.pos = "topleft", legend.values.rnd = -2,
                 legend.title.txt = "Number of tax households",
                 legend.style = "c")

# layout
layoutLayer(title = "Denominator - Tax households",
            sources = "",
            author = "",
            scale = 5,
            frame = FALSE,
            col = "black",
            coltitle = "white")

## ----ratio_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----
# margins
par(mar = c(0,0,1.2,0))

# ratio
com$ratio <- com$INC / com$TH

# ratio map
choroLayer(spdf = com.spdf,
           df = com,
           var = "ratio",
           breaks = c(min(com$ratio,na.rm=TRUE),20000,
                      30000,40000,50000,60000,
                      max(com$ratio,na.rm=TRUE)),
           col = carto.pal(pal1 = "red.pal", n1 = 6),
           border = "grey20",
           lwd=0.2,
           legend.pos = "topleft",
           legend.title.txt = "Average amount of income tax\nreference per households\n(in euros)",
           legend.values.rnd = 0)

# EPT borders
plot(ept.spdf,border="#f0f0f0",lwd=0.5,add=T)

# layout
layoutLayer(title = "Ratio - Income per tax households, 2013",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            author = "RIATE, 2016",
            scale = NULL,
            frame = FALSE,
            col = "black",
            coltitle = "white")


## ----gdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----
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
par(mar = c(0,0,1.2,0))

# Plot territories
plot(com.spdf, col = "grey70", border = "#EDEDED", lwd = 0.25)
plot(ept.spdf, border = "#1A1A19", lwd = 1, add = TRUE)

# Global deviation (relative and absolute) cartography
propSymbolsChoroLayer(spdf = com.spdf, df = com,legend.var.values.rnd = 4,
                      var = "gdevabsmil", var2 = "gdevrel",
                      add = TRUE,
                      inches = 0.3,
                      col = carto.pal(pal1 = "blue.pal", n1 = 3,
                                      pal2 = "wine.pal", n2 = 3),
                      breaks = c(min(com$gdevrel,na.rm=TRUE),
                                 75,90,100,111,133,
                                 max(com$gdevrel,na.rm=TRUE)),
                      border = "#f0f0f0",
                      lwd = 0.25,
                      legend.var.pos = "left", legend.var2.pos = "topleft",
                      legend.var.title.txt = "Redistribution (Million euros)",
                      legend.var2.title.txt = "Deviation to the global context (100 = Metropole du Grand Paris average)",
                      legend.var.style = "e")

layoutLayer(title = "Global deviation - Tax income per households",
            sources = "GEOFLA® 2015 v2.1, Apur, impots.gouv.fr",
            author = "RIATE, 2016",
            scale = NULL,
            frame = FALSE,
            col = "black",
            coltitle = "white")

## ----gdev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----
# general deviation - Top 10 of the potential contributors in regard 
# to their total amount of income
com$gdevabsPerc <- com$gdevabs / com$INC * 100
com <- com[order(com$gdevabsPerc, decreasing = TRUE), ]
com[1:10, c("gdevabsmil","gdevabsPerc")]

# general deviation - Top 10 of the potential receivers in regard to 
# their total amount of income
com <- com[order(com$gdevabsPerc, decreasing = FALSE), ]
com[1:10,c("gdevabsmil","gdevabsPerc")]

## ----lorenz_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, fig.align = 'center'----

par(cex.lab=1)
par(cex.axis=0.75)
par(mar=c(4,4,2,2))

# Lorenz Curve
Lc <- Lc (com$INC, n=com$TH)
plot(Lc,
     ylab = "proportion of numerator (total tax income)",
     xaxt = 'n',
     xlab = "proportion of denominator (households)",
     yaxt = 'n')

seq<-seq(0,1,0.1)
axis(side = 1, at = seq,labels = T)
axis(side = 2, at = seq, labels = T)

grid(10, 10, lwd = 1)

# Inequality indexes
Gini<-ineq(com$ratio)
Coeff.Var<-var.coeff(com$ratio, square = FALSE, na.rm = TRUE)
Gini
Coeff.Var

## ----mdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----

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
par(mfrow = c(1,1), mar = c(0,0,1.2,0))
layoutLayer(title = "Territorial deviation - Tax income per households, 2013",
            sources = "Data source : DGFiP, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = com.spdf)

# Plot territories
plot(com.spdf, col = "grey70", border="#EDEDED",lwd=0.25,add=T)
plot(ept.spdf, border = "#1A1A19", lwd = 1, add = T)

# Territorial deviation (relative and absolute) cartography
propSymbolsChoroLayer(spdf = com.spdf, df = com,
                      var = "mdevabsmil", var2 = "mdevrel",
                      add = TRUE,
                      inches = 0.3,
                      col = carto.pal(pal1 = "blue.pal", n1 = 3,
                                      pal2 = "wine.pal", n2 = 3),
                      breaks = c(min(com$mdevrel,na.rm=TRUE),
                                 75,90,100,111,133,
                                 max(com$mdevrel,na.rm=TRUE)),
                      border = "#f0f0f0",
                      lwd = 0.25,
                      legend.var.pos = "left", legend.var2.pos = "topleft",
                      legend.var.title.txt = "Redistribution (Million euros)",
                      legend.var2.title.txt = "Deviation to the territorial context (100 = Etablissement public territoriaux average)",
                      legend.var.style = "e")

## ----mdev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----
# Territorial deviation - Top 10 of the potential contributors as regards to their total amount of income
com$mdevabsPerc<- com$mdevabs / com$INC * 100
com<-com[order(com$mdevabsPerc, decreasing = TRUE), ]
com[1:10,c("mdevabsmil","mdevabsPerc")]

# Territorial deviation - Top 10 of the potential receivers as regards to their total amount of income
com<-com[order(com$mdevabsPerc, decreasing = FALSE), ]
com[1:10,c("mdevabsmil","mdevabsPerc")]

## ----mdev_boxplot, fig.width=7, fig.height=6, warning = FALSE, cache = TRUE, fig.align = 'center'----

# Layout parameters
par(cex.lab=1)
par(cex.axis=0.75)
par(mar=c(4,4,2,2))

# Boxplot ordered by mean
com$EPT <- with(com, reorder(EPT, gdevrel, mean))

boxplot(com$gdevrel ~ com$EPT,
        col = carto.pal(pal1 = "wine.pal", n1 = nlevels(com$EPT)),
        ylab = "Global deviation",
        varwidth = TRUE,
        range = 1,
        outline = TRUE,
        las = 1) 

abline (h = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (h = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)

par(new=TRUE) 

boxplot(com$gdevrel ~ com$EPT,
        col = carto.pal(pal1 = "wine.pal", n1 = nlevels(com$EPT)),
        ylab = "Global deviation",
        varwidth = TRUE,
        range = 1,
        outline = TRUE,
        las = 1) 

# Plot mean values
xi<- tapply(com$gdevrel,com$EPT,mean)
points(xi,col="#7C0000",pch=19)


# Legend for the boxplot
com$LIBEPT <- as.factor(com$LIBEPT)

legend("topleft",
       legend = levels(com$LIBEPT),
       pch = 20,
       col = carto.pal(pal1 = "wine.pal", n1 = nlevels(com$EPT)),
       cex = 0.8,
       pt.cex = 1,
       title = "Territorial contexts (ordered by mean value of global deviation)")



## ----localdevrel_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----

par(mar=c(2,4,0,0))

# Spatial relative deviation calculation
com$ldevrel <- sdev(spdf = com.spdf,
                    x = com,
                    spdfid = "DEPCOM",
                    xid = "DEPCOM",
                    var1 = "INC",
                    var2 = "TH",
                    order = 1,
                    type = "rel")


# Spatial absolute deviation calculation
com$ldevabs <- sdev(spdf = com.spdf,
                    x = com,
                    spdfid = "DEPCOM",
                    xid = "DEPCOM",
                    var1 = "INC",
                    var2 = "TH",
                    order = 1,
                    type = "abs")

# Spatial deviation in million Euros
com$ldevabsmil <- com$ldevabs / 1000000

# Cartography
# Plot layout
par(mfrow = c(1,1), mar = c(0,0,1.2,0))
layoutLayer(title = "Spatial deviation - Tax income per households, 2013",
            sources = "Data source : DGFiP, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = com.spdf)

# Plot territories
plot(com.spdf, col = "grey70", border="#EDEDED",lwd=0.25,add=T)
plot(ept.spdf,border="#1A1A19",lwd=1,add=T)

# Territorial deviation (relative and absolute) cartography
propSymbolsChoroLayer(spdf = com.spdf, df = com,
                      var = "ldevabsmil", var2 = "ldevrel",
                      add = TRUE,
                      inches = 0.3,
                      col = carto.pal(pal1 = "blue.pal", n1 = 3,
                                      pal2 = "wine.pal", n2 = 3),
                      breaks = c(min(com$ldevrel,na.rm=TRUE),
                                 75,90,100,111,133,
                                 max(com$ldevrel,na.rm=TRUE)),
                      border = "#f0f0f0",
                      lwd = 0.25,
                      legend.var.pos = "left", legend.var2.pos = "topleft",
                      legend.var.title.txt = "Redistribution (Million euros)",
                      legend.var2.title.txt = "Deviation to the spatial context (100 = average of the contiguous territorial units - order 1)",
                      legend.var.style = "e")

## ----ldev_listing, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----
# Spatial deviation - Top 10 of the potential contributors as regards to their total amount of income
com$ldevabsPerc<- com$ldevabs / com$INC * 100
com<-com[order(com$ldevabsPerc, decreasing = TRUE), ]
com[1:10,c("ldevabsmil","ldevabsPerc")]

# Spatial deviation - Top 10 of the potential receivers as regards to their total amount of income
com<-com[order(com$ldevabsPerc, decreasing = FALSE), ]
com[1:10,c("ldevabsmil","ldevabsPerc")]

## ----spat_autocorr_plot, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, fig.align = 'center'----

par(cex.lab=1)
par(cex.axis=0.75)
par(mar=c(4,4,2,2))

# Dot colours by EPT
colours <- c("#fb6a4a","#74c476","#74a9cf",
             "#045a8d","#006d2c","#c7e9c0",
             "#bcbddc","#fcbba1","#3690c0",
             "#d9f0a3","#9ecae1","#de2d26")

# Assign correctly colours
com<-com[order(com$DEPCOM, decreasing = FALSE), ]
com$col <- as.factor(com$LIBEPT)
com$LIBEPT <- as.factor(com$LIBEPT)
levels(com$col) <- colours

# Spatial autocorrelation
lm <- summary.lm(lm(ldevrel ~ gdevrel, com))

# Plot spatial autocorrelation
plot(com$gdevrel,com$ldevrel,
     ylab = "Local deviation",
     ylim = c(50,260),
     xlab = "Global deviation",
     xlim = c(50,260),
     pch = 20,
     col = as.vector(com$col))
abline((lm(com$ldevrel ~ com$gdevrel)), col = "red", lwd =1)

# Specify linear regression formula and R-Squared of the spatial autocorrelation on the plot
text(140,65, pos = 4, cex = 0.8, 
     labels = (paste("Local Deviation =", round(lm$coefficients["gdevrel","Estimate"], digits = 3),
                     "* (Global Deviation) +", round(lm$coefficients["(Intercept)","Estimate"], 
                                                     digits = 3))))
text(140,60, pos = 4, cex = 0.8, 
     labels = (paste("R-Squared =", round(summary(lm(ldevrel~gdevrel, com ))$r.squared, digits = 2))))

abline (h = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (h = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)
abline (v = seq(40,290,10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50,250,50), col = "gray0", lwd = 1, lty = 1)

# Legend for territorial level
legend("topleft",
       legend = levels(com$LIBEPT),
       pch = 20,
       col = colours,
       cex = 0.5,
       pt.cex = 1,
       title = "Territorial context")

## ----spat_autocor_res_plt, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, fig.align = 'center'----

par(cex.lab=1)
par(cex.axis=0.75)
par(mar=c(4,4,2,2))

# Standardised residual calculation
lm <- lm(ldevrel ~ gdevrel, com)
res.standard <- rstandard(lm)

#risk alpha = 0.1
alpha <- 0.1

# Calculation of the threshold using T-Student at (n-p-1) degrees of freedom
seuil.standard <- qt(1-alpha/2, nrow(com) - 1)

# Plot residuals
plot(com$ldevrel,res.standard,
     xlab = "Local deviation", cex.lab = 0.8,
     xlim = c(60,180),
     ylab = "Standardised residuals of spatial autocorrelation", cex.lab = 0.8,
     pch = 20,
     col = as.vector(com$col),
     cex.axis =0.8)

# Adding thresholds
abline(h=-seuil.standard, col = "red")
abline(h=+seuil.standard, col = "red")
abline(h=0, col = "red")

# Detecting exceptional values and labeling them on the plot
ab.standard <- com[res.standard < -seuil.standard | res.standard > +seuil.standard,]

for (i in 1:nrow(ab.standard)){
  # Take the territorial units listing below and above the threshold
  communes <- row.names(ab.standard)[i]
  # Plot the residual names
  text(com[communes,"ldevrel"],res.standard[communes],communes,cex =0.5, pos=4)
}

abline (v = seq(50,200,10), col = "gray70", lwd = 0.25, lty = 3)
abline (v = seq(50,200,50), col = "gray0", lwd = 1, lty = 1)

# Plot the legend (territorial zoning)
legend("topleft",
       legend = levels(ab.standard$LIBEPT),
       pch = 20,
       col = colours,
       cex = 0.5,
       pt.cex = 1,
       title = "Territorial context")


## ----effect_distance, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, fig.align = 'center'----

par(cex.lab=1)
par(cex.axis=0.75)
par(mar=c(4,4,2,2))

# Set the targeted distances for measuring spatial deviation
dfSpa <- data.frame(dist = seq(0,35000,by=500), ineq = NA) 

# Calculation of spatial deviations in absolute terms for all the distances retained
spat <- function(x){
  ldevabs <- sdev(spdf = com.spdf,
                  x = com,
                  spdfid = "DEPCOM",
                  xid = "DEPCOM",
                  var1 = "INC",
                  var2 = "TH",
                  dist = x[1],
                  type = "abs")
  
  ldevabspos <- ldevabs[ldevabs>0]
  sum(ldevabspos)
}

dfSpa$ineq <- apply(dfSpa, 1, spat)

# Convert results in million Euros and km
dfSpa$ineq <- dfSpa$ineq / 1000000
dfSpa$dist <- dfSpa$dist / 1000
dfSpa[is.na(dfSpa)] <- 0

# Set the targeted contiguity order for measuring spatial deviation
dfOrder <- data.frame(order = seq(0,20,by=1), ineq = NA) 

# Calculation of spatial deviations in absolute terms for all the contiguities retained
spat <- function(x){
  ldevabs <- sdev(spdf = com.spdf,
                  x = com,
                  spdfid = "DEPCOM",
                  xid = "DEPCOM",
                  var1 = "INC",
                  var2 = "TH",
                  order = x[1],
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
plot.window(xlim=c(0,22500),ylim=c(0,35), xaxs="i", yaxs="i")
axis(1, pos=0, xlim=c(0,22500))
axis(2, col = '#488b37', pos=0, ylim=c(0,35))
abline (v = seq(0,22000,1000), col = "gray30", lwd = 0.25)
abline (h = seq(0,35,5), col = "#488b37", lwd = 0.25)
lines(dfSpa$dist ~ dfSpa$ineq,type='b',col='#488b37',lwd=0.25, lty = 1, pch = 20)
title(xlab = "Mass of numerator to redistribute required to reach the equilibrium (million Euros)")
mtext("Neighbourhood: Euclidian distance (km)", col = "#488b37", side = 2,line = 2)

# Adding plot 2 : contiguity
plot.window(xlim=c(0,22500),ylim=c(0,20),xaxs="i", yaxs="i")
axis(4, col ='#7a378b')
abline (h = seq(0,20,5), col = "#7a378b", lwd = 0.25, lty = 1)
lines(dfOrder$order~dfOrder$ineq,type='b',col='#7a378b',lwd=0.25, lty = 1, pch = 20)
mtext("Neighbourhood: Contiguity order", side = 4, col = "#7a378b")

#title for the graph
title(main="Numerator redistribution as regards to spatial and contiguity parameters")
box(which = "plot")
 

## ----synthesishigh, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE----

par(mar=c(2,4,0,0))

# Compute the synthesis DataFrame (relative deviations)
mst <- mst(spdf = com.spdf, x = com, spdfid = "DEPCOM", xid = "DEPCOM",
           var1 = "INC", var2 = "TH", dist = NULL, key = "EPT", order = 1,
           mat = NULL, threshold = 125, superior = TRUE) 

# Plot layout
par(mfrow = c(1,1), mar = c(0,0,1.2,0))       
layoutLayer(title = "Synthesis / 3 contexts - Tax income per households, 2013",
            sources = "Data source : DGFiP, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = com.spdf)

# Colours Typology and legend layout
colours <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
             "#addea6","#ffa100","#fff226","#e30020")


# Plot typology map 
typoLayer(spdf = com.spdf, df = mst, var = "mst",
          add = TRUE, 
          border = "#D9D9D9",
          lwd=0.25,legend.values.order = 0:7, 
          col = colours,
          legend.pos = "n")

# Plot territorial level
plot(ept.spdf,border="#252525",lwd=0.5,add=T)

# Plot the legend
rVal<-c(" .   .   . ","[X]  .   . ",
        " .  [X]  . ","[X] [X]  . ",
        " .   .  [X]","[X]  .  [X]",
        " .  [X] [X]","[X] [X] [X]")

legendTypo(col = colours, categ = rVal,
           title.txt = "Global, Territorial and Spatial deviations above 125 %",
           nodata = FALSE)

## ----synthesis125_class7, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, eval = TRUE----
# Communes in favorable situation for the three contexts
subset(mst, mst == 7, select = c(gdevrel, tdevrel, sdevrel, mst))
# Communes in favorable situation in a global context or in a global and in an territorial contexts
subset(mst, mst == 1 | mst == 3, select = c(gdevrel, tdevrel, sdevrel, mst))
# Communes in favorable situation in a spatial context or in a spatial and a territorial context 
subset(mst,  mst == 4 | mst == 6, select = c(gdevrel, tdevrel, sdevrel, mst))

## ----synthesislow, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, eval = TRUE----

par(mar=c(2,4,0,0))

# Compute the synthesis DataFrame (relative deviations)
mst <- mst(spdf = com.spdf, x = com, spdfid = "DEPCOM", xid = "DEPCOM",
           var1 = "INC", var2 = "TH", dist = NULL, key = "EPT", order = 1,
           mat = NULL, threshold = 80, superior = FALSE) 

# Plot layout
par(mfrow = c(1,1), mar = c(0,0,1.2,0))
layoutLayer(title = "Synthesis / 3 contexts - Tax income per households, 2013",
            sources = "Data source : DGFiP, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = com.spdf)

# Colours Typology and legend layout
colours <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
             "#addea6","#ffa100","#fff226","#e30020")

# Plot typology map 
typoLayer(spdf = com.spdf, df = mst, var = "mst",
          add = TRUE, 
          border = "#D9D9D9",
          lwd=0.25,
          col = colours,
          legend.values.order = 0:7,
          legend.pos = "n")

# Plot territorial level
plot(ept.spdf,border="#252525",lwd=0.5,add=T)

# Plot the legend
rVal<-c(" .   .   . ","[X]  .   . ",
        " .  [X]  . ","[X] [X]  . ",
        " .   .  [X]","[X]  .  [X]",
        " .  [X] [X]","[X] [X] [X]")

legendTypo(col = colours, categ = rVal,
           title.txt = "Global, Territorial and Spatial deviations below 80%",
           nodata = FALSE)


## ----synthesis80_class7, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, eval = TRUE----

# Communes in lagging situation for the three contexts
subset(mst, mst == 7, select = c(gdevrel, tdevrel, sdevrel, mst))

# Communes in lagging situation in a global context or in a global and in an territorial contexts
subset(mst, mst == 1 | mst == 3, select = c(gdevrel, tdevrel, sdevrel, mst))

# Communes in lagging situation in a spatial context or in a spatial and a territorial context 
subset(mst,  mst == 4 | mst == 6, select = c(gdevrel, tdevrel, sdevrel, mst))

## ----synthesisabs, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, eval = TRUE----

# Compute the synthesis DataFrame (absolute deviations)
mas(spdf = com.spdf, x = com, spdfid = "DEPCOM", xid = "DEPCOM",
    var1 = "INC", var2 = "TH", dist = NULL, key = "EPT", order = 1,
    mat = NULL) 

