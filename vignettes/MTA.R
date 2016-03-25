## ----localdevrel_plot, fig.width=7, fig.height=7, warning = FALSE, cache = TRUE----
# Deviation in relative term (100 = average of the neigbouring territorial units under the threshold of 5000 meters)
COM.df$ldevrel <- localDev(spdf = COM.spdf, x = COM.df, spdfid = "id", xid = "DEPCOM",
                            var1 = "num", var2 = "denom", dist = 5000,
                            type = "rel")

# Cartography - Relative deviation
par(mfrow = c(1,1), mar = c(0,0,1.2,0))
layoutLayer(title = "Local deviation (5 km) - Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

choroLayer(spdf = COM.spdf, df = COM.df, var = "ldevrel",
           add = TRUE,
           border = NA,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the spatial context (Communes located at less than 5 km)",
           legend.nodata = "Unpopulated area",
           breaks = c(min(COM.df$ldevrel,na.rm=TRUE),75,90,100,110,125,max(COM.df$ldevrel,na.rm=TRUE)), 
           col = carto.pal(pal1 = "blue.pal", n1 = 3, 
                           pal2 = "wine.pal", n2 = 3))
plot(COM.spdf,border="#000000",lwd=0.25,add=T)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)

## ----spat_autocorr_plot, fig.width=7, fig.height=7, warning = FALSE, cache = TRUE----

# Layout parameters
par(mfrow = c(1,1), mar = c(4,4,4,2))

colours <- c("#fb6a4a","#74c476","#74a9cf","#045a8d","#006d2c","#c7e9c0","#bcbddc","#fcbba1","#3690c0","#d9f0a3","#9ecae1","#de2d26")

# Assign correctly colours
COM.df$col <- as.factor(COM.df$LIB_EPT)
levels(COM.df$col) <- colours

# Spatial autocorrelation
reg<-lm(ldevrel ~ gdevrel, COM.df)
summary.lm(reg)

plot(COM.df$gdevrel,COM.df$ldevrel,
     ylab = "Local deviation",
     xlab = "Global deviation",
     pch = 20,
     col = as.vector(COM.df$col))
abline(reg, col = "red", lwd =1)
text(140,80, pos = 4, cex = 0.8, labels = "Local Deviation = 0.46 (Global Deviation) + 55.4")
text(140,75, pos = 4, cex = 0.8, labels ="R² = 0.56")

legend("topleft",
       legend = levels(COM.df$LIB_EPT),
       pch = 20,
       col = colours,
       cex = 0.5,
       pt.cex = 1,
       title = "Territorial context")

## ----spat_autocorr_residuals_plot, fig.width=7, fig.height=7, warning = FALSE, cache = TRUE----

# Standardised residual calculation
res.standard <- rstandard(reg)

#risk alpha = 0.1
alpha <- 0.1

# Calculation of the threshold using T-Student at (n-p-1) degrees of freedom
seuil.standard <- qt(1-alpha/2, nrow(COM.df) - 1)

# Plot residuals
plot(COM.df$ldevrel,res.standard,
     xlab = "Local deviation",
     ylab = "Standardised residuals of spatial autocorrelation",
     pch = 20,
     col = as.vector(COM.df$col))

# Adding thresholds
abline(h=-seuil.standard)
abline(h=+seuil.standard)
abline(h=0)

# Detecting exceptional values and labeling them on the plot
ab.standard <- COM.df[res.standard < -seuil.standard | res.standard > +seuil.standard,]
row.names(ab.standard) <- ab.standard$LIBCOM

for (i in 1:nrow(ab.standard)){
communes <- row.names(ab.standard)[i]
text(COM.df[communes,"ldevrel"],res.standard[communes],communes, cex = 0.5, pos = 4)
}

# Plot the legend (territorial zoning)
legend("topleft",
       legend = levels(ab.standard$LIB_EPT),
       pch = 20,
       col = colours,
       cex = 0.5,
       pt.cex = 1,
       title = "Territorial context")

## ----ldevabs_plot, fig.width=7, fig.height=7, warning = FALSE, cache = TRUE----

# Deviation in absolute term (MTA function)
COM.df$ldevabs <- localDev(spdf = COM.spdf, x = COM.df, spdfid = "id", xid = "DEPCOM",
                            var1 = "num", var2 = "denom", dist = 5000,
                            type = "abs")

# Cartography - Absolute deviation
par(mfrow = c(1,1), mar = c(0,0,1.2,0))

layoutLayer(title = "Spatial redistribution (5 km) -  Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

plot(COM.spdf, col = "peachpuff",border = NA, add=TRUE)

propSymbolsLayer(spdf = COM.spdf, df = COM.df, var = "ldevabs", 
                 legend.pos = "topleft",
                 legend.title.txt = "Redistribution (Euros)",
                 col = "#ff0000",col2 = "#0000ff",
                 border = "#25252570",
                 lwd = 0.1,
                 inches = 0.2, legend.style = "e",
                 breakval = 0)

plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)

# Who should have to contribute to ensure equi-repartition, and how much ? 
COM.df<-COM.df[order(COM.df$ldevabs, decreasing = TRUE), ]
COM.df$ldevabsPerc<- COM.df$ldevabs / COM.df$num * 100
COM.df[1:10,c("LIBCOM","LIB_EPT","ldevabs","num","ldevabsPerc")]

# Who should receive to ensure global equi-repartition, and how much ? 
COM.df<-COM.df[order(COM.df$ldevabs, decreasing = FALSE), ]
COM.df[1:10,c("LIBCOM","LIB_EPT","ldevabs","num","ldevabsPerc")]

