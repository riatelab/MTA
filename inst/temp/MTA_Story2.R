library(cartography)
library(rgeos)
library(MTA)
library(RColorBrewer)

# 0. Data preparation
# Working folder
setwd("C:/Users/Ronan/Desktop/MTA_dataset")
# Data upload
COM.df <- read.csv( "MTA_COM_data.csv",header=TRUE,sep=";",dec=".",encoding="utf-8",)
COM.spdf <- rgdal::readOGR(dsn="MTA_communes.shp", layer = "MTA_communes")
EPT.spdf <- rgdal::readOGR(dsn="MTA_EPT.shp", layer = "MTA_EPT")


# 1. Numerator and denominator mapping
# Layout Plot - Numerator
par(mfrow = c(1,2), mar = c(0,0,1.2,0))
layoutLayer(title = "Numerator - Total income mass, 2012",
            sources = "Data source : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = NULL,
            frame = FALSE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            extent = COM.spdf)

# Plot numerator
plot(COM.spdf, col = "peachpuff",border = "grey20",lwd=0.2, add=T)
propSymbolsLayer(spdf = COM.spdf, df = COM.df,
                 var = "num", 
                 symbols = "circle", col =  "#F6533A",
                 fixmax = max(COM.df$num),
                 inches = 0.2,
                 border = "#25252570",
                 legend.pos = "topleft",
                 legend.title.txt = "Median income * Consommation Unit (Euros), 2012",
                 legend.style = "c")

# Layout Plot - Denominator
layoutLayer(title = "Denominator - Consommation Units, 2012",
            sources = NULL,
            author = NULL,
            scale = 5,
            frame = FALSE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            extent = COM.spdf)

# Plot denominator
plot(COM.spdf, col = "peachpuff",border = "grey20",lwd=0.2, add=TRUE)
# Plot symbols
propSymbolsLayer(spdf = COM.spdf, df = COM.df,
                 var = "denom", 
                 symbols = "circle", col =  "#515FAA",
                 fixmax = max(COM.df$denom),
                 border = "#25252570",
                 inches = 0.2,
                 legend.pos = "topleft",
                 legend.title.txt = "Adult = 1 UC; population under 14 = 0,3 UC;
                 people aged between 14 and 18 = 0,5 UC",
                 legend.style = "c")


# 2. Ratio Mapping
par(mfrow = c(1,1), mar = c(0,0,1.2,0))
layoutLayer(title = "Numerator / Denominator - Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

# Plot carto
COM.df$ratio <- COM.df$num / COM.df$denom
choroLayer(spdf = COM.spdf,
           df = COM.df,
           var = "ratio",
           breaks = c(min(COM.df$ratio,na.rm=TRUE),15000,20000,25000,30000,35000,40000,max(COM.df$ratio,na.rm=TRUE)), 
           col = carto.pal(pal1 = "red.pal", n1 = 7),
           border = "grey20",
           lwd=0.2,
           add = TRUE,
           legend.pos = "topleft",
           legend.nodata = "Unpopulated area",
           legend.title.txt = "Income per consommation unit (Euros)",
           legend.values.rnd = 1)

plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)


# 3.1 Global deviation - Relative deviation
# Deviation in relative term (100 = average of the study area)
COM.df$gdevrel <- globalDev(x = COM.df, 
                             var1 = "num", 
                             var2 = "denom", 
                             type = "rel")

# Cartography - Relative deviation
layoutLayer(title = "Global deviation - Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

choroLayer(spdf = COM.spdf, df = COM.df, var = "gdevrel",
           add = TRUE,
           border = NA,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the global context (Métropole du Grand Paris)",
           legend.nodata = "Unpopulated area",
           breaks = c(min(COM.df$gdevrel,na.rm=TRUE),75,90,100,110,125,max(COM.df$gdevrel,na.rm=TRUE)), 
           col = carto.pal(pal1 = "blue.pal", n1 = 3, 
                           pal2 = "wine.pal", n2 = 3))
plot(COM.spdf,border="#000000",lwd=0.25,add=T)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)


# 3.2 Global plot and indexes
library(ineq)

# Lorenz Curve
par(mfrow = c(1,1), mar = c(4,4,4,2))
Lc <- Lc (COM.df$num, n=COM.df$denom)
plot(Lc,
     ylab = "proportion of numerator (Median income * Consommation Units)",
     xaxt = 'n',
     xlab = "proportion of denominator (Consommation Units)",
     yaxt = 'n')

seq<-seq(0,1,0.1)
axis(side = 1, at = seq,labels = T)
axis(side = 2, at = seq, labels = T)

grid(10, 10, lwd = 1)

# Inequality indexes
Gini<-ineq(COM.df$ratio)
Coeff.Var<-var.coeff(COM.df$ratio, square = FALSE, na.rm = TRUE)
Gini
Coeff.Var


# 3.3 Global deviation, redistribution
# Deviation in absolute term
COM.df$gdevabs <- globalDev(x = COM.df, 
                             var1 = "num", 
                             var2 = "denom", 
                             type = "abs")

# Cartography - Absolute deviation
layoutLayer(title = "Global deviation - Median income per consommation unit, 2012",
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
propSymbolsLayer(spdf = COM.spdf, df = COM.df, var = "gdevabs", 
                 legend.pos = "topleft",
                 legend.title.txt = "Redistribution (Euros)",
                 col = "#ff0000",col2 = "#0000ff",
                 border = "#25252570",
                 lwd = 0.1,
                 inches = 0.2, legend.style = "e",
                 breakval = 0)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)

# Who should have to contribute to ensure equi-repartition, and how much ? 
COM.df<-COM.df[order(COM.df$gdevabs, decreasing = TRUE), ]
head(COM.df, n = 10)

# Who should receive to ensure global equi-repartition, and how much ? 
COM.df<-COM.df[order(COM.df$gdevabs, decreasing = FALSE), ]
head(COM.df, n = 10)


# 3.4 Another way to display global inequalities, comparison to Paris average 
# Deviation in relative term (100 = 26910 euros per Consommation Unit
COM.df$Parisdevrel <- globalDev(x = COM.df, 
                               var1 = "num", 
                               var2 = "denom", 
                               type = "rel",
                               ref = 26910)

# Cartography - Relative deviation
layoutLayer(title = "Global deviation to Paris average - Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

choroLayer(spdf = COM.spdf, df = COM.df, var = "Parisdevrel",
           add = TRUE,
           border = NA,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to Paris (26910 euros per consummation unit) ",
           legend.nodata = "Unpopulated area",
           breaks = c(min(COM.df$Parisdevrel,na.rm=TRUE),75,90,100,110,125,max(COM.df$Parisdevrel,na.rm=TRUE)), 
           col = carto.pal(pal1 = "blue.pal", n1 = 3, 
                           pal2 = "wine.pal", n2 = 3))
plot(COM.spdf,border="#000000",lwd=0.25,add=T)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)


# 4.1 Territorial deviation
# Deviation in relative term (100 = average of the territorial level)
COM.df$mdevrel <- mediumDev(x = COM.df, var1 = "num", var2 = "denom", 
                             type = "rel", key = "LIB_EPT")

# Cartography - Relative deviation
layoutLayer(title = "Territorial deviation - Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

choroLayer(spdf = COM.spdf, df = COM.df, var = "mdevrel",
           add = TRUE,
           border = NA,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the territorial context (Établissements Publics Territoriaux)",
           legend.nodata = "Unpopulated area",
           breaks = c(min(COM.df$mdevrel,na.rm=TRUE),75,90,100,110,125,max(COM.df$mdevrel,na.rm=TRUE)), 
           col = carto.pal(pal1 = "blue.pal", n1 = 3, 
                           pal2 = "wine.pal", n2 = 3))
plot(COM.spdf,border="#000000",lwd=0.25,add=T)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)

# 4.2 Territorial plot
n<-print(nlevels(COM.df$EPT)) 
col<-brewer.pal(n, "Paired")

par(cex.lab=1)
par(cex.axis=0.75)
par(mar=c(12,5,1,1))

boxplot(COM.df$mdevrel ~ COM.df$LIB_EPT,
        col = col,
        ylab = "Territorial deviation",
        varwidth = TRUE,
        outline = FALSE,
        las = 2) 


# 4.3 Territorial redistribution
# Deviation in absolute term
COM.df$mdevabs <- mediumDev(x = COM.df, var1 = "num", var2 = "denom", 
                             type = "abs", key = "EPT")

# Cartography - Absolute deviation
par(mfrow = c(1,1), mar = c(0,0,1.2,0))
layoutLayer(title = "Territorial redistribution - Median income per consommation unit, 2012",
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
propSymbolsLayer(spdf = COM.spdf, df = COM.df, var = "mdevabs", 
                 legend.pos = "topleft",
                 legend.title.txt = "Redistribution (Euros)",
                 col = "#ff0000",col2 = "#0000ff",
                 border = "#25252570",
                 lwd = 0.1,
                 inches = 0.2, legend.style = "e",
                 breakval = 0)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)

# Who should have to contribute to ensure equi-repartition, and how much ? 
COM.df<-COM.df[order(COM.df$mdevabs, decreasing = TRUE), ]
head(COM.df, n = 10)

# Who should receive to ensure global equi-repartition, and how much ? 
COM.df<-COM.df[order(COM.df$mdevabs, decreasing = FALSE), ]
head(COM.df, n = 10)


# 5.1 Spatial deviation
# Deviation in relative term (100 = average of the neigbouring territorial units under the threshold of 5000 meters)
COM.df$ldevrel <- localDev(spdf = COM.spdf, x = COM.df, spdfid = "id", xid = "DEPCOM",
                            var1 = "num", var2 = "denom", dist = NULL, order = 1, 
                            type = "rel")
head(COM.df)
# Cartography - Relative deviation
layoutLayer(title = "Spatial deviation (5 km) - Median income per consommation unit, 2012",
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


# 5.2 Spatial plot
par(mfrow = c(1,1), mar = c(4,4,4,2))
# Spatial autocorrelation
spat.aut<-lm(COM.df$ldevrel ~ COM.df$gdevrel)
summary.lm(spat.aut)
summary(spat.aut)$coefficients

plot(COM.df$gdevrel,COM.df$ldevrel,
     ylab = "Spatial deviation",
     xlab = "Global deviation",
     pch = 20,
     col = col)
abline(spat.aut, col = "red", lwd =2)

legend("topleft",
       legend = levels(COM.df$LIB_EPT),
       pch = 20,
       col = col,
       cex = 0.5,
       pt.cex = 1,
       title = "Territorial context")


# 5.3 Spatial redistribution 
#Deviation in absolute term
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



# 6.1 Synthesis (3 contexts, above 130 %)
# Avoid divided by 0 in the denominator
COM.synthesis.df <- COM.df[!is.na(COM.df$ratio),]

# Synthesis 3 function
COM.synthesis.df <- synthesis3(spdf = COM.spdf, x = COM.synthesis.df, spdfid = "id", xid = "DEPCOM",
                                var1 = "num", var2 = "denom", key = "EPT", dist = 5000, order = NULL, mat = NULL,
                                threshold = 130, superior = TRUE)


# Cartography - Synthesis 3 contexts
layoutLayer(title = "Synthesis / 3 contexts - Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

# Colours Typology and legend layout
colours <- c("#FFFFFF", "#fdc785","#ffffab","#fba9b0",
             "#addea6","#ffa100","#fff226","#e30020")
colours <- colours[as.numeric(levels(COM.synthesis.df$synthesis3))+1]

rVal<-c(" .   .   . ","[X]  .   . ",
        " .  [X]  . ","[X] [X]  . ",
        " .   .  [X]","[X]  .  [X]",
        " .  [X] [X]","[X] [X] [X]")

# Display the typology map 
typoLayer(spdf = COM.spdf, df = COM.synthesis.df, var = "synthesis3",
          add = TRUE, 
          border = "#000000",
          lwd = 0.25,
          col = colours,
          legend.pos = "n")

# Display the legend
legendTypo(col = colours, categ = rVal,
           title.txt = "Global, Territorial and Spatial deviations above 130 %")

# Display territorial level
plot(EPT.spdf,border="#88B0FF",lwd=1,add=T)

# Box-plot for a given commune : Neuilly-sur-Seine
# to do 

head(COM.df)

# 6.2 Synthesis (3 contexts, under 70 %)
# Avoid divided by 0 in the denominator
COM.synthesis.df <- COM.df[!is.na(COM.df$ratio),]



# Synthesis 3 function
COM.synthesis.df <- synthesis3(spdf = COM.spdf, x = COM.synthesis.df, spdfid = "id", xid = "DEPCOM",
                               var1 = "num", var2 = "denom", key = "EPT", dist = 5000, order = NULL, mat = NULL,
                               threshold = 80, superior = FALSE)

head(COM.synthesis.df)
# Cartography - Synthesis 3 contexts
layoutLayer(title = "Synthesis / 3 contexts - Median income per consommation unit, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = COM.spdf)

# Colours Typology and legend layout
colours <- c("#FFFFFF", "#fdc785","#ffffab","#fba9b0",
             "#addea6","#ffa100","#fff226","#e30020")
colours <- colours[as.vector(levels(COM.synthesis.df$synthesis3))+1]

rVal<-c(" .   .   . ","[X]  .   . ",
        " .  [X]  . ","[X] [X]  . ",
        " .   .  [X]","[X]  .  [X]",
        " .  [X] [X]","[X] [X] [X]")

# Display the typology map 
typoLayer(spdf = COM.spdf, df = COM.synthesis.df, var = "colTypo",
          add = TRUE, 
          border = "#000000",
          lwd = 0.25,
          col = colours,
          legend.pos = "n")

# Display the legend
legendTypo(col = colours, categ = rVal,
           title.txt = "Global, Territorial and Spatial deviations under 80 %")

# Display territorial level
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)






COM.synthesis.df
