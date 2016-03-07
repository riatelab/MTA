synthesis3 <- function(spdf, x, spdfid = NULL, xid = NULL,
                      var1, var2,
                      ref = NULL,
                      key,
                      order = NULL, dist = NULL, mat = NULL,
                      threshold, superior = FALSE){

  # check order & dist
  if(is.null(order) == FALSE & is.null(dist) == FALSE){
    stop("
         Define local deviation either by order or by dist, not by both.",
         call. = FALSE)
  }

  # check ids
  if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
  if (is.null(xid)){xid <- names(x)[1]}

  x <- x[, c(xid, var1, var2, key)]
  type = "rel"

  x$ratio <- x[,var1] / x[,var2]
  x$gdevrel <- globalDev(x = x, var1 = var1, var2 = var2, ref = NULL, type = type)
  x$mdevrel <- mediumDev(x = x, var1 = var1, var2 = var2, key = key, type = type)
  x$ldevrel <- localDev(spdf = spdf, x = x, spdfid = spdfid, xid = xid,
                  var1 = var1, var2 = var2, order = order, dist = dist,
                  mat = mat, type = type)
  x$tmp1 <- 0
  x$tmp2 <- 0
  x$tmp3 <- 0
  if (superior == TRUE)
  {
    x[x$gdevrel >= threshold, "tmp1"] <- 1
    x[x$mdevrel >= threshold, "tmp2"] <- 2
    x[x$ldevrel >= threshold, "tmp3"] <- 4
  }
  if (superior == FALSE)
  {
    x[x$gdevrel <= threshold, "tmp1"] <- 1
    x[x$mdevrel <= threshold, "tmp2"] <- 2
    x[x$ldevrel <= threshold, "tmp3"] <- 4
  }

  x$synthesis3 <- as.factor(x$tmp1+x$tmp2+x$tmp3)
  colours <- c("#ffffff", "#fdc785","#ffffab","#fba9b0",
               "#addea6","#ffa100","#fff226","#e30020")

  value <- as.numeric(levels(x$synthesis3))
  colours <- data.frame (value,colours)
  x <- data.frame(x, "colours"=colours[match(x$synthesis3,colours$value),c("colours")])

  return(x)
}



  cartography::typoLayer(spdf = spdf, df = x, spdfid = spdfid, dfid = xid,
                         var = "t", col = colours,
                         legend.pos = "n")

  rVal<-c(" .   .   . ","[X]  .   . ",
          " .  [X]  . ","[X] [X]  . ",
          " .   .  [X]","[X]  .  [X]",
          " .  [X] [X]","[X] [X] [X]")
  cartography::legendTypo(col = colours, categ = rVal)
  return(invisible(x))
}



library(cartography)
library(rgeos)
library(MTA)
library(RColorBrewer)

# 0. Data preparation
# Working folder
setwd("C:/Users/Ronan/Desktop/MTA_dataset")
# Data upload
MTA.df <- read.csv( "MTA_IRIS_data.csv",header=TRUE,sep=";",dec=".",encoding="utf-8",)
MTA.spdf <- rgdal::readOGR(dsn="MTA_IRIS_geom.shp", layer = "MTA_IRIS_geom")
# Calculation of the targeted indicator for the analysis (ratio = numerator / denominator)
MTA.df$num<-MTA.df$P12_NPER_RP_LOCHLMV
MTA.df$denom<-MTA.df$P12_NPER_RP
coeff<-100
MTA.df$ratio<-MTA.df$num/MTA.df$denom*coeff
MTA.df$DEP<-as.factor(MTA.df$DEP)


# 1. Territorial zonings visualisation
# Join SpatialDataFrame & DataFrame
MTA.spdf@data <- data.frame(MTA.spdf@data, MTA.df[match(MTA.spdf@data[,"DCOMIRIS"], MTA.df$CODE_IRIS),c("DEPCOM","EPT","DEP")])
# Territorial zonings aggregation
buff<-gBuffer(MTA.spdf, byid=TRUE, id=NULL, width=0.0001, quadsegs=0.0001, capStyle="ROUND",joinStyle="ROUND", mitreLimit=0.00001)
COM.spdf<-gUnaryUnion(buff,id = MTA.spdf@data$DEPCOM)
EPT.spdf<-gUnaryUnion(buff,id = MTA.spdf@data$EPT)
DEP.spdf<-gUnaryUnion(buff,id = MTA.spdf@data$DEP)
# Cartography
layoutLayer(title = "Territorial zonings available for Multiscalar Territorial Analysis",
            sources = "Data source : INSEE, IGN, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)
plot(MTA.spdf,border="#bdbdbd",lwd=0.1, col="#FCC732", add=T)
plot(COM.spdf,border="#969696",lwd=0.25,add=T)
plot(EPT.spdf,border="#252525",lwd=0.5,add=T)
plot(DEP.spdf,border="#FFFFFF",lwd=1,add=T)
someLabels <- c("IRIS, communes,établissements publics territoriaux, départements")
someColors <- c("#FCC732")
legendTypo(pos = "topleft", title.txt = "Territorial zonings in Ile-de-France, 
from the lowest to the higher geographical level",
           title.cex = 0.8,
           values.cex = 0.6, col = someColors, categ = someLabels, 
           cex = 0.75,
           nodata = FALSE, nodata.txt = "no data", frame = TRUE, symbol="box")


# 2. Numerator mapping
# Layout Plot
layoutLayer(title = "Numerator - Permanent residences in public housing unit (HLM), 2012",
            sources = "Data source : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)
# Plot IRIS
plot(MTA.spdf, col = "grey75",border = "grey20",lwd=0.2, add=TRUE)
# Plot symbols
propSymbolsLayer(spdf = MTA.spdf, df = MTA.df,
                 var = "num", 
                 symbols = "circle", col =  "#F6533A",
                 fixmax = max(MTA.df$num),
                 inches = 0.1,
                 border = "#25252570",
                 legend.pos = "right",
                 legend.title.txt = "Number of inhabitants (2012)",
                 legend.style = "c")


# 3. Denominator cartography
# Layout Plot
layoutLayer(title = "Denominator - Permanent residences, 2012",
            sources = "Source des données : INSEE, 2016",
            author = "Réalisation : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)
# Plot IRIS
plot(MTA.spdf, col = "grey75",border = "grey20",lwd=0.2, add=TRUE)
# Plot symbols
propSymbolsLayer(spdf = MTA.spdf, df = MTA.df,
                 var = "denom", 
                 symbols = "circle", col =  "#515FAA",
                 fixmax = max(MTA.df$denom),
                 border = "#25252570",
                 inches = 0.1,
                 legend.pos = "right",
                 legend.title.txt = "Number of inhabitants (2012)",
                 legend.style = "c")

# 3. Ratio cartography
# Layout Plot
layoutLayer(title = "Numerator / Denominator - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)
# Plot carto
# Delete NAs in the dataset
choroLayer(spdf = MTA.spdf,
           df = MTA.df,
           var = "ratio",
           breaks = c(min(MTA.df$ratio,na.rm=TRUE),10,20,30,40,50,60,70,80,90,max(MTA.df$ratio,na.rm=TRUE)), 
           col = carto.pal(pal1 = "turquoise.pal", n1 = 10),
           border = NA,
           add = TRUE,
           legend.pos = "topleft",
           legend.nodata = "Unpopulated area",
           legend.title.txt = "Number of inhabitants (%)",
           legend.values.rnd = 1)
plot(COM.spdf,border="#525252",lwd=0.25,add=T)
plot(EPT.spdf,border="#000000",lwd=0.5,add=T)



# 4.1 Global deviation - Relative deviation
# Deviation in relative term (100 = average of the study area)
MTA.df$gdevrel <- globalDev(x = MTA.df, 
                            var1 = "num", 
                            var2 = "denom", 
                            type = "rel")

# Cartography - Relative deviation
layoutLayer(title = "Global deviation - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)

choroLayer(spdf = MTA.spdf, df = MTA.df, var = "gdevrel",
           add = TRUE,
           border = NA,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the global context (Métropole du Grand Paris)",
           legend.nodata = "Unpopulated area",
           breaks = c(min(MTA.df$gdevrel,na.rm=TRUE),50,75,100,125,150,max(MTA.df$gdevrel,na.rm=TRUE)), 
            col = carto.pal(pal1 = "blue.pal", n1 = 3, 
                            pal2 = "wine.pal", n2 = 3))
plot(COM.spdf,border="#000000",lwd=0.25,add=T)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)

# 4.2 Global plot and indexes
library(ineq)
# Lorenz Curve
Lc <- Lc (MTA.df$num, n=MTA.df$denom)
plot(Lc,
     ylab = "proportion of numerator",
     xlab = "proportion of denominator")

# Inequality indexes
Gini<-ineq(MTA.df$ratio)
Coeff.Var<-var.coeff(MTA.df$ratio, square = FALSE, na.rm = TRUE)
Gini
Coeff.Var


#4.3 Global deviation, redistribution
# Deviation in absolute term
MTA.df$gdevabs <- globalDev(x = MTA.df, 
                            var1 = "num", 
                            var2 = "denom", 
                            type = "abs")

# Cartography - Absolute deviation
layoutLayer(title = "Global redistribution - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)
plot(MTA.spdf, col = "grey70",border = NA, add=TRUE)
propSymbolsLayer(spdf = MTA.spdf, df = MTA.df, var = "gdevabs", 
                legend.pos = "right",
                legend.title.txt = "Redistribution (inhabitants)",
                col = "#ff0000",col2 = "#0000ff",
                border = "#25252570",
                lwd = 0.1,
                inches = 0.1, legend.style = "e",
                breakval = 0)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)


# 5.1 Territorial deviation
# Deviation in relative term (100 = average of the territorial level)
MTA.df$mdevrel <- mediumDev(x = MTA.df, var1 = "num", var2 = "denom", 
                            type = "rel", key = "EPT")

# Cartography - Relative deviation
layoutLayer(title = "Territorial deviation - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)

choroLayer(spdf = MTA.spdf, df = MTA.df, var = "mdevrel",
           add = TRUE,
           border = NA,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the territorial context (Établissements Publics Territoriaux)",
           legend.nodata = "Unpopulated area",
           breaks = c(min(MTA.df$mdevrel,na.rm=TRUE),50,75,100,125,150,max(MTA.df$mdevrel,na.rm=TRUE)), 
           col = carto.pal(pal1 = "blue.pal", n1 = 3, 
                           pal2 = "wine.pal", n2 = 3))
plot(COM.spdf,border="#000000",lwd=0.25,add=T)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)

# 5.2 Territorial plot
n<-print(nlevels(MTA.df$EPT)) 
col<-brewer.pal(n, "Paired")

par(cex.lab=1)
par(cex.axis=0.75)
par(mar=c(12,5,1,1))

boxplot(MTA.df$mdevrel ~ MTA.df$LIB_EPT,
        col = col,
        ylab = "Territorial deviation",
        varwidth = TRUE,
        outline = FALSE,
        las = 2) 


# 5.3 Territorial redistribution
# Deviation in absolute term
MTA.df$mdevabs <- mediumDev(x = MTA.df, var1 = "num", var2 = "denom", 
                            type = "abs", key = "EPT")

# Cartography - Absolute deviation
layoutLayer(title = "Territorial redistribution - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)
plot(MTA.spdf, col = "grey70",border = NA, add=TRUE)
propSymbolsLayer(spdf = MTA.spdf, df = MTA.df, var = "mdevabs", 
                 legend.pos = "right",
                 legend.title.txt = "Redistribution (inhabitants)",
                 col = "#ff0000",col2 = "#0000ff",
                 border = "#25252570",
                 lwd = 0.1,
                 inches = 0.1, legend.style = "e",
                 breakval = 0)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)


# 6.1 Spatial deviation
# Deviation in relative term (100 = average of the neigbouring territorial units under the threshold of 5000 meters)
MTA.df$ldevrel <- localDev(spdf = MTA.spdf, x = MTA.df, spdfid = "DCOMIRIS", xid = "CODE_IRIS",
                           var1 = "num", var2 = "denom", dist = 5000,
                           type = "rel")

# Cartography - Relative deviation
layoutLayer(title = "Spatial deviation (5 km) - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)

choroLayer(spdf = MTA.spdf, df = MTA.df, var = "ldevrel",
           add = TRUE,
           border = NA,
           legend.pos = "topleft",
           legend.title.txt = "Deviation to the spatial context (IRIS located at less than 5 km)",
           legend.nodata = "Unpopulated area",
           breaks = c(min(MTA.df$ldevrel,na.rm=TRUE),50,75,100,125,150,max(MTA.df$ldevrel,na.rm=TRUE)), 
           col = carto.pal(pal1 = "blue.pal", n1 = 3, 
                           pal2 = "wine.pal", n2 = 3))
plot(COM.spdf,border="#000000",lwd=0.25,add=T)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)


# 6.2 Spatial plot

# Spatial autocorrelation
spat.aut<-lm(MTA.df$ldevrel ~ MTA.df$gdevrel)
summary.lm(spat.aut)
Intercept(spat.aut)
names(summary(spat.aut))$coefficients
summary(spat.aut)$coefficients

plot(MTA.df$gdevrel,MTA.df$ldevrel,
     ylab = "Spatial deviation",
     xlab = "Global deviation",
     pch = 20,
     col = col)
abline(spat.aut, col = "red", lwd =2)

legend("topleft",
       legend = levels(MTA.df$EPT),
       pch = 20,
       col = col,
       cex = 0.5,
       pt.cex = 1,
       title = "Territorial context")


#6.3 Spatial redistribution 
#Deviation in absolute term
MTA.df$ldevabs <- localDev(spdf = MTA.spdf, x = MTA.df, spdfid = "DCOMIRIS", xid = "CODE_IRIS",
                           var1 = "num", var2 = "denom", dist = 5000,
                           type = "abs")

# Cartography - Absolute deviation
layoutLayer(title = "Spatial redistribution (5 km) - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)
plot(MTA.spdf, col = "grey70",border = NA, add=TRUE)
propSymbolsLayer(spdf = MTA.spdf, df = MTA.df, var = "ldevabs", 
                 legend.pos = "right",
                 legend.title.txt = "Redistribution (inhabitants)",
                 col = "#ff0000",col2 = "#0000ff",
                 border = "#25252570",
                 lwd = 0.1,
                 inches = 0.1, legend.style = "e",
                 breakval = 0)
plot(EPT.spdf,border="#f0f0f0",lwd=0.5,add=T)



# 7.1 Synthesis (3 contexts, above 130 %)
# Avoid divided by 0 in the denominator
MTA.synthesis.df <- MTA.df[!is.na(MTA.df$ratio),]

# Deviation in relative term (100 = average of the neigbouring territorial units under the threshold of 5000 meters)
MTA.synthesis.df <- synthesis3(spdf = MTA.spdf, x = MTA.synthesis.df, spdfid = "DCOMIRIS", xid = "CODE_IRIS",
                           var1 = "num", var2 = "denom", key = "EPT", dist = 5000, order = NULL, mat = NULL,
                           threshold = 130, superior = TRUE)


# Cartography - Synthesis 3 contexts
layoutLayer(title = "Synthesis / 3 contexts - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)

# Colours Typology and legend layout
colours <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
             "#addea6","#ffa100","#fff226","#e30020")
colours <- colours[as.numeric(levels(MTA.synthesis.df$synthesis3))+1]

rVal<-c(" .   .   . ","[X]  .   . ",
        " .  [X]  . ","[X] [X]  . ",
        " .   .  [X]","[X]  .  [X]",
        " .  [X] [X]","[X] [X] [X]")

# Display the typology map 
typoLayer(spdf = MTA.spdf, df = MTA.synthesis.df, var = "synthesis3",
          add = TRUE, 
          border = NA, 
          col = colours,
          legend.pos = "n")

# Display the legend
legendTypo(col = colours, categ = rVal,
           title.txt = "Global, Territorial and Spatial deviations above 130 %")

# Display territorial level
plot(EPT.spdf,border="#252525",lwd=0.5,add=T)


# 7.2 Synthesis (3 contexts, under 50 %)
# Avoid divided by 0 in the denominator
MTA.synthesis.df <- MTA.df[!is.na(MTA.df$ratio),]

# Deviation in relative term (100 = average of the neigbouring territorial units under the threshold of 5000 meters)
MTA.synthesis.df <- synthesis3(spdf = MTA.spdf, x = MTA.synthesis.df, spdfid = "DCOMIRIS", xid = "CODE_IRIS",
                               var1 = "num", var2 = "denom", key = "EPT", dist = 5000, order = NULL, mat = NULL,
                               threshold = 50, superior = FALSE)


# Cartography - Synthesis 3 contexts
layoutLayer(title = "Synthesis / 3 contexts - Share of public housing in permanent residences, 2012",
            sources = "Data sources : INSEE, 2016",
            author = "Author : RIATE, 2016",
            scale = 5,
            frame = TRUE,
            col = "black",
            coltitle = "white",
            bg = "#FFFFFF",
            south = FALSE,
            extent = MTA.spdf)

# Colours Typology and legend layout
colours <- c("#f0f0f0", "#fdc785","#ffffab","#fba9b0",
             "#addea6","#ffa100","#fff226","#e30020")
colours <- colours[as.numeric(levels(MTA.synthesis.df$synthesis3))+1]

rVal<-c(" .   .   . ","[X]  .   . ",
        " .  [X]  . ","[X] [X]  . ",
        " .   .  [X]","[X]  .  [X]",
        " .  [X] [X]","[X] [X] [X]")

# Display the typology map 
typoLayer(spdf = MTA.spdf, df = MTA.synthesis.df, var = "synthesis3",
          add = TRUE, 
          border = NA, 
          col = colours,
          legend.pos = "n")

# Display the legend
legendTypo(col = colours, categ = rVal,
           title.txt = "Global, Territorial and Spatial deviations under 50 %")

# Display territorial level
plot(EPT.spdf,border="#252525",lwd=0.5,add=T)


 


