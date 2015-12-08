library(cartography)
x <- nuts3.df
x$gdppps2008 <- x$gdppps2008 * 1000000
x$key <- substr(x = x$id, start = 1, stop = 3)
nuts3.df$mdevabs <- mediumDev(x = x, var1 = "gdppps2008", var2 = "pop2008", type = "abs", key = "key")
nuts3.df$mdevrel <- mediumDev(x = x, var1 = "gdppps2008", var2 = "pop2008", type = "rel", key = "key")
par(mar = c(0,0,0,0))
plot(nuts0.spdf[nuts0.spdf$id %in% c("BE", "NL", "LU", "DE"),] )
choroLayer(spdf = nuts3.spdf, df = nuts3.df, var = "mdevrel", 
           legend.pos = "topright", breaks = c(11,50,75,99,101,125,150,7000), 
           col = carto.pal(pal1 = "blue.pal", 3, pal2 = "wine.pal", 3, middle = T), border = NA, add=T)
plot(nuts1.spdf, add=T)
propSymbolsLayer(spdf = nuts3.spdf, df = nuts3.df, var = "mdevabs", 
                 col = "#ff000050",col2 = "#0000ff50", k = 0.001, 
                 breakval = 0)