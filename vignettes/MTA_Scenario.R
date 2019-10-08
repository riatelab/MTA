## ----synthesislow, fig.width=7, fig.height=5, warning = FALSE, cache = TRUE, eval = TRUE----

par(mfrow = c(1,1), mar = c(0,0,1.2,0))

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


