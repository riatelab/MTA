# 
# 
# 
#   cartography::typoLayer(spdf = spdf, df = x, spdfid = spdfid, dfid = xid, 
#                          var = "t", col = colours, 
#                          legend.pos = "n")
#   
#   rVal<-c(" .   .   . ","[X]  .   . ",
#           " .  [X]  . ","[X] [X]  . ",
#           " .   .  [X]","[X]  .  [X]",
#           " .  [X] [X]","[X] [X] [X]")
#   cartography::legendTypo(col = colours, categ = rVal)
#   return(invisible(x))
# }







# 
# # TYPOLOGY (3) ----------------------
# 
# #' Multiscalar typology
# #'
# #' This function compute a multiscalar typology according to the three relative positions about contexts (global, medium, local). The elementary units are classified in eight classes according to their three relative positions.
# #' 
# #' @details xxx
# #' 
# #' @param spdf Spatial polygon data frame
# #' @param id Unique identifier of the data frame (character)
# #' @param stock1 Numerator
# #' @param stock2 Denominator
# #' @param dist distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
# #' @param key Aggregation ky for the medium deviatiopn
# #' @param threshold Threshold of typology
# #' @param superior If true, the criterium is "gretter than". If false, the criterium is "lower than" 
# #' @param map If true, the typology is ploted
# #' 
# #' @examples
# #' # synthesis (without plot)
# #' typology <-synthesis(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",distance=100000,key="nuts0",threshold=125,superior=TRUE,map=FALSE)
# #' 
# #' # synthesis (with plot)
# #' typology <-synthesis(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",distance=100000,key="nuts0",threshold=125,superior=TRUE,map=TRUE)
# #' 
# #' @return dataframe
# #' 
# #' @export
# 
# 
# 
# synthesis <- function(spdf,id,stock1,stock2,distance=0,key,threshold=100,superior=TRUE,map=FALSE){
#   
# synthesis <- function(spdf, x, spdfid = NULL, xid = NULL, 
#                       var1, var2, 
#                       ref = NULL, 
#                       key,
#                       order = NULL, dist = NULL, mat = NULL, 
#                       threshold = 100, superior = FALSE){
#   
#   # check order & dist
#   if(is.null(order) == FALSE & is.null(dist) == FALSE){
#     stop("
# Define local deviation either by order or by dist, not by both.",
#          call. = FALSE)
#   }
# 
#   # check ids
#   if (is.null(spdfid)){spdfid <- names(spdf@data)[1]}
#   if (is.null(xid)){xid <- names(x)[1]}
#   
#   x <- x[, c(xid, var1, var2, key)]
#   type = "rel"
# 
#   x$r <- x[,var1] / x[,var2]
#   x$g <- globalDev(x = x, var1 = var1, var2 = var2, ref = ref, type = type)
#   x$m <- mediumDev(x = x, var1 = var1, var2 = var2, key = key, type = type)
#   x$l <- localDev(spdf = spdf, x = x, spdfid = spdfid, xid = xid, 
#                   var1 = var1, var2 = var2, order = order, dist = dist, 
#                   mat = mat, type = type)
#   x$tmp1 <- 0
#   x$tmp2 <- 0
#   x$tmp3 <- 0
#   if (superior == TRUE)
#   {
#     x[x$g >= threshold, "tmp1"] <- 1
#     x[x$m >= threshold, "tmp2"] <- 2
#     x[x$l >= threshold, "tmp3"] <- 4
#   }
#   if (superior == FALSE)
#   {
#     x[x$g <= threshold, "tmp1"] <- 1
#     x[x$m <= threshold, "tmp2"] <- 2
#     x[x$l <= threshold, "tmp3"] <- 4
#   }
# 
#   x$t <- as.factor(x$tmp1+x$tmp2+x$tmp3)
#   colours <- c("#ffffff", "#fdc785","#ffffab","#fba9b0",
#                 "#addea6","#ffa100","#fff226","#e30020")
#   colours <- colours[as.numeric(levels(x$t))+1]
#   cartography::typoLayer(spdf = spdf, df = x, spdfid = spdfid, dfid = xid, 
#                          var = "t", col = colours, 
#                          legend.pos = "n")
# 
#   rVal<-c(" .   .   . ","[X]  .   . ",
#           " .  [X]  . ","[X] [X]  . ",
#           " .   .  [X]","[X]  .  [X]",
#           " .  [X] [X]","[X] [X] [X]")
#   cartography::legendTypo(col = colours, categ = rVal)
#   return(invisible(x))
# }
# typology@data$typo<-typology@data$tmp1+typology@data$tmp2+typology@data$tmp3
#   typology@data$ratio <- typology@data[,stock1]/typology@data[,stock2]
#   typology@data <- typology@data[,c(id,stock1,stock2,"ratio","global","medium","local","typo")]
#   typology@data$typo<-as.factor(typology@data$typo)
#   
#   # colors
#   colours<-c("#ffffff","#fdc785","#ffffab","#fba9b0","#addea6","#ffa100","#fff226","#e30020")
#   typology@data$col<-colours[1]
#   typology@data$col[typology@data$typo==1]<-colours[2]
#   typology@data$col[typology@data$typo==2]<-colours[3]
#   typology@data$col[typology@data$typo==3]<-colours[4]
#   typology@data$col[typology@data$typo==4]<-colours[5]
#   typology@data$col[typology@data$typo==5]<-colours[6]
#   typology@data$col[typology@data$typo==6]<-colours[7]
#   typology@data$col[typology@data$typo==7]<-colours[8]
#   
#   # PLOT
#   if(map==T){
#     cols <- typology@data$col
#     rVal<-c(" .   .   . ","[X]  .   . "," .  [X]  . ","[X] [X]  . "," .   .  [X]","[X]  .  [X]"," .  [X] [X]","[X] [X] [X]")
#     if (superior==T){symbol = ">="}else{symbol = "<="}
#     if (distance==0){mydist = "contiguity"}else {mydist = paste(distance," units",sep="")}
#     plot(typology, col=cols,lwd=0.2,border="black")
#     
#     # ICI, ploter le niveau intermédiaire
#     buff<-gBuffer(spdf, byid=TRUE, id=NULL, width=1, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
#     mediumLayer<-gUnaryUnion(buff,id = buff@data[,key])
#     plot(mediumLayer,col="#FFFFFF00",lwd=1,add=T)
#     
#     title(main=paste("Multiscalar typology\n(",stock1," / ",stock2,")",sep=""),sub=paste("params: local deviation = ",mydist, " medium deviation = ",key),cex.sub=0.7)
#     legend("topright",legend=rVal, fill=colours,bty="n",pt.cex=1,cex=0.7)
#     ,title=paste("Deviation ",symbol," ",threshold,"\n[glo] [med] [loc]",sep=""))
#   }
#   
#   return(typology)
# 
