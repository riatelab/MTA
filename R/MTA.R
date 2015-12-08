#' @title Global deviation
#' @name globalDev
#' @description This function compute the global deviation from two 
#' count data according to the HyperAtlas Methodolgy (methodo), see details.
#' @details The general context may be the whole chosen study area. 
#' In such a case, the associated map will be the same as the associated 
#' map to the ratio itself. So, the user may choose another general context 
#' or a reference value. For instance, in the example of the EU, even if 
#' the study area is the 29 potential countries, it may be of interest 
#' to observe the spatial differentiations according to another global 
#' reference, for instance the global value associated to EU15. For this 
#' level, the user may also exogenously enter a value. By default, this value 
#' has first been set to the value of the global area. 
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param ref ratio of reference; if NULL the mean ratio.
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned
#' @examples 
#' library(cartography)
#' x <- nuts3.df
#' nuts3.df$gdevabs <- globalDev(x = x, var1 = "gdppps2008", var2 = "pop2008", type = "abs")
#' nuts3.df$gdevrel <- globalDev(x = x, var1 = "gdppps2008", var2 = "pop2008", type = "rel")
#' par(mar = c(0,0,0,0))
#' choroLayer(spdf = nuts3.spdf, df = nuts3.df, var = "gdevrel", 
#'            legend.pos = "topright", breaks = c(11,50,75,100,125,150,7000), 
#'            col = carto.pal(pal1 = "blue.pal", 3, pal2 = "wine.pal", 3))
#' propSymbolsLayer(spdf = nuts3.spdf, df = nuts3.df, var = "gdevabs", 
#'                  col = "#ff000050",col2 = "#0000ff50", k = 0.001, 
#'                  breakval = 0)
#' @export
globalDev <- function(x, var1, var2, ref = NULL, type = "rel"){
  # no ref value
  if (is.null(ref)){
    ref <- sum(x[,var1]) / sum(x[,var2])
  }
  # relative deviation
  if (type=="rel"){
    v <- ((x[,var1] / x[,var2]) / ref) * 100
  }
  # absolute deviation
  if (type=="abs"){
    v <- x[,var1] - (ref * x[,var2])
  }
  return(v)
}

#' @title Medium deviation
#' @description This function compute the medium deviation from two count 
#' data according to the HyperAtlas Methodolgy.
#' @details The territorial context, on the other hand, has to be a 
#' geographical zoning that is an aggregation of the “elementary zoning” 
#' that was previously chosen.
#' @param x a data frame.
#' @param var1 name of the numerator variable in x.
#' @param var2 name of the denominator variable in x.
#' @param key aggregation key field.
#' @param type type of deviation; "rel" for relative deviation, "abs" for 
#' absolute deviation.
#' @return A vector is returned
#' @examples
#' library(cartography)
#' x <- nuts3.df
#' x$gdppps2008 <- x$gdppps2008 * 1000000
#' x$key <- substr(x = x$id, start = 1, stop = 3)
#' nuts3.df$mdevabs <- mediumDev(x = x, var1 = "gdppps2008", var2 = "pop2008", type = "abs", key = "key")
#' nuts3.df$mdevrel <- mediumDev(x = x, var1 = "gdppps2008", var2 = "pop2008", type = "rel", key = "key")
#' par(mar = c(0,0,0,0))
#' plot(nuts0.spdf[nuts0.spdf$id %in% c("BE", "NL", "LU", "DE"),] )
#' choroLayer(spdf = nuts3.spdf, df = nuts3.df, var = "mdevrel", 
#'            legend.pos = "topright", breaks = c(11,50,75,99,101,125,150,7000), 
#'            col = carto.pal(pal1 = "blue.pal", 3, pal2 = "wine.pal", 3, middle = T), border = NA, add=T)
#' plot(nuts1.spdf, add=T)
#' propSymbolsLayer(spdf = nuts3.spdf, df = nuts3.df, var = "mdevabs", 
#'                  col = "#ff000050",col2 = "#0000ff50", k = 0.001, 
#'                  breakval = 0)
#' @export
mediumDev <- function(x, var1, var2, key, type = "rel"){
  med <- aggregate(x[,c(var1, var2)], by = list(x[,key]), sum)
  med$ratio <- med[,var1] / med[,var2]
  mediumDev <- data.frame(x[,c(var1,var2)], 
                          med = med[match(x[,key], med$Group.1), 4])
  if (type=="rel"){
    v <- ((mediumDev[,var1] / mediumDev[,var2]) / mediumDev$med) * 100
  }
  if (type=="abs"){
    v <- mediumDev[,var1] - (mediumDev[,var2] * mediumDev$med)
  }
  return(v)
}

# LOCAL DEVIATION ----------------------


#' Local deviation
#'
#' This function compute the medium deviation from two count data according to the MyperAtlas Methodolgy.
#' 
#' @details The spatial context shows which proximity relation will be the basis of the neighborhood’s definition for each elementary unit. That is usually “contiguity”, but it may also be a relationship based on distances since they have been introduced in the hyp file (units that are less than X kilometers far from), or time-distances. Then, each elementary unit value will be compared to the value of its neighborhood.
#'
#' @param spdf Spatial polygon data frame
#' @param id Unique identifier of the data frame (character)
#' @param stock1 Numerator
#' @param stock2 Denominator
#' @param dist distance (in the units of the basemap) between centroids to 
#' determinate the local context. 0 = contiguity.
#' 
#' @examples
#' dev <-localDev(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",distance=0)
#' head(dev)
#' 
#' @return dataframe
#' 
#' @export

localDev<-function(spdf,id,stock1,stock2,distance){
  
  if(distance==0) {
    dist<-gIntersects(spdf, byid = TRUE, prepared=TRUE)
  }
  else {
    centres<-gCentroid(spdf, byid=TRUE, id = NULL)
    dist<-gWithinDistance(centres,byid=TRUE,dist=distance)
  }
  
  row.names(dist)<-spdf@data[,id]
  colnames(dist)<-spdf@data[,id]
  dist<-melt(dist,variable.name=1, na.rm=TRUE) 
  colnames(dist)<-c("i","j","cij")
  dist<-dist[dist$cij==TRUE,]
  
  dist = data.frame(dist, spdf@data[match(dist[,"j"], spdf@data[,id]),c(stock1,stock2)])
  local_stock1<-aggregate(dist[stock1], by = list(i = dist$i), sum, simplify = TRUE)
  local_stock2<-aggregate(dist[stock2], by = list(i = dist$i), sum, simplify = TRUE)
  local = data.frame(local_stock1, local_stock2[match(local_stock1[,"i"],local_stock1[,"i"]),2])
  colnames(local)<-c(id,"stock1_aggr","stock2_aggr")
  localDev <- data.frame(spdf@data[,c(id,stock1,stock2)],local[match(spdf@data[,id], local[,id]),c("stock1_aggr","stock2_aggr")])
  localDev$ratio <-localDev[,stock1]/localDev[,stock2]
  localDev$ratio_aggr <- localDev$stock1_aggr/localDev$stock2_aggr
  localDev$localRel<-(localDev$ratio/localDev$ratio_aggr)*100
  localDev <- localDev[,c(id,stock1,stock2,"ratio","localRel","ratio_aggr")]
  # redistribution
  localDev$localAbs <- localDev[,stock1]-(localDev$ratio_aggr*localDev[,stock2])
  localDev  <- localDev [!(names(localDev) %in% "ratio_aggr")] 
  
  return(localDev)
  
}

# TYPOLOGY (3) ----------------------

#' Multiscalar typology
#'
#' This function compute a multiscalar typology according to the three relative positions about contexts (global, medium, local). The elementary units are classified in eight classes according to their three relative positions.
#' 
#' @details xxx
#' 
#' @param spdf Spatial polygon data frame
#' @param id Unique identifier of the data frame (character)
#' @param stock1 Numerator
#' @param stock2 Denominator
#' @param dist distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
#' @param key Aggregation ky for the medium deviatiopn
#' @param threshold Threshold of typology
#' @param superior If true, the criterium is "gretter than". If false, the criterium is "lower than" 
#' @param map If true, the typology is ploted
#' 
#' @examples
#' # synthesis (without plot)
#' typology <-synthesis(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",distance=100000,key="nuts0",threshold=125,superior=TRUE,map=FALSE)
#' 
#' # synthesis (with plot)
#' typology <-synthesis(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",distance=100000,key="nuts0",threshold=125,superior=TRUE,map=TRUE)
#' 
#' @return dataframe
#' 
#' @export



synthesis <- function(spdf,id,stock1,stock2,distance=0,key,threshold=100,superior=TRUE,map=FALSE){
  
  glodev <-globalDev(spdf=spdf,id=id,stock1=stock1,stock2=stock2)
  meddev <-mediumDev(spdf=spdf,id=id,stock1=stock1,stock2=stock2,key=key)
  locdev <-localDev(spdf=spdf,id=id,stock1=stock1,stock2=stock2,distance=0)
  
  typology <- spdf
  typology@data <- data.frame(typology@data[,c(id,stock1,stock2)])
  typology@data <- data.frame(typology@data,glodev[match(typology@data[,id], glodev[,id]),"globalRel"])
  typology@data <- data.frame(typology@data,meddev[match(typology@data[,id], meddev[,id]),"mediumRel"])
  typology@data <- data.frame(typology@data,locdev[match(typology@data[,id], locdev[,id]),"localRel"])
  colnames(typology@data)<-c(id,stock1,stock2,"global","medium","local")
  
  typology@data$tmp1<-0 ; typology@data$tmp2<-0 ; typology@data$tmp3<-0 ; typology@data$typo<-0
  
  if (superior==TRUE)
  {
    typology@data$tmp1[typology@data$global>=threshold]<-1
    typology@data$tmp2[typology@data$medium>=threshold]<-2
    typology@data$tmp3[typology@data$local>=threshold]<-4
  }
  
  if (superior==FALSE)
  {
    typology@data$tmp1[typology@data$global<=threshold]<-1
    typology@data$tmp2[typology@data$medium<=threshold]<-2
    typology@data$tmp3[typology@data$local<=threshold]<-4
  }
  
  typology@data$typo<-typology@data$tmp1+typology@data$tmp2+typology@data$tmp3
  typology@data$ratio <- typology@data[,stock1]/typology@data[,stock2]
  typology@data <- typology@data[,c(id,stock1,stock2,"ratio","global","medium","local","typo")]
  typology@data$typo<-as.factor(typology@data$typo)
  
  # colors
  colours<-c("#ffffff","#fdc785","#ffffab","#fba9b0","#addea6","#ffa100","#fff226","#e30020")
  typology@data$col<-colours[1]
  typology@data$col[typology@data$typo==1]<-colours[2]
  typology@data$col[typology@data$typo==2]<-colours[3]
  typology@data$col[typology@data$typo==3]<-colours[4]
  typology@data$col[typology@data$typo==4]<-colours[5]
  typology@data$col[typology@data$typo==5]<-colours[6]
  typology@data$col[typology@data$typo==6]<-colours[7]
  typology@data$col[typology@data$typo==7]<-colours[8]
  
  # PLOT
  if(map==T){
    cols <- typology@data$col
    rVal<-c(" .   .   . ","[X]  .   . "," .  [X]  . ","[X] [X]  . "," .   .  [X]","[X]  .  [X]"," .  [X] [X]","[X] [X] [X]")
    if (superior==T){symbol = ">="}else{symbol = "<="}
    if (distance==0){mydist = "contiguity"}else {mydist = paste(distance," units",sep="")}
    plot(typology, col=cols,lwd=0.2,border="black")
    
    # ICI, ploter le niveau intermédiaire
    buff<-gBuffer(spdf, byid=TRUE, id=NULL, width=1, quadsegs=5, capStyle="ROUND",joinStyle="ROUND", mitreLimit=1.0)
    mediumLayer<-gUnaryUnion(buff,id = buff@data[,key])
    plot(mediumLayer,col="#FFFFFF00",lwd=1,add=T)
    
    title(main=paste("Multiscalar typology\n(",stock1," / ",stock2,")",sep=""),sub=paste("params: local deviation = ",mydist, " medium deviation = ",key),cex.sub=0.7)
    legend("topright",legend=rVal, fill=colours,bty="n",pt.cex=1,cex=0.7,title=paste("Deviation ",symbol," ",threshold,"\n[glo] [med] [loc]",sep=""))
  }
  
  return(typology)
  
}