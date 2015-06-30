#############################################
# MTA: Multiscalar territorial analysis
# Nicolas LAMBERT, 2015
#############################################

#rm(list=ls())

# PACKAGES

library(rgeos)
library(reshape2)

# DATA

#' Spatial dataframe (polygons) of the NUTS regions (2006)
#' 
#' European regions (NUTS 2006 version)
#'@name nuts2006 
#'@field nuts3 nuts3 code
#'@field nuts2 nuts2 code
#'@field nuts1 nuts1 code
#'@field nuts0 nuts0 code
#'@field gdp2008 Gross domestic product in 2008 (euros) - source : ESPON database, 2014
#'@field gdppps2008 Gross domestic product (ppp) in 2008 (euros) - source : ESPON database, 2014
#'@field popt2008 total population in 2008 (inh.) - source : ESPON database, 2014
#'
NULL
# GLOBAL DEVIATION ----------------------


#' Global deviation
#'
#' This function compute the global deviation from two count data according to the MyperAtlas Methodolgy.
#' 
#' @details The general context may be the whole chosen study area. In such a case, the associated map will be the same as the associated map to the ratio itself. So, the user may choose another general context or a reference value. For instance, in the example of the EU, even if the study area is the 29 potential countries, it may be of interest to observe the spatial differentiations according to another global reference, for instance the global value associated to EU15. For this level, the user may also exogenously enter a value. By default, this value has first been set to the value of the global area. 
#'
#' @param spdf Spatial polygon data frame
#' @param id Unique identifier of the data frame (character)
#' @param stock1 Numerator
#' @param stock2 Denominator
#' 
#' @examples
#' dev <-globalDev(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008")
#' head(dev)
#' 
#' @return dataframe
#' 
#' @export


globalDev<-function(spdf,id,stock1,stock2){
  global <- sum(spdf@data[stock1])/sum(spdf@data[stock2])
  spdf@data$ratio <- spdf@data[stock1] / spdf@data[stock2]
  spdf@data$dev<-(spdf@data$ratio/global)*100
  globaldev <- data.frame(spdf@data[,id],spdf@data[stock1],spdf@data[stock2],spdf@data$ratio,spdf@data$dev)
  names(globaldev) <- c(id,stock1,stock2,"ratio","globalRel")
  # redistribution
  globaldev$globalAbs <- globaldev[,stock1]-(global*globaldev[,stock2])
  return(globaldev)
}
   
# MEDIUM DEVIATION ----------------------

#' Medium deviation
#'
#' This function compute the medium deviation from two count data according to the MyperAtlas Methodolgy.
#' 
#' @details The territorial context, on the other hand, has to be a geographical zoning that is an aggregation of the “elementary zoning” that was previously chosen.
#'
#' @param spdf Spatial polygon data frame
#' @param id Unique identifier of the data frame (character)
#' @param stock1 Numerator
#' @param stock2 Denominator
#' @param key Aggregation key
#' 
#' @examples
#' dev <-mediumDev(spdf=nuts2006,id="nuts3",stock1="gdp2008",stock2="popt2008",key="nuts0")
#' head(dev)
#' 
#' @return dataframe
#' 
#' @export

mediumDev<-function(spdf,id,stock1,stock2,key){
  stock1_med<-aggregate(spdf@data[,stock1], by = list(key = spdf@data[,key]), sum, simplify=TRUE)
  stock2_med<-aggregate(spdf@data[,stock2], by = list(key = spdf@data[,key]), sum, simplify=TRUE)
  med<-data.frame(c(stock1_med,stock2_med))
  med$tmp<-(med$x/med$x.1)
  med <- data.frame(med[1],med$tmp)
  colnames(med)<-c("key","medium")
  mediumDev <- data.frame(spdf@data, medium=med[match(spdf@data[,key], med$key),2])
  mediumDev$ratio <- mediumDev[,stock1] / mediumDev[,stock2]
  mediumDev$MediumDev<-(mediumDev$ratio/mediumDev$medium)*100
  mediumDev <- data.frame(mediumDev[id],mediumDev[stock1],mediumDev[stock2],mediumDev$ratio,mediumDev$MediumDev, mediumDev$medium)
  names(mediumDev) <- c(id,stock1,stock2,"ratio","mediumRel","medium")
  # redistribution
  mediumDev$mediumAbs <- mediumDev[,stock1]-(mediumDev$medium*mediumDev[,stock2])
  mediumDev  <- mediumDev [!(names(mediumDev) %in% "medium")] 
  return(mediumDev)
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
#' @param dist distance (in the units of the basemap) between centroids to determinate the local context. 0 = contiguity.
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