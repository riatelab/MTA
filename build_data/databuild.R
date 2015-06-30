library("rgdal")

setwd("/home/nlambert/Documents/R/MTA/build_data")

spdf <- readOGR(dsn ="data/",layer = "GEOM_EU34_NUTS2006_poly")

#colnames(spdf@data) <-"nuts3"
#spdf<-spdf[spdf@data$level3==1,]
spdf@data <- data.frame(nuts3=spdf@data$ID)
spdf@data$nuts2 <-substr(spdf@data$nuts3,1,4)
spdf@data$nuts1 <-substr(spdf@data$nuts3,1,3)
spdf@data$nuts0 <-substr(spdf@data$nuts3,1,2)

df <- read.csv("data/data_nuts3.txt",header=T,sep="\t",dec=",",encoding="utf-8")
head(df)

spdf@data <- data.frame(spdf@data, df[match(spdf@data$nuts3, df$ID),c("gdp_2008","gdp_pps_2008","pop_t_2008")])
colnames(spdf@data)<-c("nuts3","nuts2","nuts1","nuts0","gdp2008","gdppps2008","popt2008")

spdf@data$gdp2008 <- as.numeric(spdf@data$gdp2008)
spdf@data$gdppps2008 <- as.numeric(spdf@data$gdppps2008)
spdf@data$popt2008 <- as.numeric(spdf@data$popt2008)

nuts2006 <- spdf

save(nuts2006, file = "../data/nuts2006.RData")

