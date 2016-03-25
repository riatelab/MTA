library(rgdal)
com.spdf <- readOGR(dsn = '/data/depot/MTA/inst/data', layer = 'MTA_communes')
com.spdf@proj4string
row.names(com.spdf) <- as.character(com.spdf$id)
names(com.spdf)[1] <- "DEPCOM"

com <- read.csv("/data/depot/MTA/inst/data/MTA_COM_data.csv", stringsAsFactors=FALSE)
com$LIBCOM <- iconv(com$LIBCOM, "latin1", "ASCII//TRANSLIT", "bytes")
com$LIB_EPT <- iconv(com$LIB_EPT, "latin1", "ASCII//TRANSLIT", "bytes")
names(com)[c(4,6,7)] <- c("LIBEPT", "INC", "TH")
com <- com[,1:7]

ept <- com.spdf 
ept@data <- com[match(x = com$DEPCOM, table = ept@data$DEPCOM),]
ept.spdf <- aggregate(ept[,3:4], by  = list(ept$EPT, ept$LIBEPT))
ept.spdf@data <- ept.spdf@data[,1:2]
names(ept.spdf) <- c('EPT', 'LIBEPT')

save(list = c("com.spdf", "ept.spdf", "com"), 
     file = "data/GrandParisMetropole.RData", 
     compress = "xz" )

load("data/GrandParisMetropole.RData")
