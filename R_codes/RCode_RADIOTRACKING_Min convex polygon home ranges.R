library(adehabitatHR)

Fox3732
mydata<-read.table("3732_2011.txt", header=TRUE)
Fox3732

Fox3689
mydata<-read.table("3689_2011.txt", header=TRUE)
Fox3689

Fox3739
mydata<-read.table("3739_2011.txt", header=TRUE)
Fox3739

#Calculate MCP
xy <- mydata[,c("X","Y")] 
ii<-SpatialPoints(xy)

#Calculate MCP area, default units are hectares
mcp.area(ii, percent=95,
		unin = c("m", "km"),
		unout = c("ha", "km2", "m2"), plotit = TRUE)

F3689mcp<-mcp.area(ii,percent=95,
		unout = c("m2"), plotit = TRUE)

mcp(ii,percent=95) 



cp <- mcp(ii, percent=95)
class(cp)
plot(cp)