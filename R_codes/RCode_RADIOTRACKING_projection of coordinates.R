#projection   prob don't need...

act<-data.frame(foxid, datetime1, mydata$Activity)

# To project the GPS coordinates in correct projection system (British Nat 
# Grid)as a SpatialPointsDataFrame:
coordinates(mydata)<-xy
# WGS84 is UTM Zone 30U - in France and England. ESPG Code 27700.
proj4string(mydata) <- CRS("+proj=xy +datum=30U")
Toto <- spTransform(mydata, CRS("+init=epsg:27700"))
Coord <- data.frame(coordinates(Toto))
