
# SIMULATING VIRTUAL SPECIES DISTRIBUTIONS 

library(virtualspecies)
library(raster)

library(rgdal)
library(raster)
library(sp)

# IMPORT CLIMATE DATA ####

# BEST IMPORT METHOD: import World Climate data from http://worldclim.org/version2 (needs internet) - imports world map of temp, precip etc as a raster (pixels are measurements)
worldclim <- getData("worldclim", var = "bio", res = 10) # bio = bioclimatic variables derived from the tmean, tmin, tmax and prec (see http://worldclim.org/bioclim for descriptions of layers)
# at lower resolutions (res=2.5, 5 and 10), can download data for the whole planet at once (10 mins of degree = approx 18km2 resolution)
# at high resolutions have to specify latlong of the required grid space.
w <- getData('worldclim', var='tmin', res=0.5, lon=5, lat=45) # tmin = min temperature

worldclim # already a RasterStack


# SLOWER IMPORT METHOD: Download data from http://worldclim.org/version2 into MyDocuments and import each specific layer separately
r1 <- raster("E:/R/Species distrib mapping/WorldClimV2_BioClimaticVariables_5minSpatialRes/bio/bio_1")
r2 <- raster("E:/R/Species distrib mapping/WorldClimV2_BioClimaticVariables_5minSpatialRes/bio/bio_2")
s <- stack(r1, r2) # combine RasterLayers into a RasterStack


# VIEW AND PLOT RASTER LAYER DATA ####

# View names of layers in RasterStack
names(worldclim)

# modify layer names
names(my.rasterstack) <- c("name1", "name2", etc.).

# View subsets of layers 
worldclim[[1:4]] # the first four layers
worldclim[[c("bio1", "bio12")]] # Layers bio1 and bio12 (annual mean temperature and annual precipitation)

# Plot variables (layers)
par(oma = c(0.1, 0.1, 0.1, 2.1)) # set margins
plot(worldclim[[c("bio1", "bio12")]]) # plot layers bio1 (annual mean temp) and bio12 (annual precipitation) # see http://worldclim.org/bioclim for descriptions of layers


# PREPARE SIMULATION OF VIRTUAL SPECIES ####
# Criteria is species that lives in hot and humid conditions, so temps >15C and precip >

# Specify which responses you want for which variables
my.parameters <- formatFunctions(bio1 = c(fun = 'dnorm', mean = 250, sd = 50),
                                 bio12 = c(fun = 'dnorm', mean = 4000, sd = 2000))






