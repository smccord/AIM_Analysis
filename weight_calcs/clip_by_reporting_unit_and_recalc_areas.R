library(rgdal)
library(rgeos)
library(raster)
library(dplyr)

## Load the strata and reporting unit shapefiles
esd.shp <- "C:\\Users\\jasokarl\\Google Drive\\BLM_AIM\\Master_Sample\\Bruneau\\Ecosites_grouped_in_BFO_on_BLM.shp"
runits.shp <- "C:\\Users\\jasokarl\\Google Drive\\BLM_AIM\\Master_Sample\\Bruneau\\Bruneau_Allotments.shp"

esd.spdf <- readOGR(esd.shp,"Ecosites_grouped_in_BFO_on_BLM",stringsAsFactors = FALSE)
runits.spdf <- readOGR(runits.shp,"Bruneau_Allotments",stringsAsFactors = FALSE)


## Get the first reporting unit. Could easily set this up as an iterative loop.
runit <- runits.spdf[1,]

## Run intersect (from the raster package) to clip the strata to the reporting unit.
runit.esd <- intersect(esd.spdf,runit)

## Show the intersected layer
plot(runit.esd)

## Extract the areas associated with the intersected/clipped polygons and assign them to Acres and Hectares fields in the data frame
## Note that this step assumes that all you want for the output is a data frame with the stratum info and the areas of the polygons.
## If you need a SpatialPolygonsDataFrame, you can do the same thing, but use spCbind (from the maptools package) instead of 
## regular 'ol cbind for the last line.
areas <- data.frame(area=sapply(runit.esd@polygons,FUN=function(x){slot(x,"area")}))
areas$AcresNew <- areas$area/4046.86
areas$HectaresNew <- areas$area/10000.00
row.names(areas) <- sapply(runit.esd@polygons,FUN=function(x){slot(x,"ID")})
runit.df <- cbind(runit.esd@data,areas)

## Show the summaries of area by stratum
runit.df %>% group_by(ESD_Group) %>% summarise("Acres"=sum(AcresNew))
