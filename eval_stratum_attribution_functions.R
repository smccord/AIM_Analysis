#####################################################################
#### ATTRIBUTING POINTS WITH EVALUATION/BENCHMARK STRATA ####
## Functions for adding a column/field called Evaluation.Stratum to points
## for use in benchmark evaluations

## attribute.shapefile() adds from a specified field in a shapefile, either points or polygons
## attribute.list() adds based on a simple lookup table of PlotID and Evaluation.Stratum
## attribute.field() adds from a lookup table based on values found in the fields of the points fed to it
#####################################################################



## TODO: Function to make sure that no plot has received more than one evaluation stratum if multiple attribution functions were used

## TODO: Shapefile attribute extraction function where the shapefile attribute table contains the evaluation stratum (possibly strata)
attribute.shapefile <- function(points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
                                datapath = "", ## If the shape is in a .gdb feature class then this should be the full path, including the file extension .gdb. If the SPDF is already made, do not specify this argument
                                shape = "", ## The name of the shapefile or feature class !!!OR!!! an SPDF
                                attributefield = "", ## Name of the field in the shape that specifies the evaluation stratum
                                projection = CRS("+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs")){
  ## Strip the file extension from shape, just in case it was there
  shape <- str_replace(shape, pattern = "\\.[Ss][Hh][Pp]$", replacement = "")
  ## If this is coming from a geodatabase, extract the shapefile appropriately. Otherwise read in the .shp
  if (grepl(x = datapath, pattern = "\\.[Gg][Dd][Bb]$")) {
    shape.spdf <- readOGR(dsn = datapath, layer = shape, stringsAsFactors = F) %>% spTransform(projection)
  } else if (datapath != "") {
    shape.spdf <- readOGR(dsn = paste0(datapath, "/", shape, ".shp"), layer = shape, stringsAsFactors = F) %>% spTransform(projection)
  } else if (class(shape)[1] == "SpatialPointsDataFrame" | class(shape)[1] == "SpatialPolygonsDataFrame") {
    shape.spdf <- shape
  }
  if (points@proj4string@projargs != shape.spdf@proj4string@projargs) {
    shape.spdf %>% spTransform(points@proj4string)
  }
  points.evalstrata <- over(points, shapefile)[, attributefield]
  names(points.evalstrata)[names(points.evalstrata) == attributefield] <- "Evaluation.Stratum"
  return(cbind(points, points.evalstrata))
}

## TODO: Function to import .csv or .xlsx that attributes the plots matching the PLOT column with evaluation strata from the EVAL.STRATUM column
attribute.list <- function(points = points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
                           datapath = "", ## Only specify if you need to read in the lookup table from a file
                           lut = "" ## Either the filename !!!OR!!! a data frame. Either way it needs a PlotID column and an Evaluation.Stratum column
                           ){
  ## Sanitize the input
  datapath <- str_replace(datapath, pattern =  "/$", replacement = "")
  if ((datapath == "" | is.null) & is.data.frame(lut)) {
    lut <- lut[, c("PlotID", "Evaluation.Stratum")]
  } else if (!is.data.frame(lut) & grepl(x = lut, pattern = "\\.[Cc][Ss][Vv]$")) {
    lut <- read.csv(paste0(datapath, "/", lut), stringsAsFactors = F)[, c("PlotID", "Evaluation.Stratum")]
  }
  return(merge(points, lut))
}

## TODO: Function to import .csv or .xlsx to function as a lookup table with columns for TerrADat/MS field, field values, and evaluation strata
attribute.field <- function(points = points = SpatialPointsDataFrame( coords = matrix(1:2,1:2), data = data.frame(matrix(1:2,1:2))),
                            datapath = "", ## Only specify if you need to read in the lookup table from a file
                            lut = "" ## Either the filename !!!OR!!! a data frame. Either way it needs the columns Attribute.Field, Field.Value, Evaluation.Stratum
                            ){
  ## Sanitize the input
  datapath <- str_replace(datapath, pattern =  "/$", replacement = "")
  if ((datapath == "" | is.null) & is.data.frame(lut)) {
    lut <- lut[, c("Attribute.Field", "Field.Value", "Evaluation.Stratum")]
  } else if (!is.data.frame(lut) & grepl(x = lut, pattern = "\\.[Cc][Ss][Vv]$")) {
    lut <- read.csv(paste0(datapath, "/", lut), stringsAsFactors = F)[, c("Attribute.Field", "Field.Value", "Evaluation.Stratum")]
  }
  for (n in 1:nrow(lut)) {
    points$Evaluation.Stratum[points[, lut$Attribute.Field[n]] == lut$Attribute.Value[n]] <- lut$Evaluation.Stratum[n]
  }
  return(points)
}