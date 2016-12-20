## Brief script to run cat.analysis on some test data.
## Only utility of this script may be in how the input data frame is subset for the cat.analysis run.

library(spsurvey)
library(dplyr)

infile <- "C:\\Users\\jasokarl\\Google Drive\\Landscape Toolbox\\LandscapeToolbox_GitHub\\AIM_Analysis\\analysis\\test_input_evaluated.csv"
outfile <- "C:\\Users\\jasokarl\\Google Drive\\Landscape Toolbox\\LandscapeToolbox_GitHub\\AIM_Analysis\\analysis\\analysis_output.csv"

data <- read.csv(infile, header=T)

names(data)
'''[1] "ï..OBJECTID.."       "Shape.."             "SiteID"              "ProjectName"         "EcologicalSiteId"    "PlotID"             
[7] "PlotKey"             "Weight"              "Latitude"            "Longitude"           "ReportingUnit.1"     "ReportingUnit.2"    
[13] "DateEstablished"     "DateVisited"         "HerbLitterCover_FH"  "HerbLitter.Eval"     "RockCover_FH"        "RockCover.Eval"     
[19] "TotalFoliarCover_FH" "TotalFoliar.Eval"    "GapPct_200_plus"     "Gap200.Eval"  
'''
`

## The only potentially confusing part here is that cat.analysis wants a "siteID" which is analagous to the PlotID in TerrADat or DIMA
##    Need to be careful because TerrADat/DIMA also has a field called SiteID, but that is different.
aim.sites <- data.frame(siteID=data$PlotID,Active=TRUE)
aim.subpop <- data[,c("PlotID","ReportingUnit.1","ReportingUnit.2")]
names(aim.subpop)[1] <- "siteID"
aim.design <- data[,c("PlotID","Weight","Longitude","Latitude")]
names(aim.design) <- c("siteID","wgt","xcoord","ycoord")
aim.datacat <- data[,c(6,16,18,20,22)]  ## There's a more elegant/programmatic way to get these columns.
names(aim.datacat)[1] <- "siteID"

ru1.areas <- data %>% group_by(ReportingUnit.1) %>% summarise(area=sum(Weight))
ru2.areas <- data %>% group_by(ReportingUnit.2) %>% summarise(area=sum(Weight))

## for expediency, I'm just going to manually create this population size list. Need to automate, though...
## This example is for unstratified sampling (also for simplicity) for stratified, need to add the stratum field to the design data frame
##    and add the stratum areas to the popsize list
aim.popsize = list("ReportingUnit.1"=767543,
               "ReportingUnit.2"=list("One"=750,
                            "Two"=500))


### Now run cat.analysis
aim.analysis <- cat.analysis(sites = aim.sites, subpop = aim.subpop, design = aim.design, data.cat = aim.datacat, popsize = aim.popsize)


### Export the result
write.csv(aim.analysis,outfile)
