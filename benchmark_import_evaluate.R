library(xlsx)
library(dplyr)
library(rgdal)

## TODO: Bring in the relevant TerrADat data and call it tdat
## TODO: Make sure tdat gets assigned a column for the reporting stratum (assumed in this code to be called Reporting.Stratum)

## Get TerrADat imported
terradat.spdf <- readOGR(dsn = "C:/Users/nstauffe/Documents/Projects/LandscapeToolbox-NS/AIM_Reference/Terradat_data_8.17.15_complete.gdb", layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = F)
terradat.prj <- proj4string(terradat.spdf)
tdat <- terradat.spdf@data

indicator.lut <- read.csv("C:/Users/nstauffe/Documents/Projects/LandscapeToolbox-NS/tdat_indicator_lut.csv", stringsAsFactors = F)

## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
benchmarks.raw <- read.xlsx(file = "C:/Users/nstauffe/Downloads/TerrestrialAIM_DataAnalysis_Template.xlsx",
                    sheetName = "Monitoring Objectives",
                    header = F,
                    stringsAsFactors = F)

## Strip out the extraneous columns and rows
benchmarks <- benchmarks.raw[3:nrow(benchmarks.raw), 1:10]

## Rename the remaining columns
names(benchmarks) <- c("Management.Question", "Condition.Source", "Reporting.Stratum", "Lower.Limit.Value", "Lower.Limit.Operator", "Indicator.Name", "Upper.Limit.Operator", "Upper.Limit.Value", "Units", "Minimum.Proportion")

## Keep only rows where an indicator was specified and it wasn't the example row included in the spreadsheet to guide users
benchmarks <- benchmarks[!grepl(x = benchmarks$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks$Indicator),]

## Create the two evaluations. The way the spreadsheet is configured, there should be no rows without both defined
benchmarks$eval.string.lower <- paste0(benchmarks$Lower.Limit.Value, benchmarks$Lower.Limit.Operator)
benchmarks$eval.string.upper <- paste0(benchmarks$Upper.Limit.Operator, benchmarks$Upper.Limit.Value)

## Reorder the data frame
benchmarks <- merge(x = benchmarks, y = indicator.lut, by.x = "Indicator.Name", by.y = "indicator.name") %>%
  .[,c("Management.Question", "Condition.Source", "Reporting.Stratum", "Lower.Limit.Value", "Lower.Limit.Operator", "Indicator.Name", "Upper.Limit.Operator", "Upper.Limit.Value", "Units", "Minimum.Proportion", "eval.string.lower", "eval.string.upper", "indicator.tdat")]

## Slicing the data frame from terradat.spdf
tdat <- terradat.spdf@data[grepl(x = terradat.spdf@data$ProjectName, pattern = "rgdnnm", ignore.case = T),]

## TODO: Proper assignment of reporting stratum identities
tdat$Reporting.Stratum <- "Test"

## Making a tall version of the data frame
tdat.tall <- gather(tdat, Indicator, Value,
                    GapPct_25_50,GapPct_51_100,GapPct_101_200,GapPct_200_plus,GapPct_25_plus,BareSoilCover_FH,TotalFoliarCover_FH,NonInvPerenForbCover_AH,NonInvAnnForbCover_AH,NonInvPerenGrassCover_AH,NonInvAnnGrassCover_AH,NonInvAnnForbGrassCover_AH,NonInvPerenForbGrassCover_AH,NonInvSucculentCover_AH,NonInvShrubCover_AH,NonInvSubShrubCover_AH,NonInvTreeCover_AH,InvPerenForbCover_AH,InvAnnForbCover_AH,InvPerenGrassCover_AH,InvAnnGrassCover_AH,InvAnnForbGrassCover_AH,InvPerenForbGrassCover_AH,InvSucculentCover_AH,InvShrubCover_AH,InvSubShrubCover_AH,InvTreeCover_AH,SagebrushCover_AH,WoodyHgt_Avg,HerbaceousHgt_Avg,SagebrushHgt_Avg,OtherShrubHgt_Avg,NonInvPerenGrassHgt_Avg,InvPerenGrassHgt_Avg,InvPlantCover_AH,InvPlant_NumSp,SoilStability_All,SoilStability_Protected,SoilStability_Unprotected)

## Merge the tall TerrADat with the benchmark information
tdat.tall.benched <- merge(x = tdat.tall, y = benchmarks, by.x = c("Reporting.Stratum", "Indicator"), by.y = c("Reporting.Stratum", "indicator.tdat")) %>%
  .[!is.na(.["Lower.Limit.Value"]),]

tdat.tall.benched$eval.string.lower <- paste0(tdat.tall.benched$eval.string.lower, tdat.tall.benched$Value)
tdat.tall.benched$eval.string.upper <- paste0(tdat.tall.benched$Value, tdat.tall.benched$eval.string.upper)

## To put into my lapply()
parser <- function(string){
  return(eval(parse(text = string)))
}

## Use lapply to add columns for whether or not the given indicator met the upper and lower benchmark values
tdat.tall.benched$meeting.lower <- lapply(tdat.tall.benched$eval.string.lower, parser) %>% unlist()
tdat.tall.benched$meeting.upper <- lapply(tdat.tall.benched$eval.string.upper, parser) %>% unlist()
## Falling within the upper and lower bounds?
tdat.tall.benched$meeting <- tdat.tall.benched$meeting.lower & tdat.tall.benched$meeting.upper

