library(xlsx)
library(dplyr)
library(rgdal)
library(tidyr)

## TODO: Handle missing minimum required percentage of landscape values. If there's nothing, just be cool/substitute 0?
## TODO: Make sure that it only deals with rows where indicators are valid/selected and the top and bottom evalutions have been set
## TODO: Flag plots that don't end up with classifications?

## Get TerrADat imported
terradat.terrestrial.spdf <- readOGR(dsn = "C:/Users/nstauffe/Documents/Projects/LandscapeToolbox-NS/AIM_Reference/Terradat_data_8.17.15_complete.gdb", layer = "SV_IND_TERRESTRIALAIM", stringsAsFactors = F)
terradat.remote.spdf <- readOGR(dsn = "C:/Users/nstauffe/Documents/Projects/LandscapeToolbox-NS/AIM_Reference/Terradat_data_8.17.15_complete.gdb", layer = "SV_IND_REMOTESENSING", stringsAsFactors = F)
terradat.prj <- proj4string(terradat.terrestrial.spdf)
tdat <- merge(terradat.terrestrial.spdf@data, terradat.remote.spdf@data)

indicator.lut <- read.csv("C:/Users/nstauffe/Documents/Projects/AIM_Analysis/tdat_indicator_lut.csv", stringsAsFactors = F)

## Import the spreadsheet from the workbook. Should work regardless of presence/absence of other spreadsheets as long as the name is the same
benchmarks.raw <- read.xlsx(file = "C:/Users/nstauffe/Documents/Projects/AIM_Analysis/TerrestrialAIM_DataAnalysis_Template.xlsx",
                    sheetName = "Monitoring Objectives",
                    header = T,
                    stringsAsFactors = F)

## Strip out the extraneous columns and rows, which includes if they left the example in there
benchmarks <- benchmarks.raw[!grepl(x = benchmarks.raw$Management.Question, pattern = "^[Ee].g.") & !is.na(benchmarks.raw$Indicator), 1:11]

## Create the two evaluations. The way the spreadsheet is configured, there should be no rows without both defined
benchmarks$eval.string.lower <- paste0(benchmarks$Lower.Limit, benchmarks$LL.Relation)
benchmarks$eval.string.upper <- paste0(benchmarks$UL.Relation, benchmarks$Upper.Limit)

## Reorder the data frame
benchmarks <- merge(x = benchmarks, y = indicator.lut, by.x = "Indicator", by.y = "indicator.name")

## Slicing the data frame from terradat.spdf
## This step depends on how you're attributing the points with evaluation strata.
## If they aren't constrained somehow to the project you intend, then something along these lines needs to happen
tdat <- tdat[grepl(x = tdat$ProjectName, pattern = "rgdnnm", ignore.case = T),]

## To properly assign, see eval_stratum_attribution_functions.R
tdat$Evaluation.Stratum <- "Loamy"

## Strip out the ones without evaluation strata
tdat <- tdat[!is.na(tdat$Evaluation.Stratum),]

## Making a tall version of the data frame
## Indicators listed in order of appearance in TerrADat, line breaks inserted at thematic breaks
tdat.tall <- gather(tdat, Indicator, Value,
                    ## Terrestrial AIM values first
                    GapPct_25_50,GapPct_51_100,GapPct_101_200,GapPct_200_plus,GapPct_25_plus,
                    BareSoilCover_FH,TotalFoliarCover_FH,
                    NonInvPerenForbCover_AH,NonInvAnnForbCover_AH,NonInvPerenGrassCover_AH,NonInvAnnGrassCover_AH,NonInvAnnForbGrassCover_AH,NonInvPerenForbGrassCover_AH,
                    NonInvSucculentCover_AH,NonInvShrubCover_AH,NonInvSubShrubCover_AH,NonInvTreeCover_AH,
                    InvPerenForbCover_AH,InvAnnForbCover_AH,InvPerenGrassCover_AH,InvAnnGrassCover_AH,InvAnnForbGrassCover_AH,InvPerenForbGrassCover_AH,
                    InvSucculentCover_AH,InvShrubCover_AH,InvSubShrubCover_AH,InvTreeCover_AH,
                    SagebrushCover_AH,
                    WoodyHgt_Avg,HerbaceousHgt_Avg,SagebrushHgt_Avg,OtherShrubHgt_Avg,
                    NonInvPerenGrassHgt_Avg,InvPerenGrassHgt_Avg,
                    InvPlantCover_AH,
                    InvPlant_NumSp,
                    SoilStability_All,SoilStability_Protected,SoilStability_Unprotected,
                    ## Remote sensing values
                    HerbLitterCover_FH,WoodyLitterCover_FH,EmbLitterCover_FH,TotalLitterCover_FH,
                    RockCover_FH,BiologicalCrustCover_FH,VagrLichenCover_FH,LichenMossCover_FH,
                    DepSoilCover_FH,WaterCover_FH,
                    NonInvPerenForbCover_FH,NonInvAnnForbCover_FH,NonInvPerenGrassCover_FH,NonInvAnnGrassCover_FH,
                    NonInvSucculentCover_FH,NonInvShrubCover_FH,NonInvSubShrubCover_FH,NonInvTreeCover_FH,
                    InvPerenForbCover_FH,InvAnnForbCover_FH,InvPerenGrassCover_FH,InvAnnGrassCover_FH,
                    InvSucculentCover_FH,InvShrubCover_FH,InvSubShrubCover_FH,InvTreeCover_FH,
                    SageBrushCover_FH)

## Merge the tall TerrADat with the benchmark information
tdat.tall.benched <- merge(x = tdat.tall, y = benchmarks[, c("Evaluation.Stratum", "indicator.tdat", "Classification", "eval.string.lower", "eval.string.upper")], by.x = c("Evaluation.Stratum", "Indicator"), by.y = c("Evaluation.Stratum", "indicator.tdat"))

tdat.tall.benched$eval.string.lower <- paste0(tdat.tall.benched$eval.string.lower, tdat.tall.benched$Value)
tdat.tall.benched$eval.string.upper <- paste0(tdat.tall.benched$Value, tdat.tall.benched$eval.string.upper)

## To put into my lapply()
parser <- function(string){
  return(eval(parse(text = string)))
}

## Falling within the upper and lower bounds?
tdat.tall.benched$meeting <- lapply(tdat.tall.benched$eval.string.lower, parser) %>% unlist() & lapply(tdat.tall.benched$eval.string.upper, parser) %>% unlist()

## Because all the benchmark classifications should be mutually exclusive, applying the vector from meeting should result in one row per indicator per plot
output <- tdat.tall.benched[tdat.tall.benched$meeting, c("PrimaryKey", "PlotID", "Evaluation.Stratum", "Indicator", "Value", "Classification")]

