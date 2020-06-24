

# Yearly table of abundance index, effective area, and COG

# IndexTable <- SpatialDeltaGLMM::PlotIndex_Fn(DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=sort(unique(DatG$Year)), strata_names=strata.limits[,1], use_biascorr=TRUE,
#                                     width = 11, height = 8)$Table
# 
# RangeShifts <-  SpatialDeltaGLMM::Plot_range_shifts(Sdreport=Opt$SD, Report=Report, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile)
# COG.LongLat <- UTM.to.LatLong(1000*data.frame(Eastings = RangeShifts$COG_Table[1:13, 'COG_hat'], Northings = RangeShifts$COG_Table[14:26, 'COG_hat']))
# 
# 
# Year.Index.EffArea.COG <- data.frame(Year = IndexTable$Year, Biomass.Index.mt = IndexTable$Estimate_metric_tons, Biomass.Index.SE.mt = IndexTable$SD_mt,  EffectiveArea = RangeShifts$EffectiveArea_Table[, 'EffectiveArea'], EffectiveArea.SE = RangeShifts$EffectiveArea_Table[, 'SE'], 
#       COG.Eastings = RangeShifts$COG_Table[1:13, 'COG_hat'], COG.Eastings.SE = RangeShifts$COG_Table[1:13, 'SE'], COG.Northings = RangeShifts$COG_Table[14:26, 'COG_hat'], 
#       COG.Northings.SE = RangeShifts$COG_Table[14:26, 'SE'], COG.Long = COG.LongLat$Long, COG.Lat = COG.LongLat$Lat)
#  
# save(Year.Index.EffArea.COG, file=paste0(DateFile, "Year.Index.EffArea.COG.RData"))




# Year.Index.EffArea.COG is now saved at the bottom of: VAST.Length.Restricted.Catch.R and so can be found in the directory of each species run.


# For Nick

# devtools::install_github("John-R-Wallace/JRWToolBox")
library(JRWToolBox)

# JRWToolBox::lib("John-R-Wallace/Imap")

load('Year.Index.EffArea.COG.RData')


JRWToolBox::mapDataImapWC(Year.Index.EffArea.COG[,c("COG.Long", "COG.Lat")], Year.Index.EffArea.COG$Year)
 

change(Year.Index.EffArea.COG)
dev.new()
par(mfrow = c(2,2))
plot(Year, Biomass.Index.mt, type='o')
plot(Year, EffectiveArea, type='o')
plot(Year, COG.Long, type='o')
plot(Year, COG.Lat, type='o')

dev.new()
par(mfrow = c(3,2))
JRWToolBox::plot.lowess.lsfit(Biomass.Index.mt, EffectiveArea)
JRWToolBox::plot.lowess.lsfit(Biomass.Index.mt, COG.Long)
JRWToolBox::plot.lowess.lsfit(Biomass.Index.mt, COG.Lat)
JRWToolBox::plot.lowess.lsfit(EffectiveArea, COG.Long)
JRWToolBox::plot.lowess.lsfit(EffectiveArea, COG.Lat)
plot(COG.Long, COG.Lat)


# -------------------------------------------------------------------------------------------------------------------------------


mapDataImapWC <- function(XY, label = seq_along(XY[,1]), figureDelta = 0.2, labelDelta = 0.05) {
    N <- nrow(XY)
 '  '   
 '  # Just the data  '
    dev.new()
    plot(XY, xlim = c(min(XY[,1]) - figureDelta, max(XY[,1]) + figureDelta), ylim = c(min(XY[,2]) - figureDelta, max(XY[,2]) + figureDelta), type = 'o')
    text(XY + data.frame(rep(labelDelta, N), rep(0, N)), label = label)
 '  ' 
 '  # West Coast  '
    dev.new()
    imap(longrange = c(-130, -115), latrange = c(32, 48.5), zoom = FALSE)
    points(XY, type = 'o')
    text(XY + data.frame(rep(labelDelta, N), rep(0, N)), labels = label)
 '  '
 '  # Zoomed in'
    dev.new()
    imap(longrange = c(min(XY[,1]) - figureDelta, max(XY[,1]) + figureDelta), latrange = c(min(XY[,2]) - figureDelta, max(XY[,2]) + figureDelta), zoom=FALSE)
    points(XY, type = 'o')
    text(XY + data.frame(rep(labelDelta, N), rep(0, N)), label = label)
 
}

















