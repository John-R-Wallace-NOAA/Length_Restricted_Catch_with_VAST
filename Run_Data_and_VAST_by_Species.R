
# Windows
   library(JRWToolBox)
   # setwd("W:/ALL_USR/JRW/Assessment/WCGBTS Juvenile Species Habitat")

# Linux
   library(JRWToolBox)
   Linux.First() # **** Answer prompts before moving on ****
  
   
              
# Now with 'LM'            
spList <- list()
spList[[1]] <- list(SP = 'petrale sole', LenMaxAges = c(17, 21), LatMax = 48.3, LatMin = 33.8, DepMin = 50, DepMax = 200, LM = 21)  # Lat: 33.8 - 48.3; Near Mex. border
spList[[2]] <- list(SP = 'sablefish',  LenMaxAges = 29, LatMax = 48.3, LatMin = 33.8, DepMin = 50, DepMax = 475, LM = 29) # Lat: 33.8 - 48.3; Near Mex. border
spList[[3]] <- list(SP = 'Dover sole',  LenMaxAges = c(12, 17), LatMax = 47.5, LatMin = 32.5, DepMin = 50, DepMax = 465, Extra.Group.Size = c(1, 2, 2), LM = 17) # Lat: 32.5 - 47.5; Near Mex. border
spList[[4]] <- list(SP = 'English sole',  LenMaxAges = 16, LatMax = 46.0, LatMin = 33.0, DepMin = 50, DepMax = 140, LM = 16) # Lat: 33.0 - 46.0; Near Mex. border
spList[[5]] <- list(SP = 'Pacific sanddab',  LenMaxAges = c(8, 13), LatMax = 48.2, LatMin = 32.5, DepMin = 50, DepMax = 245, LM = 13) # Lat: 32.5 - 48.2; Near Mex. border
spList[[6]] <- list(SP = 'arrowtooth flounder',  LenMaxAges = 22, LatMax = 48.5, LatMin = 38, DepMin = 50, DepMax = 470, Extra.Group.Size = c(1, 5), LM = 22) # Lat: 38 - 48.5; North of San Fran.
spList[[7]] <- list(SP = 'Pacific hake',  LenMaxAges = c(15, 26), LatMax = 48.5, LatMin = 32.5, DepMin = 50, DepMax = 700, Extra.Group.Size = c(1, 2, 2), LM = 26) # Entire Coast
spList[[8]] <- list(SP = 'lingcod',  LenMaxAges = 25, LatMax = 47.4, LatMin = 33.5, DepMin = 50, DepMax = 240, Extra.Group.Size = c(1, 8), LM = 25) # Lat: 32.5 - 48.5; Near Mex. border
spList[[9]] <- list(SP = 'Pacific grenadier',  LenMaxAges = c(1, 2, 3), LatMax = 48.5, LatMin = 33.0, DepMin = 490, DepMax = 1275, Extra.Group.Size = 25, LM = 3) # Lat: 33.0 - 48.5; Near Mex. border
spList[[10]] <- list(SP = 'shortspine thornyhead',  LenMaxAges = c(6, 7, 8), LatMax = 48.3, LatMin = 33.8, DepMin = 160, DepMax = 625, Extra.Group.Size = 20, LM = 8) # Lat: 33.8 - 48.3; Near Mex. border
spList[[11]] <- list(SP = 'longspine thornyhead',  LenMaxAges = c(5, 6, 7), LatMax = 48.2, LatMin = 32.5, DepMin = 385, DepMax = 1245, Extra.Group.Size = 20, LM = 7) # Lat: 32.5 - 48.2; Near Mex. border
spList[[12]] <- list(SP = 'darkblotched rockfish',  LenMaxAges = c(9, 15), LatMax = 48.5, LatMin = 37.5, DepMin = 80, DepMax = 240, LM = 15) # Lat: 37.8 - 48.5; North of San Fran.
spList[[13]] <- list(SP = 'splitnose rockfish',  LenMaxAges = c(5, 10), LatMax = 48.5, LatMin = 32.5, DepMin = 65, DepMax = 460, LM = 10) # # Lat: 32.5 - 48.5; Near Mex. border

# spList

# Now have 2018 data, so no need for: dupYears = 2012 (until early in 2021)

DATA <- c(TRUE, FALSE)[2]

# Rho: Structure for beta (only) over time: 0=None (default); 1=WhiteNoise; 2=RandomWalk; 3=Constant intercept; 4=Autoregressive, each year as random following AR1 process
 for ( j in c(0,3)) {  # j is rho
   for ( i in 1:13) {  # i is species
   
     if(DATA)
       VAST.Length.Restricted.Catch( spList[[i]]$SP, Top.Prct = 15, Top.Years = 6, warehouseDownload = TRUE, LenMaxAges = spList[[i]]$LenMaxAges, RawDataPlots = TRUE, runVAST = FALSE, 
            LatMax = spList[[i]]$LatMax,  LatMin = spList[[i]]$LatMin , DepMin = spList[[i]]$DepMin, DepMax = spList[[i]]$DepMax)
        
     else
        try(VAST.Length.Restricted.Catch( spList[[i]]$SP, Top.Prct = 15, Top.Years = 6, warehouseDownload = FALSE, LenMaxAges = spList[[i]]$LenMaxAges, LatMax = spList[[i]]$LatMax,
               LatMin = spList[[i]]$LatMin , DepMin = spList[[i]]$DepMin, DepMax = spList[[i]]$DepMax, RawDataPlots = FALSE, runVAST = TRUE, runDiagnostics = TRUE, allAgesBubble = FALSE, 
               rhoConfig = j, Extra.Group.Size = spList[[i]]$Extra.Group.Size))
   }
 
}
   

# See page 10 of the VAST manual for information on rho options:
 
JRWToolBox::gitAFile("https://github.com/James-Thorson/VAST/blob/master/manual/VAST_model_structure.pdf", "pdf")
  

  
# ===================== Redo Final Figures =============================================

# ---------- Prelimininary Steps ---------  
  
JRWToolBox::gitAFile("John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/VAST.Length.Restricted.Catch.R", show = F)
gitEdit(VAST.Length.Restricted.Catch, 'John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/')
gitEdit(YearlyResultsFigures)  # From JRWToolBox
gitEdit(plotGIS, "John-R-Wallace-NOAA/Imap/master/R/") # From Imap


HomeDir = "."
(DateFile <- paste0(HomeDir, Sys.Date(), '_', spShortName, '_DV_', DataSource, '_LM', LenMax, '_', substring(Version, 6), '_Rho=', rhoConfig, '_AS_nx=', n_x, '/'))
dir.create(DateFile, recursive = TRUE)

# PNG
YearlyResultsFigures(spLongName. = spLongName, spShortName. = spShortName, longitudeDelta = 2.6, Index = IndexTable, SP.Results.Dpth = SP.Results.Dpth, 
              MapDetails_List = MapDetails_List, Report = Report, Opt = Opt, DateFile = DateFile, Year_Set = Year_Set, Years2Include = Years2Include, Ages = Ages, LenMin = LenMin, LenMax = LenMax, 
              strata.limits = strata.limits, HomeDir = HomeDir, title = TRUE, Graph.Dev = "png")
# TIFF
YearlyResultsFigures(spLongName. = spLongName, spShortName. = spShortName, longitudeDelta = 2.6, Index = IndexTable, SP.Results.Dpth = SP.Results.Dpth, 
              MapDetails_List = MapDetails_List, Report = Report, Opt = Opt, DateFile = DateFile, Year_Set = Year_Set, Years2Include = Years2Include, Ages = Ages, LenMin = LenMin, LenMax = LenMax, 
              strata.limits = strata.limits, HomeDir = HomeDir, title = TRUE, Graph.Dev = "tiff")
# PDF
YearlyResultsFigures(spLongName. = spLongName, spShortName. = spShortName, longitudeDelta = 2.6, Index = IndexTable, SP.Results.Dpth = SP.Results.Dpth, 
              MapDetails_List = MapDetails_List, Report = Report, Opt = Opt, DateFile = DateFile, Year_Set = Year_Set, Years2Include = Years2Include, Ages = Ages, LenMin = LenMin, LenMax = LenMax, 
              strata.limits = strata.limits, HomeDir = HomeDir, title = TRUE, Graph.Dev = "pdf")
            
              
N <- length(Year_Set)
longitudeDelta = 2.6
eastLongitude = -124 - (N + 1) * longitudeDelta 

latExtend <- ifelse(N > 13, -((-125 - (N + 1) * 3.5 + 117) - (-125 - 14 * 3.5 + 117))/3, 0)
   
imap(longlat = list(Imap::world.h.land, Imap::world.h.borders), col= c("black", "cyan"), poly = c("grey40", NA), longrange = c(eastLongitude, -117), latrange = c(27 - latExtend, 48.2), 
         axes = 'latOnly', zoom = FALSE, bg = "white", cex.ylab = 2, cex.axis = 2, lwd.ticks = 2)
box(lwd = 5)
 

 
# ---------------  Main Loop -------------------------------------------------

 
baseLoad <- " "
# source("W:\\ALL_USR\\JRW\\R.Vanilla\\imap.R")
# source("W:\\ALL_USR\\JRW\\R.Vanilla\\imap.ll.R")
# source("W:\\ALL_USR\\JRW\\R.Vanilla\\YearlyResultsFigures.R")
    


for ( jj in 1:13) {
   

    print(spList[[jj]]$SP)
    
    if(exists('Ages')) rm(Ages) 
    
    if( jj %in%  2:5)
       load(paste0("\\\\nwctantalus.nmfs.local\\jwallace\\h_jwallace\\WCGBTS_JV_3\\2019-03-07_", as.character(Spec.code.f(spList[[jj]]$SP, char=TRUE)[4]), "_DV_WCGBTS_LM", spList[[jj]]$LM, "_v5_5_0_Rho=3_AS_nx=600\\Image.RData"))
    
    if( jj %in%  c(1, 6:9, 11:13))
       load(paste0("\\\\nwctantalus.nmfs.local\\jwallace\\h_jwallace\\WCGBTS_JV_3\\2019-03-08_", as.character(Spec.code.f(spList[[jj]]$SP, char=TRUE)[4]), "_DV_WCGBTS_LM", spList[[jj]]$LM, "_v5_5_0_Rho=3_AS_nx=600\\Image.RData"))
    
    if( jj %in%  10)
       load(paste0("\\\\nwctantalus.nmfs.local\\jwallace\\h_jwallace\\WCGBTS_JV_3\\2019-03-11_", as.character(Spec.code.f(spList[[jj]]$SP, char=TRUE)[4]), "_DV_WCGBTS_LM", spList[[jj]]$LM, "_v5_5_0_Rho=3_AS_nx=600\\Image.RData"))
    
        
    HomeDir = "."
    print(DateFile <- paste0(Sys.Date(), '_', spShortName, '_DV_', DataSource, '_LM', LenMax, '_', substring(Version, 6), '_Rho=', rhoConfig, '_AS_nx=', n_x, '/'))
    dir.create(DateFile, recursive = TRUE)

    if(exists('Ages')) cat("\nAges = ", Ages, "\n")
    
    
    # YearlyResultsFigures(spLongName. = spLongName, spShortName. = spShortName, longitudeDelta = 2.6, Index = IndexTable, SP.Results.Dpth = SP.Results.Dpth, 
    #         MapDetails_List = MapDetails_List, Report = Report, Opt = Opt, DateFile = DateFile, Year_Set = Year_Set, Years2Include = Years2Include, Ages = Ages, LenMin = LenMin, LenMax = LenMax, 
    #         strata.limits = strata.limits, HomeDir = HomeDir, title = TRUE, Graph.Dev = "tiff")
    
    # Redo creating SP.Results.Dpth
    # SP.Results.Dpth.OLD <- SP.Results.Dpth
    rm(SP.Results.Dpth)
    SP.Results.Dpth <- JRWToolBox::YearlyResultsFigures(spLongName. = spLongName, spShortName. = spShortName, longitudeDelta = 2.6, Index = IndexTable, SP.Results.Dpth = NULL, 
              MapDetails_List = MapDetails_List, Report = Report, Opt = Opt, DateFile = DateFile, Year_Set = Year_Set, Years2Include = Years2Include, Ages = Ages, LenMin = LenMin, LenMax = LenMax, 
              strata.limits = strata.limits, HomeDir = HomeDir, title = TRUE, Graph.Dev = "tiff")
              
    cat("\nDone:", spList[[jj]]$SP, "\n\n\n")         
   
 }   
    
    
 
