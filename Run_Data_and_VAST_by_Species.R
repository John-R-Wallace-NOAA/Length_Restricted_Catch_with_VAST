
# Windows
   library(JRWToolBox)
   # setwd("W:/ALL_USR/JRW/Assessment/WCGBTS Juvenile Species Habitat")

# Linux
   library(JRWToolBox)
   Linux.First() # **** Answer prompts before moving on ****
  
JRWToolBox::gitAFile("John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/VAST.Length.Restricted.Catch.R", show = F)

spList <- list()
spList[[1]] <- list(SP = 'petrale sole', LenMaxAges = c(17, 21), LatMax = 48.3, LatMin = 33.8, DepMin = 50, DepMax = 200)  # Lat: 33.8 - 48.3; Near Mex. border
spList[[2]] <- list(SP = 'sablefish',  LenMaxAges = 29, LatMax = 48.3, LatMin = 33.8, DepMin = 50, DepMax = 475) # Lat: 33.8 - 48.3; Near Mex. border
spList[[3]] <- list(SP = 'Dover sole',  LenMaxAges = c(12, 17), LatMax = 47.5, LatMin = 32.5, DepMin = 50, DepMax = 465, Extra.Group.Size = c(1, 2, 2)) # Lat: 32.5 - 47.5; Near Mex. border
spList[[4]] <- list(SP = 'English sole',  LenMaxAges = 16, LatMax = 46.0, LatMin = 33.0, DepMin = 50, DepMax = 140) # Lat: 33.0 - 46.0; Near Mex. border
spList[[5]] <- list(SP = 'Pacific sanddab',  LenMaxAges = c(8, 13), LatMax = 48.2, LatMin = 32.5, DepMin = 50, DepMax = 245) # Lat: 32.5 - 48.2; Near Mex. border
spList[[6]] <- list(SP = 'arrowtooth flounder',  LenMaxAges = 22, LatMax = 48.5, LatMin = 38, DepMin = 50, DepMax = 470, Extra.Group.Size = c(1, 5)) # Lat: 38 - 48.5; North of San Fran.
spList[[7]] <- list(SP = 'Pacific hake',  LenMaxAges = c(15, 26), LatMax = 48.5, LatMin = 32.5, DepMin = 50, DepMax = 700, Extra.Group.Size = c(1, 2, 2)) # Entire Coast
spList[[8]] <- list(SP = 'lingcod',  LenMaxAges = 25, LatMax = 47.4, LatMin = 33.5, DepMin = 50, DepMax = 240, Extra.Group.Size = c(1, 8)) # Lat: 32.5 - 48.5; Near Mex. border
spList[[9]] <- list(SP = 'Pacific grenadier',  LenMaxAges = c(1, 2, 3), LatMax = 48.5, LatMin = 33.0, DepMin = 490, DepMax = 1275, Extra.Group.Size = 25) # Lat: 33.0 - 48.5; Near Mex. border
spList[[10]] <- list(SP = 'shortspine thornyhead',  LenMaxAges = c(6, 7, 8), LatMax = 48.3, LatMin = 33.8, DepMin = 160, DepMax = 625, Extra.Group.Size = 20) # Lat: 33.8 - 48.3; Near Mex. border
spList[[11]] <- list(SP = 'longspine thornyhead',  LenMaxAges = c(5, 6, 7), LatMax = 48.2, LatMin = 32.5, DepMin = 385, DepMax = 1245, Extra.Group.Size = 20) # Lat: 32.5 - 48.2; Near Mex. border
spList[[12]] <- list(SP = 'darkblotched rockfish',  LenMaxAges = c(9, 15), LatMax = 48.5, LatMin = 37.5, DepMin = 80, DepMax = 240) # Lat: 37.8 - 48.5; North of San Fran.
spList[[13]] <- list(SP = 'splitnose rockfish',  LenMaxAges = c(5, 10), LatMax = 48.5, LatMin = 32.5, DepMin = 65, DepMax = 460) # # Lat: 32.5 - 48.5; Near Mex. border

spList


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
  
