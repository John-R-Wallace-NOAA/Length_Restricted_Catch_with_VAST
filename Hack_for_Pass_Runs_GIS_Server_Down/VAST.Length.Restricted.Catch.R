
 
VAST.Length.Restricted.Catch <- function(spLongName = 'petrale sole', Species = NULL, spShortName = NULL, Top.Prct = 15, Top.Years = 6, warehouseDownload = TRUE, dupYears = NULL, 
     LenNum = if(is.null(LenMaxAges)) NULL else length(LenMaxAges), LenMaxAges = NULL, LenMax = max(LenMaxAges), LatMax = NULL, 
     LatMin = NULL, DepMin = 50.0, DepMax = NULL, Knots = 600, rhoConfig = 0, Threads = 3, AS = TRUE, DV = TRUE, Pass = FALSE, firstYear = 2003, lastYear = 3000, Bubble.Size = 0.005, badWts = NULL, RawDataPlots = TRUE,
     allAgesBubble = FALSE, runVAST = TRUE, runDiagnostics = runVAST, Extra.Group.Size = NULL, Data_List = NULL) {                                    

   # AS = "Area Swept"; DV = "Depth covariate" 
   # Use 3 or 4 threads for Windows on a 6 core PC and 5-12 threads for 50-1200 knots on Tantalus, which has 64 threads
   
   # ============================================== SETUP =================================================================
   
   oldOpt <- options(stringsAsFactors = FALSE)
   on.exit(options(oldOpt))
   
   if(!.Platform$OS.type == "windows") {
          options(width = 160) # Linux
          INLA:::inla.dynload.workaround()  # INLA fix on some older Linux system - does nothing on Windows
          options(repos=c(CRAN="http://cran.fhcrc.org", CRANextra = "http://www.stats.ox.ac.uk/pub/RWin")) # Change to CRAN repository away from the Revolution Analytics frozen mirror.
   }
      
   if (!any(installed.packages()[, 1] %in% "devtools")) 
           install.packages("devtools")
   if (!any(installed.packages()[, 1] %in% "RCurl")) 
            install.packages("RCurl")
   if(!any(installed.packages()[, 1] %in% "JRWToolBox")) 
           devtools::install_github("John-R-Wallace/JRWToolBox")
   require(JRWToolBox)
   # SHA('JRWToolBox')
   # devtools::install_github('John-R-Wallace/JRWToolBox', ref = '7ab2790284a0c75458eda651617fd029c2ff09cd')  # R 3.4.3; ; 2018-04-13 21:11:21 UTC; windows

   lib("John-R-Wallace/Imap", quiet = F)
   
   lib(numDeriv)
   # packageDescription('numDeriv')$Built #  "R 3.4.1; ; 2017-08-16 00:39:08 UTC; windows"
   
   lib(lattice)
   lib(TeachingDemos)
   
   
   # # # # Using CRAN version for now - GitHub version gives an error
   # # # # lib(TMB)  # CRAN - May be older on CRAN
   # # # # lib(TMB, type = 'source')  # May need a recompile if the Matrix library is older
   
   # Using GitHub version for now - GitHub version doesn't currently give an error
   # lib('kaskr/adcomp/TMB') # GitHub
   # SHA('TMB') # GitHub version working for now
   # devtools::install_github('kaskr/adcomp', ref = 'abbd69a1a684b4b8e442a4c73fbc41f3ba921540')  # R 3.5.1; x86_64-w64-mingw32; 2019-01-28 18:08:53 UTC; windows
     
   # lib('kaskr/TMB_contrib_R/TMBhelper')
   # SHA('TMBhelper')
   # devtools::install_github('kaskr/TMB_contrib_R', ref = '2071ec99dcb8df4dccd2866dea2528257c37bb4c')  # R 3.5.1; ; 2019-01-28 18:09:26 UTC; windows

   # # geostatistical_delta-GLMM should load with VAST
   # # lib('nwfsc-assess/geostatistical_delta-GLMM', 'SpatialDeltaGLMM')
   # # SHA('SpatialDeltaGLMM') #GithubSHA1: 774b0f6ff7e019c8c6e8f2d8909ac771dcdbf0bb
   # # devtools::install_github('nwfsc-assess/geostatistical_delta-GLMM', ref = '774b0f6ff7e019c8c6e8f2d8909ac771dcdbf0bb')  # R 3.4.3; ; 2018-03-29 20:02:34 UTC; windows
   
   # Jim's Utilities should load with VAST
   # lib('james-thorson/FishStatsUtils')
   # SHA('FishStatsUtils')
   # devtools::install_github('james-thorson/FishStatsUtils', ref = 'fe3582768bc1763bd171311261cca51498c9abc5')  # R 3.5.1; ; 2019-01-28 18:13:30 UTC; windows

   # lib('james-thorson/VAST')
   # SHA('VAST')
   # devtools::install_github('james-thorson/VAST', ref = '2d53ce07673c0f00bdafc2cc3d2383da0b534cf0')  # R 3.5.1; ; 2019-01-28 18:12:56 UTC; windows

   lib(maps)  # Installed by VAST, but need to be loaded for diagnostic plots
   lib(mapdata) # Installed by VAST, but need to be loaded for diagnostic plots
   require(TMB)
   require(sp)
   
   
   # ============================================== VAST Setup =================================================================
   
   Region <- "Other"  
   # Region <- "California_current"  
   DataSource <- 'WCGBTS'
     
   HomeDir <- paste0(getwd(), '/')
         
   # Version <- "VAST_v3_0_0" 
   Version <- "VAST_v5_5_0" 
   # Ansio figure very different and main index results slightly different with  VAST_v4_2_0
   # (Version <- substr(list.files(R.home(file.path("library", "VAST", "executables")))[length(list.files(R.home(file.path("library", "VAST", "executables"))))], 1, 11))
    
   Method <- "Mesh"
   n_x <- Knots # Number of stations/knots - for Mesh method
   grid_size_km = 25 # For Grid method (not used here)
   
   setMKLthreads(Threads)
   
   if(is.null(Species)) { Species <- as.character(Spec.code.f(spLongName, char=TRUE)[2]); cat('\n\nSpecies scientific name =', Species, "\n") }
   if(is.null(spShortName)) { spShortName <- as.character(Spec.code.f(spLongName, char=TRUE)[4]); cat('\nSpecies short name =', spShortName, "\n\n") }
   
   
   
  if(is.null(Data_List)) {
   
   # ================================= WCGBTS data from the Warehouse ================================================
    
   if(warehouseDownload) { 
       SP.BD.NWFSC <- dataWareHouseTrawlBio(species = Species, yearRange = c(firstYear, lastYear), projectShort = 'WCGBTS.Combo', verbose = TRUE)
       SP.BD.NWFSC$KEY <- paste(SP.BD.NWFSC$Year, SP.BD.NWFSC$Vessel, SP.BD.NWFSC$Tow)
       
       SP.Catch.NWFSC <- dataWareHouseTrawlCatch(species = Species, yearRange = c(firstYear, lastYear), projectShort = 'WCGBTS.Combo', verbose = TRUE)
       SP.Catch.NWFSC$KEY <- paste(SP.Catch.NWFSC$Year, SP.Catch.NWFSC$Vessel, SP.Catch.NWFSC$Tow)
       
       SP.BD.NWFSC <- match.f( SP.BD.NWFSC, SP.Catch.NWFSC, "KEY", "KEY", c("Total_sp_wt_kg", "Area_Swept_ha"))
       print(SP.BD.NWFSC[1:4, ])
       # save(SP.BD.NWFSC, SP.Catch.NWFSC, file = paste0(HomeDir, spShortName, '.BD.Catch.NWFSC.dmp'))
       
       if(!is.null(dupYears))  {
          for ( i in length(dupYears)) {
             SP.Catch.NWFSC.DUP <- SP.Catch.NWFSC[SP.Catch.NWFSC$Year %in% dupYears[i], ]
             SP.Catch.NWFSC.DUP$Year <- max(SP.Catch.NWFSC$Year) + 1
             delta <- rnorm(nrow(SP.Catch.NWFSC.DUP), sd = sd(SP.Catch.NWFSC.DUP$Total_sp_wt_kg))
             SP.Catch.NWFSC.DUP$Total_sp_wt_kg <- SP.Catch.NWFSC.DUP$Total_sp_wt_kg + delta
             SP.Catch.NWFSC.DUP$Total_sp_wt_kg <- SP.Catch.NWFSC.DUP$Total_sp_wt_kg - min(SP.Catch.NWFSC.DUP$Total_sp_wt_kg)
             SP.Catch.NWFSC <- rbind(SP.Catch.NWFSC, SP.Catch.NWFSC.DUP)
          } 
          cat("\n\nYears were duplicated, the year range is now", min(SP.BD.NWFSC$Year), "to", max(SP.BD.NWFSC$Year), "\n\n")
       }       
    } else
        base::load(paste0(HomeDir, spShortName, '.BD.Catch.NWFSC.dmp'), envir = sys.frame(sys.nframe()))
        
    if(any(SP.Catch.NWFSC$Total_sp_wt_kg < 0)) stop("Species catch weight is less than zero!")
   
    # ============== Enter length maximums (LenMax)) based on age - the largest of the maximums will be used in VAST unless runVAST = FALSE =================
       
    if(is.null(LenNum))  {
        print(Table(SP.BD.NWFSC$Age[SP.BD.NWFSC$Age %in% 0:10], SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Age %in% 0:10]))
        print(Table(SP.BD.NWFSC$Age[SP.BD.NWFSC$Age %in% c(0:10, NA)], SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Age %in% c(0:10, NA)]))
        LenNum <- keypad("How many age classes should be plotted with raw data bubbles (1, 2, or 3): ")
    }  
        
    if(is.null(LenMaxAges)) {   
        for ( i in 1:LenNum)   {
           LenMaxAges <- c(LenMaxAges, keypad("Enter maximum fish length: "))
        }
    }
   
  }  
   
   if(is.null(LenMax))
        LenMax <- max(LenMaxAges)

   # CW = Coast Wide; NCV = No Covariate; ; DV = Depth Covariate; DataSource is data source used; Region is region used; AS = Area Swept; BM = Biomass; NV = No Vessel; NY = No Year;  vX is version X
   # LM = Length Max; 
   if(DV & AS & !Pass) (DateFile <- paste0(HomeDir, Sys.Date(), '_', spShortName, '_DV_', DataSource, '_LM', LenMax, '_', substring(Version, 6), '_Rho=', rhoConfig, '_AS_nx=', n_x, '/'))  # nx Mesh method
   if(DV & AS & Pass) (DateFile <- paste0(HomeDir, Sys.Date(), '_', spShortName, '_DV_', DataSource, '_LM', LenMax, '_', substring(Version, 6), '_Rho=', rhoConfig, '_AS_P_nx=', n_x, '/'))  # nx Mesh method
   
   # if(DV & !AS ) (DateFile <- paste0(HomeDir, Sys.Date(), '_', spShortName, '_DV_', DataSource, '_LM', LenMax, '_', substring(Version, 6), '_BM_nx=', n_x, '/'))  # nx Mesh method
   if(!DV & AS) (DateFile <- paste0(HomeDir, Sys.Date(), '_', spShortName, '_', DataSource, '_LM', LenMax, '_', substring(Version, 6), '_Rho=', rhoConfig, '_AS_nx=', n_x, '/'))  # nx Mesh method
   if(!DV & !AS) (DateFile <- paste0(HomeDir, Sys.Date(), '_', spShortName, '_', DataSource, '_LM', LenMax, '_', substring(Version, 6), '_Rho=', rhoConfig, '_BM_nx=', n_x, '/')) # nx Mesh method
                 
                     
   dir.create(DateFile)
   
  if(is.null(Data_List)) { 
   
   ############################################################################################################  
   # Find proportion of species total weight that is less than or equal to the length(s) specified (LenMaxAges)
   # Bio Data (BD) is used to find what proportion of catch data is within a length range
   ############################################################################################################ 
   
   LwMale <- glm(log(SP.BD.NWFSC$Weight_kg[SP.BD.NWFSC$Sex %in% 'M']) ~ log(SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Sex %in% 'M']))$coefficients
   LwMale[1] <- exp(LwMale[1])
   LwMale
   
   LwFemale <- glm(log(SP.BD.NWFSC$Weight_kg[SP.BD.NWFSC$Sex %in% 'F']) ~ log(SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Sex %in% 'F']))$coefficients
   LwFemale[1] <- exp(LwFemale[1])
   LwFemale
 
   # Length restriction 
   SP.BD.NWFSC$WeightCalc_kg <- NA
   
   # Males
   SP.BD.NWFSC$WeightCalc_kg[ SP.BD.NWFSC$Sex %in% 'M'] <- LwMale[1] * SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Sex %in% 'M']^LwMale[2]
   
   # Females
   # For Petrale - the females and large unknown sex that are assumed to be females
   if(spShortName %in% 'PTRL') {
      TF <- SP.BD.NWFSC$Sex %in% 'F' | (SP.BD.NWFSC$Sex %in% 'U' & SP.BD.NWFSC$Length_cm > 50)
      SP.BD.NWFSC$WeightCalc_kg[TF] <- LwFemale[1] * SP.BD.NWFSC$Length_cm[TF]^LwFemale[2]
   } else
      SP.BD.NWFSC$WeightCalc_kg[SP.BD.NWFSC$Sex %in% 'F'] <- LwFemale[1] * SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Sex %in% 'F']^LwFemale[2]
   
   # Unknown sex - average of M & F used
   # For Petrale - for small unknown sex - average of M & F used
   if(spShortName %in% 'PTRL') {
      TF <- SP.BD.NWFSC$Sex %in% 'U' & SP.BD.NWFSC$Length_cm <= 50
      SP.BD.NWFSC$WeightCalc_kg[TF] <-  mean(c(LwMale[1], LwFemale[1])) * SP.BD.NWFSC$Length_cm[TF]^mean(c(LwMale[2], LwFemale[2])) 
   } else
      SP.BD.NWFSC$WeightCalc_kg[SP.BD.NWFSC$Sex %in% 'U'] <-  mean(c(LwMale[1], LwFemale[1])) * SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Sex %in% 'U']^mean(c(LwMale[2], LwFemale[2]))
   
   dev.new(8.5, 11); par(mfrow = c(3, 2))
   plot(SP.BD.NWFSC$Weight_kg, SP.BD.NWFSC$WeightCalc_kg, main = 'Black all; Red Kept', xlab = 'Sp Sample Wt', ylab = 'Sp Calc Sample Wt'); abline(0, 1, col='green')
   # print((SP.BD.NWFSC[SP.BD.NWFSC$Sex %in% 'U' & is.finite(SP.BD.NWFSC$Length_cm), ])[1:5, ])
   # print(dim(SP.BD.NWFSC[SP.BD.NWFSC$Sex %in% 'U' & is.finite(SP.BD.NWFSC$Length_cm), ]))
   # print(SP.BD.NWFSC[is.na(SP.BD.NWFSC$WeightCalc_kg), ][1:5,])
   # print(dim(SP.BD.NWFSC[is.na(SP.BD.NWFSC$WeightCalc_kg),]))
   
   # Deal with bad weights -  Now corrected weights are saved with the data # OLD: optionally, put identified bad weights in the badWts argument
   if(warehouseDownload) { 
          if(is.null(badWts)) {
          dev.new()
          cat("\n\nIdentity bad weights in the figure by clicking on the outliers.\n\n")
          plot(SP.BD.NWFSC$Weight_kg, SP.BD.NWFSC$WeightCalc_kg); abline(0, 1, col='green')
          badWts <- identify(SP.BD.NWFSC$Weight_kg, SP.BD.NWFSC$WeightCalc_kg)
          dev.off()
       }
         
       if(length(badWts) > 0) {
              SP.BD.NWFSC$Weight_kg[badWts] <- SP.BD.NWFSC$WeightCalc_kg[badWts]  # Comment out this line for collecting bad weights for Beth
              cat("\n\nBad weights:", badWts, "\n\n")
              print(SP.BD.NWFSC[badWts, ])
              
              # Collect bad weights for Beth
              # BadWts <- SP.BD.NWFSC[badWts, ]
              # BadWts$spLongName <- spLongName
              # print(BadWts)
              # assign('BadWts.SAVE', rbind(BadWts.SAVE, BadWts), pos = 1)
       } 
       
       save(SP.BD.NWFSC, SP.Catch.NWFSC, file = paste0(HomeDir, spShortName, '.BD.Catch.NWFSC.dmp')) 
   }
   
   # Use calculated wts where the normal weight is missing
   # plot(SP.BD.NWFSC$Weight_kg, SP.BD.NWFSC$WeightCalc_kg)
   SP.BD.NWFSC$Weight_kg[is.na(SP.BD.NWFSC$Weight_kg)] <- SP.BD.NWFSC$WeightCalc_kg[is.na(SP.BD.NWFSC$Weight_kg)]
   
   points(SP.BD.NWFSC$Weight_kg, SP.BD.NWFSC$WeightCalc_kg, col = 'red')
   numTowsMissingLens <- length(unique(SP.BD.NWFSC[is.na(SP.BD.NWFSC$Weight_kg), 'KEY'])) # Need to save for pushing info out to file (see below)
   cat("\n\nNumber of tows which contain missing lengths:", numTowsMissingLens, "\n\n")
   
   # Remove completely those tows which contain missing lengths - expressed here in no calculated weight from the L/W regression
   if(length(unique(SP.BD.NWFSC[is.na(SP.BD.NWFSC$Weight_kg), 'KEY'])) > 0) {
      SP.BD.NWFSC <- SP.BD.NWFSC[!SP.BD.NWFSC$KEY %in% unique(SP.BD.NWFSC[is.na(SP.BD.NWFSC$Weight_kg), 'KEY']), ]
      # print(SP.BD.NWFSC[is.na(SP.BD.NWFSC$Weight_kg), ])
   }  
   
   SP.BD.NWFSC <- match.f(SP.BD.NWFSC, aggregate(list(Total_sp_wt_sum_kg = SP.BD.NWFSC$WeightCalc_kg), list(KEY = SP.BD.NWFSC$KEY), sum, na.rm = TRUE), 
                              "KEY", "KEY", "Total_sp_wt_sum_kg")
   plot(SP.BD.NWFSC$Total_sp_wt_kg, SP.BD.NWFSC$Total_sp_wt_sum_kg, xlab = 'Total Species Wt (kg)', ylab = 'Total Sp Sample Calc Wt (kg)'); abline(0,1)
   
   # LR = Length Restricted
   DatG <- SP.Catch.NWFSC
   
   for( i in 1:LenNum) {
       LR_Name <- paste0("Total_sp_wt_LR_kg_", i)
       SP.BD.NWFSC <- match.f(SP.BD.NWFSC, aggregate(list(Total_sp_wt_sum_LR_kg = SP.BD.NWFSC$WeightCalc_kg * (SP.BD.NWFSC$Length_cm <= LenMaxAges[i] )), 
                  list(KEY = SP.BD.NWFSC$KEY), sum, na.rm = TRUE), "KEY", "KEY", "Total_sp_wt_sum_LR_kg")
       SP.BD.NWFSC[ ,LR_Name] <- SP.BD.NWFSC$Total_sp_wt_kg * (SP.BD.NWFSC$Total_sp_wt_sum_LR_kg/SP.BD.NWFSC$Total_sp_wt_sum_kg)
       SP.BD.NWFSC$Total_sp_wt_sum_LR_kg <- NULL
       plot(SP.BD.NWFSC$Total_sp_wt_kg, SP.BD.NWFSC[ ,LR_Name], ylab = paste('Extrap Sp Wt (kg); Len <=',  LenMaxAges[i], 'cm'), xlab = 'Total Extrap Species Wt (kg)'); abline(0,1)
       
       DatG <- match.f(DatG, SP.BD.NWFSC[!duplicated(SP.BD.NWFSC$KEY), c("KEY", LR_Name)], "KEY", "KEY", LR_Name)
       DatG[ ,LR_Name][is.na(DatG[ ,LR_Name])] <- 0
   }       
   # Give raw data the same strata limits as the model - this is important because with Region = 'Other' the raw data defines the underlying extrapolation grid. 
   # This also limits the nuumber of deep zeros, and hence the number of knots needed, since it appears that the underlying extrapolation grid is made from all the raw data, 
   #           not just the data defined within the strata limits below.
   
   # Depth limits of LR raw data
   # stem(DatG$Depth_m[ DatG$Total_sp_wt_LR_kg > 0], scale = 2) 
   # plot(DatG$Depth_m[ DatG$Total_sp_wt_LR_kg > 0], DatG$Total_sp_wt_LR_kg[ DatG$Total_sp_wt_LR_kg > 0])
   
   # gof()
   
   DatG.AllLengths <- DatG
   
   # ================================ END - Find proportion of species total weight =======================================================================
  
   
   # Continue below with oldest age class from above: Total_sp_wt_LR_kg_X => Total_sp_wt_LR_kg   
   DatG$Total_sp_wt_LR_kg <- DatG[, paste0("Total_sp_wt_LR_kg_", LenNum)]    
  
   # ------------- Put restrictions on latitude and depth --------------- 
     
   # Find max depth (meters) for the given LenMax
   if (is.null(DepMax)) {
       cat("\n\nPercent of zeros in length restricted data WITHOUT depth restriction:", 100 * sum(DatG$Total_sp_wt_LR_kg %in% 0)/length(DatG$Total_sp_wt_LR_kg), "\n\n")
          
       print(stem(DatG$Depth_m[DatG$Total_sp_wt_LR_kg > 0], scale = 2))
       print(rev(sort(DatG$Depth_m[DatG$Total_sp_wt_LR_kg > 0]))[1:30])
        
       gof() 
       DepMax <- keypad("Input the max depth in meters: ")
       if (!is.finite(DepMax) | DepMax > 1300) stop("DepMax is not a finite number or is greater than 1,300 meters") 
   }
   
   if (is.null(DepMin)) {
      cat("\n\n")
      print(sort(DatG$Depth_m[DatG$Total_sp_wt_LR_kg > 0])[1:30])    
      DepMin <- keypad("Input the min depth in meters: ")
       if (!is.finite(DepMin) | DepMin < 50) stop("DepMin is not a finite number or is less than 50 meters") 
   }    
   
   if(is.null(LatMax) | is.null(LatMin)) {
     Imap::imap(longrange = c(-128.5, -116), latrange = c(31.5, 49), zoom = FALSE)
     points(DatG[DatG$Total_sp_wt_LR_kg > 0, c('Longitude_dd', 'Latitude_dd')])
   }
      
   if (is.null(LatMax)) 
      LatMax <- keypad("Input the max latitude in decmial degrees: ")
   if (!is.finite(LatMax) | LatMax > 49) stop("LatMax is not a finite number or is greater than 49 degrees") 
   
   if (is.null(LatMin)) 
      LatMin <- keypad("Input the min latitude in decmial degrees: ")
   if (!is.finite(LatMin) | LatMin < 31) stop("LatMin is not a finite number or is less than 31 degrees") 
   
   
   cat("\n\nLatMax = ", LatMax, ", LatMin = ", LatMin, ", DepMin = ", DepMin, ", DepMax = ", DepMax, "\n\n", sep="") 
   DatG <- DatG[DatG$Latitude_dd <= LatMax & DatG$Latitude_dd >= LatMin & DatG$Depth_m >= DepMin & DatG$Depth_m <= DepMax, ]
   cat("\n\nPercent of zeros in length restricted data WITH depth and latitude restriction:", 100 * sum(DatG$Total_sp_wt_LR_kg %in% 0)/length(DatG$Total_sp_wt_LR_kg), "\n\n")
 
   plot(DatG$Total_sp_wt_kg, DatG$Total_sp_wt_LR_kg, ylab = 'Extrap Sp Wt (kg) used in VAST', xlab = 'Total Extrap Species Wt (kg)'); abline(0,1)
   
   cat("\n\nNumber of tows with non-zero catch (TRUE) by year with restrictions on latitude and depth:\n")
   print(Table(DatG$Total_sp_wt_LR_kg != 0, DatG$Year))

   cat("\n\n")
   
   Ages <- sort(unique(SP.BD.NWFSC$Age))[1:LenNum]
   if(any(is.na(Ages))) Ages <- NULL

   # Create species info file
   sink(paste0(DateFile, "Species Info", spShortName, ".txt"))
       cat("\n", casefold.f(spLongName), " (", Species, "; ", spShortName, ")\n\n", sep="")
       print(Table(SP.BD.NWFSC$Age[SP.BD.NWFSC$Age %in% 0:10], SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Age %in% 0:10]))
       cat("\n")
       print(Table(SP.BD.NWFSC$Age[SP.BD.NWFSC$Age %in% c(0:10, NA)], round(SP.BD.NWFSC$Length_cm[SP.BD.NWFSC$Age %in% c(0:10, NA)])))
       cat("\n\nAges:",  paste(Ages, fill = ",", sep = ""), "\n")
       cat("\nMaximum lengths by age class (cm):", paste(LenMaxAges, fill = ",", sep = ""), "\n")
       cat("\nLatitude min: ", LatMin, "; Latitude max: ", LatMax, "\n", sep="")
       cat("\nDepth min (m): ", DepMin, "; Depth max (m): ", DepMax, "\n", sep="")
       cat("\nNumber of extreme individual weights removed:", length(badWts), "\n")
       cat("\nNumber of tows which contain missing lengths:", numTowsMissingLens, "\n")
       cat("\nPercent of zeros in length restricted data WITH depth and latitude restriction:", 100 * sum(DatG$Total_sp_wt_LR_kg %in% 0)/length(DatG$Total_sp_wt_LR_kg), "\n\n")
       cat("\nNumber of tows per year after applying depth and latitude restrictions:\n")
       print(Table(DatG$Year))
       cat("\n\n\n")
   sink()
   
   # LenMin <- SP.BD.NWFSC$Length_cm <= LenMaxAges[i]  - need lots of changes for one year class at a time........
   LenMin <- min(SP.BD.NWFSC$Length_cm, na.rm = TRUE) # For info in species results figure.
 
   # DatG.SAVE <<- DatG 
   
   # Coastal Areas for both raw data and results with raw data plots
    if(F) {
       cA <- list()
       cA[[1]] <- list(Name = "1. WA North", long = c(-126, -124), lat = c(47.0, 48.5))
       cA[[2]] <- list(Name = "2. WA South & OR North", long = c(-126, -123.7), lat = c(44.9, 47.25))
       cA[[3]] <- list(Name = "3. OR North & Central", long = c(-126, -123.7), lat = c(43.6, 46.0))
       cA[[4]] <- list(Name = "4. OR Central & South", long = c(-126, -124), lat = c(42, 44))
       cA[[5]] <- list(Name = "5. OR South, CA North", long = c(-126, -124), lat = c(40, 42.3))
       cA[[6]] <- list(Name = "6. CA Central", long = c(-126, -121), lat = c(36, 40))
       cA[[7]] <- list(Name = "7. CA South", long = c(-122, -116.8), lat = c(32, 36)) 
     }     
     
     # All figures are 2.75 degrees in latitude - #6 is in landscape dimensions
       cA <- list()
       cA[[1]] <- list(Name = "1. WA & OR North", long = c(-126, -123.8), lat = c(45.75, 48.5))
       cA[[2]] <- list(Name = "2. OR Central", long = c(-126, -123.9), lat = c(43.0, 45.75))
       cA[[3]] <- list(Name = "3. OR South & CA North", long = c(-126, -124), lat = c(40.25, 43.0))
       cA[[4]] <- list(Name = "4. CA North & Central", long = c(-125.5, -122), lat = c(37.5, 40.25))
       cA[[5]] <- list(Name = "5. CA Central & South", long = c(-123.7, -120.5), lat = c(34.75, 37.5))
       # cA[[6]] <- list(Name = "6. CA South", long = c(-122, -116.8), lat = c(32, 34.75))
       cA[[6]] <- list(Name = "6. CA South - Landscape", long = c(-122, -116.8), lat = c(32, 34.75))
       cA[[7]] <- list(Name = "7. Entire Coast", long = c(-127.3, -114.7), lat = c(31.5, 48.5))
        
        
    #################################
    #    Bubble plots of raw data
    ################################# 
   
    if(RawDataPlots) {
    
      if(allAgesBubble) DAT <- DatG.AllLengths else DAT <- DatG
     
       (PNG <- c(T, F)[1]) # TRUE = PNG; FALSE = Windows
       (BIOMASS <- c(T, F)[2]) # TRUE  = Biomass Level; FALSE = Random color
       (SCALE.SIZE = Bubble.Size) # Scale for bubbles in the bubble plots
       (CONTOUR <- c(T, F)[1])
        
       
       DirRaw <- paste0(DateFile, "Figs/", "Raw Data Bubble Plots", ", ", LenMax, " LenMax", "/")
       dir.create(DirRaw, recursive = TRUE)
       print(DirRaw)
       
       for ( i in 1:6) {
           if(PNG)  png(paste0(DirRaw, cA[[i]]$Name, ".png"), width = 2048, height = 2048 * ifelse( i == 6, 0.65, 1), bg = 'grey') else dev.new(width = 40, height = 30) 
           # autoLayer fails for cA[[5]], giving a partial SoCal hi-rez result
           Imap::plotGIS(long = cA[[i]]$long, lat = cA[[i]]$lat, levels.contour = { if(CONTOUR) { c(-60, -80, -100, -120, -140, seq(0, -2000, by = -200)) } else NULL }, autoLayer = ifelse( i == 6, TRUE, FALSE))
           
            # stars(data.frame(Two_and_Older = DAT$Total_sp_wt_kg - DAT$Total_sp_wt_LR_kg, Zero_and_One = DAT$Total_sp_wt_LR_kg), 
            #            locations = DAT[,c("Longitude_dd", "Latitude_dd")], labels = NULL, add = TRUE, draw.segments = TRUE, len=0.25)
            # if(PNG) gof()                      
            #  }
            
            # TMP1 <- DAT[!duplicated(DAT$KEY) & (DAT$Total_sp_wt_LR_kg %in% 0), c("Longitude_dd", "Latitude_dd", "Total_sp_wt_kg")]
            
            if(allAgesBubble) {
                BubDATA <- DAT[!duplicated(DAT$KEY), c("Longitude_dd", "Latitude_dd", "Total_sp_wt_kg")]
            } else 
                BubDATA <- DAT[!duplicated(DAT$KEY) & DAT$Total_sp_wt_LR_kg %in% 0, c("Longitude_dd", "Latitude_dd", "Total_sp_wt_LR_kg")]
           
            names(BubDATA) <- c('X', 'Y', 'Z')
            BubGroup <- rep(0, nrow(BubDATA))
            LenMaxCols <- c("cyan", list("red", c("blue", "red"), c("magenta", "blue", "red"))[[LenNum]])
        
            for( i in LenNum:1) {
              TMP <- DAT[!duplicated(DAT$KEY) & !(DAT[, paste0("Total_sp_wt_LR_kg_", i)] %in% 0), c("Longitude_dd", "Latitude_dd", paste0("Total_sp_wt_LR_kg_", i))]
              names(TMP) <- c('X', 'Y', 'Z')
              BubDATA <- rbind(BubDATA, TMP)
              BubGroup <- c(BubGroup, rep(i, nrow(TMP)))
            }
            
            if(is.null(Extra.Group.Size))
                  Extra.Group.Size <- rep(1, LenNum + 1)
            if(length(Extra.Group.Size) == 1)
                  Extra.Group.Size <- c(1, rep(Extra.Group.Size, LenNum))
      
            JRWToolBox::plot.bubble.zero.cross(BubDATA, group = BubGroup, add=T, scale.size = SCALE.SIZE, cross.cex = ifelse(PNG, 1, 0.2), cross.col = ifelse(i == 1, 'darkcyan', 'cyan'),
                      fill.col = LenMaxCols, border.col = LenMaxCols, fill.col.alpha = c(0.75, 0.80, 0.80), border.col.alpha = c(0.75, 0.80, 0.80), Extra.Group.Size = Extra.Group.Size, legend = F)
            
            if(PNG) dev.off() 
       }
       rm(DAT)
   }
   
   # ================================ END - Bubble plots of raw data ================================================================
   
   
   rm(SP.BD.NWFSC, SP.Catch.NWFSC,  DatG.AllLengths) # Current environment
   # rm(SP.BD.NWFSC, SP.Catch.NWFSC, pos = 1) # .GlobalEnv
 
  }  # End if(is.null(Data_List))
  
  
 if(runVAST) {
    
     # --------------- Continue with VAST ---------------------------------------
  
     cat('\n\nStarting VAST\n\n')
	 
	 if(!is.null(Data_List)) {
             DatG <- Data_List[[1]]
       depthCovar <- Data_List[[2]]
	         ages <- Data_List[[3]]
	       LenMin <- Data_List[[4]]
     }
 
  
  
     # Year_Set & Years2Include
         Year_Set = seq(min(DatG$Year),max(DatG$Year))
         Years2Include = which(Year_Set %in% sort(unique(DatG$Year)))
     
     # Coast wide only area strata in meters 
     (strata.limits <- data.frame(
           STRATA = c("Coastwide"),
           north_border = LatMax,
           south_border = LatMin,
           shallow_depth = DepMin,
           deep_depth = DepMax
      ))
     
           
      FieldConfig = c(Omega1=1, Epsilon1=1, Omega2=1, Epsilon2=1) #  Where Omega refers to spatial variation, Epsilon refers to spatio-temporal variation, Omega1 refers to variation in
         # encounter probability, and Omega2 refers to variation in positive catch rates, where 0 is off, "AR1" is an AR1 process, and >0 is the number of elements in a factor-analysis covariance
      RhoConfig = c(Beta1=rhoConfig, Beta2=rhoConfig, Epsilon1=0, Epsilon2=0) # OPTIONAL, vector of form c("Beta1"=0,"Beta2"=0,"Epsilon1"=0,"Epsilon2"=0) specifying whether either intercepts 
         # (Beta1 and Beta2) or spatio-temporal variation (Epsilon1 and Epsilon2) is structured among time intervals (0: each year as fixed effect; 1: each year as random following IID distribution;
         # 2: each year as random following a random walk; 3: constant among years as fixed effect; 4: each year as random following AR1 process)
                                                 
      OverdispersionConfig = c("Delta1"=1, "Delta2"=1) # OPTIONAL, a vector of format c("eta1"=0, "eta2"="AR1") governing any correlated overdispersion among categories for each level of v_i, 
         # where eta1 is for encounter probability, and eta2 is for positive catch rates, where 0 is off, "AR1" is an AR1 process, and >0 is the number of elements in a factor-analysis covariance
         
         # See the help for ?VAST::make_data() in R and the VAST manual on GitHub
         # Matrix with two columns where first column specifies the distribution for positive catch rates, and second element specifies the functional form for encounter probabilities
         # ******** ObsModel[2] = 0  # Conventional delta-model using log-link for positive catch rates and logit-link for encounter probability
         # ObsModel = c(2, 0)  # Gamma example: 0=normal (log-link); 1=lognormal; 2=gamma; 4=ZANB; 5=ZINB; 11=lognormal-mixture; 12=gamma-mixture
         # ******** ObsModel[2] = 1 # Poisson-link function that approximates a Tweedie distribution was needed for the Petrale Winter fishery logbook data
         #                            Alternative delta-model using log-link for numbers-density and log-link for biomass per number (see the manual)    
      ObsModel = c(2,1)  # Poisson-link functions with a Gamma model for positive catch and binominal errors for the presence/absence
         # ********  ObsModel[2] = 2
         # ObsModel = c(10, 2) # 10 = Tweedie distribution; 2 = Link function for a [true] Tweedie distribution, necessary for ObsModel[1]=8 or ObsModel[1]=10
         # ObsModel = c( 8, 2) #  8 = Compound-Poisson-Gamma
         
      Kmeans_Config = list( randomseed = 1, nstart = 100, iter.max = 1000 )    # Samples: Do K-means on trawl locs; Domain: Do K-means on extrapolation grid
      
      # PlotResultsOnMap_Fn() says to: "Please re-run with Options('SD_site_logdensity'=1,...) to use 'plot_num=10' in 'VAST'"
      Options <- c(SD_site_density=0, SD_site_logdensity=1, Calculate_Range=1, Calculate_evenness=0, Calculate_effective_area=1, Calculate_Cov_SE=0, 
                   Calculate_Synchrony=0, Calculate_Coherence=0)
     
      # Save options for future records
      # Record <- ThorsonUtilities::bundlelist( c("Version","Method","grid_size_km","n_x","FieldConfig","RhoConfig","OverdispersionConfig","ObsModel","Kmeans_Config") )
      # capture.output( Record, file=paste0(DateFile,"Record.txt"))
         
     
      Extrapolation_List <- FishStatsUtils::make_extrapolation_info(Region=Region, strata.limits=strata.limits, observations_LL = cbind(Lat = DatG$Latitude_dd, Lon = DatG$Longitude_dd) )
           
      Spatial_List = FishStatsUtils::make_spatial_info( grid_size_km=grid_size_km, n_x=n_x, Method=Method, Lat = DatG$Latitude_dd, Lon = DatG$Longitude_dd, 
            Extrapolation_List=Extrapolation_List, randomseed=Kmeans_Config[["randomseed"]], nstart=Kmeans_Config[["nstart"]], iter.max=Kmeans_Config[["iter.max"]], 
            DirPath = DateFile )
     
      # save(Spatial_List, file=paste0(DateFile, "Spatial_List.RData"))
           
      # Make TMB data list - ****** Using area sweeped for the effort with WCGBTS ******
     
      # No depth covariate     
      if(!DV) {
           
      # No duration of effort; a_i = DatG$DURATION is now: a_i = rep(1, nrow(DatG))  
         # Area_Swept_ha for effort  ## No vessel tow effort
                  
         if(AS) 
           TmbData <- VAST::make_data(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
             "c_i"=rep(0, nrow(DatG)), "b_i"=DatG$Total_sp_wt_LR_kg, "a_i"=DatG$Area_Swept_ha, "v_i"=as.numeric(factor(DatG$Vessel))-1, 
             spatial_list = Spatial_List, "s_i"=Spatial_List$knot_i-1, "t_i"=DatG$Year, "a_xl"=Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, 
             Method=Spatial_List$Method, Options=Options )         
                 
         if(!AS) 
          TmbData <- VAST::make_data(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
             c_i=rep(0, nrow(DatG)), b_i=DatG$Total_sp_wt_LR_kg, a_i=rep(1, nrow(DatG)), v_i=as.numeric(factor(DatG$Vessel))-1, 
             spatial_list = Spatial_List, s_i=Spatial_List$knot_i-1, t_iz=DatG$Year, a_xl=Spatial_List$a_xl, MeshList=Spatial_List$MeshList, GridList=Spatial_List$GridList, 
             Method=Spatial_List$Method, Options=Options )
       }
      
       # Depth covariate - **** with area swept (AS) =  TRUE only *****
      if(DV) {
	   
	    if(is.null(Data_List)) { 
		
             knotsLatLong <- JRWToolBox::UTM.to.LatLong(1000 * Spatial_List$MeshList$loc_x)
             cat("\n\nCalculating bathymetric depths for the VAST knots using the Imap package's depthMeters() function:\n\n")
             knotsDepth <- Imap::depthMeters(knotsLatLong)		 
             dev.new(); hist(knotsDepth)
             knotsDepth[knotsDepth < 0] <- 0  # Set positive elevation (negative depth) of any knots on an island to zero. This can happen using Region = 'Other' - and bad things happen!!
             knotsDepth[knotsDepth == 0] <- min(knotsDepth[knotsDepth > 0])/2   # ########## try log ###############
             knotsDepth <- log(knotsDepth)  # ########## try log ###############
             depthCovar <- array(rep(knotsDepth, times = length(Year_Set)), dim = c(n_x, length(Year_Set), 1))
           
          # Attempts to use format_covariates() for the depth covariate
          # depthCovar <- SpatialDeltaGLMM::format_covariates(knotsLatLong$Lat, knotsLatLong$Long, rep(2003:2015, len = nrow(knotsLatLong)), matrix(rep(knotsDepth, times = 13), nrow=600), Extrapolation_List, Spatial_List, Year_Set = 2003:2015)
             # depthCovar <- SpatialDeltaGLMM::format_covariates(knotsLatLong$Lat, knotsLatLong$Long, data.frame(Year = rep(2015, nrow(knotsLatLong))), data.frame(Depth_m = knotsDepth), Extrapolation_List, Spatial_List, Year_Set = 2015)
             # depthCovar <- SpatialDeltaGLMM::format_covariates(knotsLatLong$Lat, knotsLatLong$Long, matrix(as.numeric(gl(13, nrow(knotsLatLong))), nrow=nrow(knotsLatLong)) + 2002, 
             #             knotsDepth, Extrapolation_List, Spatial_List)
        }
	  
          if(AS) {
		  
		    if(Pass) {
			  DatG$Pass <- DatG$Pass - 1.5 
              TmbData <- VAST::make_data(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
                 c_i=rep(0, nrow(DatG)), b_i = DatG$Total_sp_wt_LR_kg, a_i = if(AS) DatG$Area_Swept_ha else rep(1, nrow(DatG)), v_i=as.numeric(factor(DatG$Vessel)) - 1, 
                 spatial_list = Spatial_List, s_i=Spatial_List$knot_i - 1, t_i = DatG$Year, a_xl = Spatial_List$a_xl, Q_ik = matrix(DatG$Pass, ncol = 1), MeshList=Spatial_List$MeshList, 
				 GridList=Spatial_List$GridList, X_xtp = depthCovar, Method=Spatial_List$Method, Options = Options )
		    }
		  
		    if(!Pass) 
			  TmbData <- VAST::make_data(Version=Version, FieldConfig=FieldConfig, OverdispersionConfig=OverdispersionConfig, RhoConfig=RhoConfig, ObsModel=ObsModel, 
                 c_i=rep(0, nrow(DatG)), b_i = DatG$Total_sp_wt_LR_kg, a_i = if(AS) DatG$Area_Swept_ha else rep(1, nrow(DatG)), v_i=as.numeric(factor(DatG$Vessel)) - 1, 
                 spatial_list = Spatial_List, s_i=Spatial_List$knot_i - 1, t_i=DatG$Year, a_xl = Spatial_List$a_xl, MeshList = Spatial_List$MeshList, GridList=Spatial_List$GridList, 
                 X_xtp = depthCovar, Method = Spatial_List$Method, Options = Options )
         }				 
      }    
   

     
     
     
      # Make TMB object
      TmbList = VAST::make_model(TmbData=TmbData, RunDir=DateFile, Version=Version, RhoConfig=RhoConfig, loc_x=Spatial_List$loc_x)
      Obj = TmbList[["Obj"]]
     
      # Run the model
      gc()
      setwd(DateFile)
      sink("TMB_Output.txt")
         # Not using the bias correction here since we are more interested in the areas than the index (J. Thorson, pers. comm.).
         # Opt = TMBhelper::fit_tmb( obj=Obj, lower=TmbList$Lower, upper=TmbList$Upper, getsd=TRUE, savedir=DateFile, bias.correct=FALSE, bias.correct.control = list(sd = TRUE, nsplit = 5))
         # Removed the parameter limits used by nlminb() since previously one parameter was hitting a bound
      Opt = TMBhelper::fit_tmb( obj = Obj, getsd = TRUE, bias.correct = FALSE, bias.correct.control = list(sd = TRUE, nsplit = 5))
         # Opt = TMBhelper::fit_tmb( obj=Obj, lower=TmbList[["Lower"]], upper=TmbList[["Upper"]], getsd=TRUE, savedir=DateFile, 
         #                           bias.correct=TRUE, bias.correct.control=list(sd=FALSE, split=NULL, nsplit=5, vars_to_correct="Index_cyl"))
      sink()  
      Report <- Obj$report()
         
      # Create 'parameter_estimates.txt' without scientific notation
         
      OptRnd <- Opt
      OptRnd <- list()
      OptRnd$par <- JRWToolBox::r(Opt$par)
      OptRnd$diagnostics <- JRWToolBox::r(Opt$diagnostics)
      OptRnd$SD <- JRWToolBox::r(summary(Opt$SD, "fixed"), 6) 
      OptRnd$Maximum_gradient_component <- Opt$max_gradient
      OptRnd$pdHess <- Opt$SD$pdHess
      OptRnd$Convergence_check <- ifelse(Opt$SD$pdHess,  { ifelse(Opt$max_gradient < 0.0001, "There is no evidence that the model is not converged", 
                       "The model is likely not converged (the critera is a pd Hess and the max_gradient < 0.0001)") }, "The model is definitely not converged")
      print(OptRnd)
      capture.output(OptRnd, file = file.path(DateFile, "parameter_estimates.txt"))
        
      # Optimization result- including the test of the range of Raw1 and Raw2 should be inside of min and max distance of between knot locations
      cat("\nnlminb() convergence (Zero indicates successful convergence):", Opt$convergence, "\n\nnlminb() message:", Opt$message, "\n\nnlminb() pdHess:", Opt$SD$pdHess, "\n\nAIC:", Opt$AIC, "\n\n")
      sink(paste0(DateFile, "Final_Convergence_Results.txt"))
         cat("\nnlminb() convergence (Zero indicates successful convergence):", Opt$convergence, "\n\nnlminb() message:", Opt$message, "\n\nnlminb() pdHess:", Opt$SD$pdHess, "\n\nAIC:", Opt$AIC, "\n\n")
         cat("\nRange Raw1 and Raw2 should be inside of min and max distance of between knot locations\n\n")
         # Range Raw1 and Raw2 should be inside of min and max distance of between knot locations (J. Thorson, pers. comm.)
         print(r(sort(c(Range_raw1 = Report$Range_raw1, Range_raw2 = Report$Range_raw2, minDist = min(dist( Spatial_List$loc_x )), maxDist = max(dist( Spatial_List$loc_x ))))))
      sink()
      
      # Pre-save in case make_map_info() fails!
      save(list = c(ls(), names(.GlobalEnv)), file = paste0(DateFile, "Image.RData")) # Save files inside the function also!!!!!! 
      
      # Create MapDetails_List
         MapDetails_List = FishStatsUtils::make_map_info(Region = Region, Extrapolation_List = Extrapolation_List, spatial_list = Spatial_List, NN_Extrap = Spatial_List$PolygonList$NN_Extrap) # Make this list before the save!!!!!
         
      setwd(HomeDir)
      
      # Save the VAST run - with make_map_info() - final save right at the end!
      save(list = c(ls(), names(.GlobalEnv)), file = paste0(DateFile, "Image.RData")) # Save files inside the function also!!!!!!
      
     } # End runVAST
      
     if(runDiagnostics) {
          #######################
          # Make diagnostic plots
          #######################
          
              
          ######################
          
          # Load up image file, change the DateFile and run the code needed
          # base::load(Image.RData)
          # setwd("W:/ALL_USR/JRW/Assessment/VAST_Runs/SP")
          # HomeDir = "./"
          # DateFile = "./"
          # FishStatsUtils::plot_range_index(Sdreport=Opt$SD, Report=Report, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile)
          
          ######################
          
         
          # Plot data and knots
          try( FishStatsUtils::plot_data(Extrapolation_List=Extrapolation_List, Spatial_List=Spatial_List, Data_Geostat=data.frame( spp=rep("SP", nrow(DatG)), 
                     Year=DatG$Year, Catch_KG=DatG$Total_sp_wt_LR_kg, AreaSwept_km2=DatG$Area_Swept_ha/100, Vessel=DatG$Vessel, Lat=DatG$Latitude_dd, Lon=DatG$Longitude_dd), PlotDir = DateFile) )
              
          # Plot Anisotropy  
          try( FishStatsUtils::plot_anisotropy( FileName=paste0(DateFile,"Aniso.png"), Report=Report, TmbData=TmbData ) )
         
          # Plot index & Table_for_SS3.csv
          IndexTable <- FishStatsUtils::plot_biomass_index(DirName=DateFile, TmbData=TmbData, Sdreport=Opt$SD, Year_Set=sort(unique(DatG$Year)), strata_names=strata.limits[,1], use_biascorr=TRUE,
                                        width = 11, height = 8)$Table
                  
          # Plot surface - Msg from code: plot_num=1 doesn't work well when using ObsModel[2]==1
          try( FishStatsUtils::plot_maps(plot_set=c(2:5,8:10), MappingDetails=MapDetails_List[["MappingDetails"]], Report=Report, Sdreport=Opt$SD, PlotDF=MapDetails_List[["PlotDF"]], 
                                  MapSizeRatio=MapDetails_List[["MapSizeRatio"]], Xlim=MapDetails_List[["Xlim"]], Ylim=MapDetails_List[["Ylim"]], FileName=paste0(DateFile,"Field_"), 
                                  Year_Set=Year_Set, Years2Include=Years2Include, Rotate=MapDetails_List[["Rotate"]], mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), Cex=MapDetails_List[["Cex"]], 
                                  cex=1.8, Legend=MapDetails_List[["Legend"]], zone=MapDetails_List[["Zone"]]) )
         
          # Plot center of gravity and effective area occupied.
          RangeShifts <- FishStatsUtils::plot_range_index(Sdreport=Opt$SD, Report=Report, TmbData=TmbData, Znames=colnames(TmbData$Z_xm), PlotDir=DateFile)
     
          # QQ plot by year; field density, EPS, linear prediction positive catch rate and presence, posterior predictive, and over all years; posterior predictive histogram, and Q-Q plot
          QQ <- FishStatsUtils::plot_quantile_diagnostic( TmbData=TmbData, Report=Report, DateFile = DateFile, FileName_PP="Posterior_Predictive.jpg", FileName_Phist = "Posterior_Predictive-Histogram.jpg",
                                       FileName_QQ = "Q-Q_plot.jpg", FileName_Qhist = "Q-Q_hist.jpg") 
         
          # Residuals,  not sure if needed now: mar=c(0,0,2,0), oma=c(3.5,3.5,0,0), cex=1.8) 
           try( FishStatsUtils::plot_residuals(Lat_i=Data_Geostat[,'Lat'], Lon_i=Data_Geostat[,'Lon'], Report = Report, Q=QQ, spatial_list = Spatial_List, extrapolation_list = Extrapolation_List, TmbData=TmbData,  
                working_dir=DateFile, Year_Set=Year_Set, Years2Include=Years2Include) )
        
          stopifnot(exists('IndexTable'), exists('RangeShifts'), exists('QQ'))
          
          graphics.off()
          setwd(HomeDir)
          
          
          #####################################################
          #  VAST Species Results by Year
          #####################################################   
          
          # YearlyResultsFigures(SP.Results.Dpth = SP.Results.Dpth)
          # eastLongitude = -160.5 # OLD
          # Warning: if the argument SP.Results.Dpth. = NULL, but 'SP.Results.Dpth' is found, that 'SP.Results.Dpth' is used. Delete or rename the file and rerun to have it recalculated.
          SP.Results.Dpth <- JRWToolBox::YearlyResultsFigures(spLongName. = spLongName, spShortName. = spShortName, longitudeDelta = 2.6, Index = IndexTable, SP.Results.Dpth = NULL, 
              MapDetails_List = MapDetails_List, Report = Report, Opt = Opt, DateFile = DateFile, Year_Set = Year_Set, Years2Include = Years2Include, Ages = Ages, LenMin = LenMin, LenMax = LenMax, 
              strata.limits = strata.limits, HomeDir = HomeDir, rhoConfig. = rhoConfig, Graph.Dev = "tif", title = TRUE)
              
          stopifnot(exists('SP.Results.Dpth'))
          
          # assign('SP.Results.Dpth.SAVE', SP.Results.Dpth, pos = 1)
          # save(list = ls(), file = paste0(DateFile, "Dump.RData"))  
          
          #####################################################
          #  WCGBTS VAST results plotted with raw data bubbles 
          #####################################################
          
          # ------ First, prepare the VAST species results as reflected in the groups of underlying grid points associated with each knot.  -------
          
          # Make sure DateFile exists and is correct
          print(DateFile)
              
          lib(VAST) # Loads FishStatsUtils also
                
          # str(Spatial_List$PolygonList$NN_Extrap)
          # dim(cbind(Spatial_List$PolygonList$NN_Extrap$nn.idx, Spatial_List$PolygonList$NN_Extrap$nn.dists))
          # cbind(Spatial_List$PolygonList$NN_Extrap$nn.idx, Spatial_List$PolygonList$NN_Extrap$nn.dists)[1:10,]
          # Table(Spatial_List$PolygonList$NN_Extrap$nn.idx)
          
          
          # ------- Add an area label - counting from South to North on first latitude in the area (didn't bother with centroid) ----------
          SP.X2003 <- sort.f(SP.Results.Dpth[!duplicated(SP.Results.Dpth$X2003), 3:5], 2)
          SP.X2003$Area <- paste0("A:", 1:length(unique(SP.Results.Dpth$X2003))) 
          SP.Results.Dpth.Area <- match.f(SP.Results.Dpth[,-(1:2)], SP.X2003, "X2003", "X2003", "Area", round.=F) # Removed northings and eastings here
          
          
          # Stack the data by year so all years can be plotted
          # First year
          SP.Results.Biomass.Stacked.Dpth <- data.frame(SP.Results.Dpth.Area[, c(1:3, ncol(SP.Results.Dpth.Area))], Year =  min(DatG$Year))
          names(SP.Results.Biomass.Stacked.Dpth)[3] <- "CPUE.kgPerh"  #  Labeled here as not in log space - moved out of log space below
          print(SP.Results.Biomass.Stacked.Dpth[1:2,])
          
          # Second => last year
          for ( i in 4:(ncol(SP.Results.Dpth.Area) - 1)) {
                cat("\n\n", i +  min(DatG$Year) - 3, "\n")
                Out <- data.frame(SP.Results.Dpth.Area[,c(1:2, i, ncol(SP.Results.Dpth.Area))], Year = i + min(DatG$Year) - 3)
                names(Out)[3] <- "CPUE.kgPerh"
                print(Out[1:2, ]); flush.console()  
                SP.Results.Biomass.Stacked.Dpth <- rbind(SP.Results.Biomass.Stacked.Dpth, Out)
          }
          
          # Moved out of log space
          SP.Results.Biomass.Stacked.Dpth$CPUE.kgPerh <- exp(SP.Results.Biomass.Stacked.Dpth$CPUE.kgPerh)
          
          dev.new()
          print(histogram(~SP.Results.Biomass.Stacked.Dpth$CPUE.kgPerh | factor(SP.Results.Biomass.Stacked.Dpth$Year), type = 'count')) # Without logging
          dev.new()
          print(histogram(~log(SP.Results.Biomass.Stacked.Dpth$CPUE.kgPerh) | factor(SP.Results.Biomass.Stacked.Dpth$Year), type = 'count')) # Logged
          
          # save(SP.Results.Biomass.Stacked.Dpth, file = "SP.Results.Biomass.Stacked.Dpth.dmp")
          
          
          # Find the quantiles 
          tmp <- quantile(unique(SP.Results.Dpth.Area$X2003), seq(0, 1, length = length(unique(SP.Results.Dpth.Area$Area))))
          (Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100))[1:10,]
          
          # First year
          SP.Quant.Biomass.Dpth <- renum(match.f(SP.Results.Dpth.Area[,c("X", "Y", "Area", "X2003")], Quant.Tab, "X2003", "Val", "Quant", round. = T, digits = 7))[,-4]
          colnames(SP.Quant.Biomass.Dpth)[4] <- paste0("X", min(DatG$Year))
          
          # Second => last year
          for( i in 4:(ncol(SP.Results.Dpth.Area) - 1)) {
             bar(i, max(4:(ncol(SP.Results.Dpth.Area) - 1)))
             tmp <- quantile(unique(SP.Results.Dpth.Area[,i]), seq(0, 1, length = length(unique(SP.Results.Dpth.Area$Area))))
             Quant.Tab <- data.frame(Val = tmp, Quant = as.numeric(strsplit( names(tmp), "%"))/100)
             Temp <- match.f(SP.Results.Dpth.Area[,c(1, 2, i)], Quant.Tab, paste0("X", i + min(DatG$Year) - 3), "Val", "Quant", round. = T, digits = 7)
             SP.Quant.Biomass.Dpth <- cbind(SP.Quant.Biomass.Dpth, Temp[,4])
             colnames(SP.Quant.Biomass.Dpth)[ncol(SP.Quant.Biomass.Dpth)] <- paste0("X", i + min(DatG$Year) - 3)
          }
          
          sum(is.na(SP.Quant.Biomass.Dpth)) # No NA's now with using "round. = T, digits = 7"
          SP.Quant.Biomass.Dpth[1:4,]
          
          # save(SP.Quant.Biomass.Dpth, file = "SP.Quant.Biomass.Dpth.dmp")
          
          
          # Stack the quants
          SP.Quant.Biomass.Stacked.Dpth <- data.frame(SP.Quant.Biomass.Dpth[,1:4], Year = 2003)
          names(SP.Quant.Biomass.Stacked.Dpth)[4] <- "Quants" 
          
          for ( i in 5:(ncol(SP.Quant.Biomass.Dpth))) {
          
                Out <- data.frame(SP.Quant.Biomass.Dpth[,c(1:3,i)], Year = i + 1999)
                names(Out)[4] <- "Quants" 
          
                SP.Quant.Biomass.Stacked.Dpth <- rbind(SP.Quant.Biomass.Stacked.Dpth, Out)
          }
          
          SP.Quant.Biomass.Stacked.Dpth <- renum(SP.Quant.Biomass.Stacked.Dpth)
          sum(is.na(SP.Quant.Biomass.Stacked.Dpth)) # 0
          
          # save(SP.Quant.Biomass.Stacked.Dpth, file="SP.Quant.Biomass.Stacked.Dpth.dmp")  
          
          # Find the top areas
           
          Pet.Quant.80 <- SP.Quant.Biomass.Stacked.Dpth[SP.Quant.Biomass.Stacked.Dpth$Quants > ( 1 - Top.Prct/100), ]
          Table(Pet.Quant.80$Area, Pet.Quant.80$Year)
          dim(Table(Pet.Quant.80$Area, Pet.Quant.80$Year))
          
          tmp <- sort(apply(Table(Pet.Quant.80$Area, Pet.Quant.80$Year), 1, function(x) sum(x > 0)))
          Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth <- names(tmp[tmp >= Top.Years])  # This name is not modified for particular Top.Prct and Top.Year arguments
          tmp2 <- Pet.Quant.80[Pet.Quant.80$Area %in% Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth,]
          # Table(tmp2$Area, tmp2$Year)
          dim(Table(tmp2$Area, tmp2$Year))
          
          Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth
          length(Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth)
          # save(Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth, file= "Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth.dmp")
          
              
          # --------------- Finally plot VAST results along with the raw data in DatG -----------------
          
          dev.new(); hist(SP.Results.Biomass.Stacked.Dpth$CPUE.kgPerh)
          dev.new(); hist(DatG$Total_sp_wt_LR_kg/DatG$Area_Swept_ha)
          
          
          (PNG <- c(T, F)[1]) # TRUE = PNG; FALSE = Windows  # Windows is only good very initially and not that WYSWYG for PNG
          (PLOTTER <- c("GIS", "Imap")[1]) # Use Imap only for the entire coast option, i.e. uncomment  < # for ( i in 7) {   # Entire coast using Imap >  and comment out  < for ( i in 1:6) { >  on lines 807 and 808.
          (AREA.TYPE <- c("All", "Top")[2])   # AREAS are the groupings of the underlying extrapolation points
          (BIOMASS <- c(T, F)[2]) # TRUE = Biomass Level coloration; FALSE = Random color; Biomass Level are those values assigned by running VAST, here standardized for plotting.
          (DRAW.TRIANGULATION.MESH <- c(T, F)[2])    #  TRUE turns on drawing the triangulation mesh
          (SCALE.SIZE = Bubble.Size) # Scale argument for bubbles in the bubble plots  (now set via an argument)
          (CONTOUR <- c(T, F)[1])
          (POLYGONS <- c(T, F)[2])
          (PLOTLOWAREAS <- c(T, F)[2])   # Plot the areas not considered high, i.e. the low areas.
          
          if(POLYGONS & !exists('AreaGroup'))
             load("W:\\ALL_USR\\JRW\\Assessment\\SP - Melissa\\Org. Files Nov 2017, 2015 Data\\4 - Run VAST on WCGBTS\\SP WCGBTS AreaGroup 05 Apr 2018 3_25PM.dmp")
          
          
         
          Dir <- paste0(DateFile, "Figs/", AREA.TYPE, " Areas, ", ifelse(BIOMASS, "Biomass Level", "Random Col"), ", Top ", Top.Prct, "per in ", Top.Years, "_", max(Years2Include), " Years/")
          dir.create(Dir, recursive = TRUE)
          print(Dir)
          
          # ---- All Areas ----
          if(AREA.TYPE == "All")
              (AREAS <- unique(SP.Results.Biomass.Stacked.Dpth$Area))  
           
          # ---- Top Areas ----
          if(AREA.TYPE == "Top") 
              AREAS <- Top.Areas.80.by.Year.at.Least.40prct.of.Years.Dpth
               
          # Biomsss colors
          Col = colorRampPalette(colors = c("darkblue", "blue",  "orange", "red"))
          
          # For each area, find the random colors and shapes for the the dual stacked points (this makes them consistent across figure panels)
          AreaCol <- unique(SP.Results.Biomass.Stacked.Dpth$Area)
          NC <- length(AreaCol)
          AreaCol <- data.frame(Area = AreaCol, Col1 = c("grey40","cyan4","gold4",rainbow(11))[sample(14, NC, replace=T)], Col2 = c("grey40","cyan4","gold4",rainbow(11))[sample(14, NC, replace=T)],
                                    Shape1 = sample(c(15,17,19), NC, replace=T), Shape2 = sample(c(15,17,19), NC, replace=T))
            
          # See the raw data plots above for the cA (Coastal Areas) list
           
          # for ( i in 7) {   # Entire coast using Imap
          for ( i in 1:6) {
               if(PNG) png(paste0(Dir, cA[[i]]$Name, ".png"), width = 2048, height = 2048 * ifelse( i == 6, 0.65, 1), bg = 'grey')  else dev.new(width = 40, height = 30)
               if(i %in% 1:6) {
                   # autoLayer fails for cA[[5]], giving a partial SoCal hi-rez result
                   Imap::plotGIS(long = cA[[i]]$long, lat = cA[[i]]$lat, levels.contour = { if(CONTOUR) { c(-60, -80, -100, -120, -140, seq(0, -2000, by = -200)) } else NULL }, autoLayer = ifelse( i == 6, TRUE, FALSE))
               } else  
                   Imap::imap(list(world.h.land), longrange = cA[[i]]$long, latrange = cA[[i]]$lat, zoom = FALSE)
               
               BubDATA <- NULL
               BubGroup <- NULL
               for( j in LenNum:1) {
                  TMP <- DatG[!duplicated(DatG$KEY) & !(DatG[, paste0("Total_sp_wt_LR_kg_", j)] %in% 0), c("Longitude_dd", "Latitude_dd", paste0("Total_sp_wt_LR_kg_", j))]
                  names(TMP) <- c('X', 'Y', 'Z')
                  BubDATA <- rbind(BubDATA, TMP)
                  BubGroup <- c(BubGroup, rep(j, nrow(TMP)))
              }
              LenMaxCols <- list("red", c("blue", "red"), c("magenta", "blue", "red"))[[LenNum]]
              if(i %in% 1:6)
                  JRWToolBox::plot.bubble.zero.cross(BubDATA, group = BubGroup, add=T, scale.size = SCALE.SIZE, cross.cex = ifelse(PNG, 1, 0.2), cross.col = ifelse(i == 1, 'darkcyan', 'cyan'),
                          fill.col = LenMaxCols, border.col = LenMaxCols, fill.col.alpha = 0.75, border.col.alpha = 0.75, legend = F)
                          
              for ( j in 1:length(AREAS)) {
                  DATA <- SP.Results.Biomass.Stacked.Dpth[SP.Results.Biomass.Stacked.Dpth$Area %in% AREAS[j], ]
                  if(BIOMASS) {
                      # points(DATA[, c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$CPUE.kgPerh))) + 14], pch = 19, cex=1.75)
                        points(DATA[!duplicated(paste(DATA$X, DATA$Y)), c("X","Y")], col = Col(n = 12)[round(2 * mean(log(DATA$CPUE.kgPerh))) + 14], pch = 19, cex = 1.6)
                        points(DATA[!duplicated(paste(DATA$X, DATA$Y)), c("X","Y")], col = c('grey40',"cyan4","gold4",rainbow(11))[sample(14, 1)], pch = sample(c(15,17,19),1), cex = 0.6)
                   } else {
                        TF <- AreaCol$Area %in% AREAS[j]
                        points(DATA[!duplicated(paste(DATA$X, DATA$Y)), c("X","Y")], col = col.alpha(AreaCol$Col1[TF], 0.6), pch = AreaCol$Shape1[TF], cex = ifelse(PNG, 2.1, 1.2)) # 1.6, 0.7
                        points(DATA[!duplicated(paste(DATA$X, DATA$Y)), c("X","Y")], col = col.alpha(AreaCol$Col2[TF], 0.6), pch = AreaCol$Shape2[TF], cex = ifelse(PNG, 1.1, 0.7)) # 0.6, 0.2
                  }
              }
              if(PLOTLOWAREAS) {
                   lowAREAS <- unique(SP.Quant.Biomass.Stacked.Dpth$Area[!SP.Quant.Biomass.Stacked.Dpth$Area %in% AREAS])
                   lowN <- length(lowAREAS)
                   for ( j in 1:lowN ) {
                      Col.rnd <- c('grey40',"cyan4","gold4",rainbow(11))[sample(14,1)]
                      DATA <- SP.Results.Biomass.Stacked.Dpth[SP.Results.Biomass.Stacked.Dpth$Area %in% lowAREAS[j],]
                      points(DATA[, c("X","Y")], col = Col.rnd, pch = 15, cex = 0.7)
                   }
              }
              if(Region == "Other" & BIOMASS & POLYGONS)  
                    for( j in 1:11)  points(AreaGroup[[j]]$AreasPts[, c("X","Y")], col = 'red', pch = 19, cex=1.75)
                   
              if(DRAW.TRIANGULATION.MESH) {
                   SpatialIsoLL <- Spatial_List$MeshList$isotropic_mesh
                   SpatialIsoLL$loc <- JRWToolBox::UTM.to.LatLong(1000*SpatialIsoLL$loc)
                   INLA::plot.inla.mesh(SpatialIsoLL, draw.vertices = TRUE, lwd = 2, size = 10, add = TRUE)
                   # points(JRWToolBox::UTM.to.LatLong(1000*Spatial_List$loc_x), col = 'blue', pch = 19, cex=1)  # Add larger vertices (knots)
              }
              if(POLYGONS) {
                  for (G in 1:10)
                       polygon(AreaGroup[[G]]$Boundary, col=col.alpha('purple', 0.25))
              } 
              
              if(PNG) gof()
         }
                           
         COG_Table <- data.frame(RangeShifts$COG_Table)
         COG.LongLat <- UTM.to.LatLong(1000*data.frame(Eastings = COG_Table[COG_Table$m %in% 1, 'COG_hat'], Northings = COG_Table[COG_Table$m %in% 2, 'COG_hat']))
      
         Year.Index.EffArea.COG <- data.frame(Year = IndexTable$Year, Biomass.Index.mt = IndexTable$Estimate_metric_tons, Biomass.Index.SE.mt = IndexTable$SD_mt, 
             EffectiveArea = RangeShifts$EffectiveArea_Table[, 'EffectiveArea'], EffectiveArea.SE = RangeShifts$EffectiveArea_Table[, 'SE'], 
             COG.Eastings = COG_Table[COG_Table$m %in% 1, 'COG_hat'], COG.Eastings.SE = COG_Table[COG_Table$m %in% 1, 'SE'], 
             COG.Northings = COG_Table[COG_Table$m %in% 2, 'COG_hat'], COG.Northings.SE = COG_Table[COG_Table$m %in% 2, 'SE'], 
             COG.Long = COG.LongLat$Long, COG.Lat = COG.LongLat$Lat)
             
         rm(RangeShifts, COG_Table, COG.LongLat)
         save(Year.Index.EffArea.COG, file=paste0(DateFile, "Year.Index.EffArea.COG.RData"))
         # See 'Yearly table of abundance index, effective area, and COG.R' for plotting the Year.Index.EffArea.COG data
        
         # Save it all, now with diagnostics!
         save(list = c(ls(), names(.GlobalEnv)), file = paste0(DateFile, "Image.RData")) # Save files inside the function also!!!!!!
         
     } # End runDiagnostics 

}
                      
                      









