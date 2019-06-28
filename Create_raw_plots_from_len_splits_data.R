
      # Download with:
      JRWToolBox::gitAFile("John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/Create_raw_plots_from_len_splits_data.R", "script", File = "Create_raw_plots_from_len_splits_data.R", show = FALSE)
      
      # Directly edit with [using a properly configured gitEdit()]
      JRWToolBox::gitEdit(Create_raw_plots_from_len_splits_data, "John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/")  

      # ====================================================================================================================================
      
      # **** When using all the data available, figure 7 may take a long time to run. I use dplyr::sample_frac() below to sample the data when testing. ****
      
      if (!any(installed.packages()[, 1] %in% "devtools"))  
         install.packages('devtools')  
      devtools::install_github("John-R-Wallace-NOAA/JRWToolBox")
      JRWToolBox::lib("John-R-Wallace-NOAA/Imap")

            
      JRWToolBox::lib(dplyr)
      JRWToolBox::lib(TeachingDemos)

      # setwd("W:/ALL_USR/JRW/Assessment/WCGBTS Juvenile Species Habitat/")
      
      load("SSPN.LenSplits.RData")
      
          
       (HomeDir <- paste0(getwd(), '/'))
       spShortName <- 'SSPN'
       # (DateFile <- paste0(HomeDir, Sys.Date(), '_', spShortName,  "/"))
       (DateFile <- paste0(HomeDir, "2018-11-19_SSPN/"))
              
       Bubble.Size <- 0.005
       Bubble.Size.Zoomed.Out <- 0.025
       xDelta <- -3.4

            cA <- list()
       cA[[1]] <- list(Name = "1. WA & OR North", long = c(-126, -123.8), lat = c(45.75, 48.5))
       cA[[2]] <- list(Name = "2. OR Central", long = c(-126, -123.9), lat = c(43.0, 45.75))
       cA[[3]] <- list(Name = "3. OR South & CA North", long = c(-126, -124), lat = c(40.25, 43.0))
       cA[[4]] <- list(Name = "4. CA North & Central", long = c(-125.5, -122), lat = c(37.5, 40.25))
       cA[[5]] <- list(Name = "5. CA Central & South", long = c(-124, -120.5), lat = c(34.75, 37.5))
       cA[[6]] <- list(Name = "6. CA South - Landscape", long = c(-122, -116.8), lat = c(32, 34.75))
	   cA[[7]] <- list(Name = "7. Entire Coast", long = c(-127.3, -114.7), lat = c(31.5, 48.5))     

       
       
       DAT <- SSPN.LenSplits
       LenSplitsList = list(c(1,17), c(18, 29), c(30,35), c(36, 40), c(41, Inf))
       (LenNum <- length(LenSplitsList))
       (Extra.Group.Size = rep(1, LenNum + 1))
     
       (Figs <- list(1:7, 1:6, 7))[[3]]  # All 7 figures, just the first 6, or only the last one with the entire coast
       (PNG <- c(T, F)[1]) # TRUE = PNG; FALSE = Windows
       (CONTOUR <- c(T, F)[1])
        
       
       DirRaw <- paste0(DateFile, "Figs/", "Raw Data Bubble Plots", "/")
       dir.create(DirRaw, recursive = TRUE)
       print(DirRaw)
       
       Col <- colorRampPalette(colors = c("blue", "cyan", "green", "orange", "red"))
       LenCols <- Col(LenNum)

       # 7 for the entire coast
       for ( i in Figs) {
            if(PNG)  png(paste0(DirRaw, cA[[i]]$Name, ".png"), width = ifelse(i %in% 7, 6000, 2048), height = ifelse(i %in% 7, 6000, 2048) * ifelse( i == 6, 0.65, 1), bg = 'grey') else dev.new(width = 40, height = 30) 
            if(i %in% 1:6) 
               Imap::plotGIS(long = cA[[i]]$long, lat = cA[[i]]$lat, levels.contour = { if(CONTOUR) { c(-60, -80, -100, -120, -140, seq(0, -2000, by = -200)) } else NULL }, autoLayer = ifelse(i %in% 6, TRUE, FALSE))
            if( i %in% 7)  {       
               Imap::imap(longlat = list(Imap::world.h.land, Imap::world.h.borders), col = c("black", "cyan"), poly = c("grey40", NA), longrange = c(-140, -117), latrange = c(31.75, 48.2), 
                          zoom = FALSE, bg = "white", axes = "latOnly")  
               box(lwd = 5) 
               axis(1, at = -(124:117), labels = paste0(124:117, "W"), col = Col(LenNum)[1], cex.axis = 5, col.axis = Col(LenNum)[1], lwd = 5 , padj = 1, tck = -0.005)
      
            }
          
		   BubDATA <- NULL
           BubGroup <- NULL
           
           
          #  for( j in ncol(DAT)) {
           for( j in ncol(DAT):(ncol(DAT) - LenNum + 1)) {
              TMP <- DAT[!duplicated(DAT$KEY), c(9, 10, j)]
              # TMP <- dplyr::sample_frac(TMP, 0.01) # **** Un-comment to test on a small sample ****
              names(TMP) <- c('X', 'Y', 'Z')
              BubDATA <- rbind(BubDATA, TMP)
              BubGroup <- c(BubGroup, rep(j, nrow(TMP)))
            }
            
            if(length(Extra.Group.Size) == 1)
			      Extra.Group.Size <- c(1, rep(Extra.Group.Size, LenNum))
           
            # xDelta = ifelse(i %in% 1:6, 0, -2.6)
            # cross.cex = ifelse(i %in% 1:6, c(ifelse(PNG, 1, 0.2), rep(0, LenNum)), 0)
            JRWToolBox::plot.bubble.zero.cross(BubDATA, group = BubGroup, add = TRUE, scale.size = ifelse(i %in% 1:6, Bubble.Size, Bubble.Size.Zoomed.Out), cross.cex = 0, center.points = ifelse(i %in% 7, TRUE, FALSE), 
                center.cex = 1, xDelta = if(i %in% 1:6) 0 else rev(seq(0, xDelta * (LenNum - 1), by = xDelta)), fill.col = rev(LenCols), border.col = rev(LenCols), fill.col.alpha = rep(0.4, LenNum), 
                border.col.alpha =rep(0.4, LenNum), Extra.Group.Size = Extra.Group.Size, verbose = ifelse(i == 7, TRUE, FALSE), legend = F)
            if(i %in% 1:6)        
                points(DAT[DAT$Total_sp_wt_kg %in% 0, c("Longitude_dd", "Latitude_dd")], pch = 3, col = ifelse(i == 1, 'darkcyan', 'cyan'), cex = ifelse(PNG, 1, 0.2), lwd = 1)        
            if(i == 1)  
               TeachingDemos::subplot( { par(cex = 2); pie(rep(1, LenNum), col = LenCols, labels = c("1-17 cm", "18-29 cm", "30-35 cm", "36-40 cm", "41-Inf cm")) },
                  x=grconvertX(c(0.65, 0.95), from='npc'), y=grconvertY(c(0.65, 0.85), from='npc'), type='fig', pars=list( mar=c(0,0,1,0) + 0.1) )    
            if(i == 7)  
               TeachingDemos::subplot( { par(cex = 5); pie(rep(1, LenNum), col = LenCols, labels = c("1-17 cm", "18-29 cm", "30-35 cm", "36-40 cm", "41-Inf cm")) },
                  x=grconvertX(c(0.67, 0.97), from='npc'), y=grconvertY(c(0.64, 0.82), from='npc'), type='fig', pars=list( mar=c(0,0,1,0) + 0.1) )         
            
            if(PNG) gof()
      }
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
