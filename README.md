# Length_Restricted_Catch_with_VAST
Using VAST on Length Restricted Catch to define Essential Fish Habitat (EFH)

    gitEdit(Run_Data_and_VAST_by_Species, 'John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/')
    JRWToolBox::gitAFile("John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/VAST.Length.Restricted.Catch.R", show = F)
    gitEdit(VAST.Length.Restricted.Catch, 'John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST/master/')
    gitEdit(YearlyResultsFigures)  # From JRWToolBox
    gitEdit(plotGIS, "John-R-Wallace-NOAA/Imap/master/R/") # From Imap

Current call to revert to this Commit in the future with build date and time:

devtools::install_github('James-Thorson/FishStatsUtils', ref = '437341d55a23241005c171b91b4cc104238080e7')  # R 3.4.3; ; 2019-06-10 17:02:24 UTC; unix

devtools::install_github('james-thorson/VAST', ref = 'c2c43293421f4f2f257ecd4033211ab289553357')  # R 3.4.3; ; 2019-06-10 17:15:17 UTC; unix

On Tantalus need to use R ver 3.4.5 (/opt/microsoft/ropen/3.4.3/lib64/R/bin/R) to get a compatiable rgdal package.
