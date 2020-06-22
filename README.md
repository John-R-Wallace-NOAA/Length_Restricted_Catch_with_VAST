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

(As of 5 June 2020, on Tantalus (a Linux server) R ver 3.4.3 (/opt/microsoft/ropen/3.4.3/lib64/R/bin/R) is needed to access a compatible GDAL app that works with the rgdal package (ver 1.4-4).)

INLA package from CRAN info:

      Package:           INLA
      Type:              Package
      Title:             Full Bayesian Analysis of Latent Gaussian Models using Integrated Nested Laplace Approximations
      Version:           18.07.12
      Date:              2018-07-12 (Version_18.07.12)
      Built:             R 3.4.3; ; 2019-06-10 17:12:59 UTC; unix
      


