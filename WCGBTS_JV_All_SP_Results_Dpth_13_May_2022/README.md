

I had to make a loop to extract this data from the saved VAST runs, but here we are.

The data for each species (see attached) looks like:

             x        y         X        Y    X2003     X2004     X2005     X2006     X2007     X2008     X2009
    1 759.1116 3745.335 -120.2006 33.81650 -7.52835 -7.404844 -9.012492 -8.245524 -7.921544 -7.287862 -6.984567
    2 761.1116 3745.335 -120.1790 33.81601 -7.52835 -7.404844 -9.012492 -8.245524 -7.921544 -7.287862 -6.984567
    3 763.1116 3745.335 -120.1574 33.81551 -7.52835 -7.404844 -9.012492 -8.245524 -7.921544 -7.287862 -6.984567

          X2010     X2011     X2012     X2013     X2014     X2015     X2016     X2017     X2018
    1 -4.285058 -7.106127 -6.830975 -4.971122 -7.217141 -8.433116 -6.170058 -8.276747 -7.526347
    2 -4.285058 -7.106127 -6.830975 -4.971122 -7.217141 -8.433116 -6.170058 -8.276747 -7.526347
    3 -4.285058 -7.106127 -6.830975 -4.971122 -7.217141 -8.433116 -6.170058 -8.276747 -7.526347

x and y are Eastings and Northings, X and Y are longitude and latitude, and the years of data are in logged (kilograms per square kilometer).
So after exponentiation, convert to grams per hectare used in the paper by multiplying by ten, since 1000 (gram to kg) divided by 100 (kilometer2 to hectare) = 10

Using Sablefish as an example:

    10 * range(exp(SABL_SP.Results.Dpth[, -(1:4)]))
    [1] 1.914617e-04 2.108660e+01

Which is the [0, 21]  grams per hectare range for Sablefish seen in Fig. 4 of the paper.

Similar conversions and use of the range of the data can be seen in the YearlyResultsFigures() function here:

     https://github.com/John-R-Wallace-NOAA/JRWToolBox/blob/master/R/YearlyResultsFigures.R

Note that there is newer version for VAST 3.X:

    https://github.com/John-R-Wallace-NOAA/JRWToolBox/blob/master/R/YearlyResultsFigure_VAST3X.R


The calling of  YearlyResultsFigures()  is in  Run_Data_and_VAST_by_Species() here:

    https://github.com/John-R-Wallace-NOAA/Length_Restricted_Catch_with_VAST
