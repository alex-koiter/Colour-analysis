# Colour-analysis
Scripts to process spectral data from the ASD field spec pro to several different colour coefficients. This script assumes that you have taken 10 scans of each sample and the data has been exported as a .txt file. Please see the SOP for information on how to use the instrument and export the data.

There are two scripts that you can use that produce nearly identical results, the only difference is the R G B colour coefficients. 

1) The colour_coef.R script uses the colorscience R package to calculate all the colour coefficients with the exception of the h and c which are calculated manually. 

2) The colour_coef_kui.R calculates everything manually. The R G B values in this script is the average reflectance data in the ranges 450 to 520 nm, 520 to 600 nm, and 630 to 690 nm. This method produces different results as compared to the other script.

Last thing to note is that some groups report h in radians and others in degrees. I am not sure if one is better than the other. Currently the default is to report radians but the script does contain the code to calculate h in degrees.
