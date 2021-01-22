#' @title nhanes
#' @description A subset of the NHANES 2009-2012 dataset (with adjusted weighting). The column descriptions are from the NHANES package itself.
#' @format A data frame with 10000 rows and 6 variables:
#' \describe{
#'   \item{\code{Age}}{integer Age in years at screening of study participant. Note: Subjects 80 years or older were recorded as 80.}
#'   \item{\code{Weight}}{double Weight in kg}
#'   \item{\code{Height}}{double Standing height in cm. Reported for participants aged 2 years or older.}
#'   \item{\code{TotChol}}{double Total HDL cholesterol in mmol/L. Reported for participants aged 6 years or older.}
#'   \item{\code{Smoke100}}{double Study participant has smoked at least 100 cigarettes in their entire life. Reported for participants aged 20 years or older as Yes or No.}
#'   \item{\code{Education}}{integer Educational level of study participant Reported for participants aged 20 years or older. One of 8thGrade, 9-11thGrade, HighSchool, SomeCollege, or CollegeGrad.}
#'}
"nhanes"

#' @title abalone
#' @description Predicting the age of abalone from physical measurements. The age of abalone is determined by cutting the shell through the cone, staining it, and counting the number of rings through a microscope -- a boring and time-consuming task. Other measurements, which are easier to obtain, are used to predict the age. Further information, such as weather patterns and location (hence food availability) may be required to solve the problem. Data comes from the UCI machine learning repository. Further information can be found at:  \url{https://archive.ics.uci.edu/ml/datasets/Abalone}
#' @format A data frame with 4177 rows and 9 variables:
#' \describe{
#'   \item{\code{Sex}}{Male, Female, Infant}
#'   \item{\code{Length}}{Longest shell measurement (mm)}
#'   \item{\code{Diameter}}{Perpendicular to length (mm)}
#'   \item{\code{Height}}{With meat in shell (mm)}
#'   \item{\code{Whole_Weight}}{Whole abalone (g)}
#'   \item{\code{Shuck_Weight}}{Weight of meat (g)}
#'   \item{\code{Viscera_Weight}}{Gut weight after bleeding (g)}
#'   \item{\code{Shell_Weight}}{Weight after being dried (g)}
#'   \item{\code{Older_12}}{Whether the abalone is (strictly) greater than 12 years of age (0 = no, 1 = yes)}
#'}
#' @source Warwick J Nash, Tracy L Sellers, Simon R Talbot, Andrew J Cawthorn and Wes B Ford (1994) "The Population Biology of Abalone (_Haliotis_ species) in Tasmania. I. Blacklip Abalone (_H. rubra_) from the North Coast and Islands of Bass Strait",Sea Fisheries Division, Technical Report No. 48 (ISSN 1034-3288)
"abalone"


