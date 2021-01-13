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