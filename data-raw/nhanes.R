library(tidyverse)
library(readxl)
library(usethis)

# 2009 - 2012
# Source: Obtained from the NHANES package in R. These data were originally assembled by Michelle Dalrymple of Cashmere High School and Chris Wild of the University of Auckland, New Zealand for use in teaching statistics.

load('data-raw/nhanes.rdata')

nhanes <- NHANES %>%
  select(c(ID, Age, Weight, Height, TotChol, Smoke100, Education)) %>%
  mutate(Smoke100 = ifelse(Smoke100 == "No", 0, 1)) %>%
  distinct %>%
  select(-ID)



write_csv(nhanes, "data-raw/nhanes.csv")
usethis::use_data(nhanes, overwrite = TRUE, compress = 'xz')
