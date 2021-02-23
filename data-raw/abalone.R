library(data.table)
library(tidyverse)
library(usethis)
library(mice)
set.seed(1234)

# load data set and take a peek
abalone <- load(file = "data-raw/abalone.Rdata")

# add the column names
colnames(abalone) <- c("Sex", "Length", "Diameter", "Height", "Whole_Weight", "Shuck_Weight", "Viscera_Weight", "Shell_Weight", "Rings")

# Cleanup the abaolone data to make available for our simulations

abalone <- abalone %>%
  dplyr::mutate(Age = Rings + 1.5,
         Sex = as.factor(Sex),
         Older_12 = as.integer(Age > 12)
  ) %>%
  dplyr::select(-c(Rings, Age, Shuck_Weight, Viscera_Weight, Shell_Weight))

mypatterns <- expand.grid(Sex = 0:1, Length = 0:1, Diameter = 0:1, Height = 0:1, Whole_Weight_Pred = 0:1, Older_12 = 0:1)
mypatterns <- mypatterns[sample(1:nrow(mypatterns), replace = FALSE, 8),]
mypatterns <- mypatterns[rowSums(mypatterns) != 0,]

amputed_abalone <- mice::ampute(abalone, mech = "MCAR", prop = .50)

abalone <- as_tibble(amputed_abalone$amp)

write_csv(abalone, "data-raw/abalone.csv")
usethis::use_data(abalone, overwrite = TRUE, compress = 'xz')
