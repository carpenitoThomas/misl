library(data.table)
library(tidyverse)
library(usethis)

# load data set and take a peek
abalone <- read.csv("~/Desktop/abalone.data", header=FALSE)

# add the column names
colnames(abalone) <- c("Sex", "Length", "Diameter", "Height", "Whole_Weight", "Shuck_Weight", "Viscera_Weight", "Shell_Weight", "Rings")

# Cleanup the abaolone data to make available for our simulations

abalone <- abalone %>%
  mutate(Age = Rings + 1.5,
         Sex = as.factor(Sex),
         Older_12 = as.integer(Age > 12)
  ) %>%
  select(-c(Rings, Age))

abalone <- as_tibble(abalone)

write_csv(abalone, "data-raw/abalone.csv")
usethis::use_data(abalone, overwrite = TRUE, compress = 'xz')
