context("misl: parameters")

test_that("MISL runs as expected with default parameters",{
  expect_silent(
    misl(nhanes)
  )
})



context("misl: checks")

complete_data <- nhanes[complete.cases(nhanes),]

test_that("MISL does not execute if the dataframe is complete.",{
  expect_error(
    misl(complete_data),
    "Your dataset is complete, no need for MISL!"
  )
})
