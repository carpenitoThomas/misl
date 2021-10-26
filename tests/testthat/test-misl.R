context("misl: parameters")

reduced_data <- nhanes[1:100,]

test_that("MISL runs as expected with default parameters",{
  expect_silent(
    misl(reduced_data)
  )
})

test_that("MISL runs with a custom number of iterations",{
  expect_silent(
    misl(reduced_data, maxit = 1)
  )
})

test_that("MISL runs with a custom number of multiply imputed datasets",{
  expect_silent(
    misl(reduced_data, m = 1)
  )
})

test_that("MISL runs with a custom number of multiply imputed datasets",{
  expect_silent(
    misl(reduced_data, maxit = 1, m = 1, quiet = FALSE)
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
