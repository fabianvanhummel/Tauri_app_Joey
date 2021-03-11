source("fun/armorSet.R", chdir = TRUE)
library(testthat)

test_that("empty list",{
  
    expect_equal(armorSet(list(),list()),list())
})
