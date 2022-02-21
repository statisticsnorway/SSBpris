# Testing of price index calculations
#source("R/Calculation_funcs.R")

test_that("CalcInd returns correct value", {
  data(priceData)
  ind <- CalcInd(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", wVar = "weight", 
                 consumVar = "coicop", type = "dutot")
  expect_equal(as.numeric(ind[1]), 30.06709, tolerance = 1E-4)
})
