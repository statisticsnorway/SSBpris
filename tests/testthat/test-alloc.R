test_that("Allocation using FillStrataInd works", {
  
  data(priceData)
  
  s2 <- CalcIndS2(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", 
                  type = "carli")$s2
  # Calculate bhg matrix
  ngh <- as.matrix(table(priceData$nace3, priceData$varenr))
  mh <- matrix(rep(table(priceData$nace3), 200), 5, 200)
  bhg <- ngh/mh 
  
  # Specify variance function for n (number of observations) - unweighted
  VarP <- function(n){ return(sum(s2 / n , na.rm = TRUE)) }
  
  # Run algorithm filling up one at a time for sample size of 300
  a <- FillStrataInd(totm = 300, bhg, totVar = VarP, min_n = 1, steps = 1, 
                     direction = "up")
  
  expect_equal(as.numeric(a$alloc[1]), 59)
})


