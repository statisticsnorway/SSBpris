
#' Optimal allocation of sample sizes by filling up or down for an index
#' 
#' This function finds an optimal allocation by filling up or down one (or
#' several) units at a time in a stratum(a).
#' 
#' 
#' @param totm The total desired sample size as the number of units to select 
#' (NOT the number of desired observations).
#' @param bhg A matrix giving the average number of observations in each elementary 
#' group in each stratum. See Zhang 2015 for more details.
#' @param totVar The variance to the index as a function of n (number of observations).
#' @param min_n A minimum sample size in each stratum. This can be a single number 
#' used in all strata or a vector of numbers (one for each stratum).
#' @param max_n A maximum sample size in each stratum. This can be a single number 
#' used in all strata or a vector of numbers (one for each stratum).
#' @param steps The number of strata to fill up at each iteration. Default is set to 1.
#' @param rand specify the number of strata to consider for random selection fill up. 
#' For example rand = 10 will randomly select 1 (or the number specified in steps) strata 
#' to fill up based on the top 10 using probabilities proportional to the difference 
#' between the variances. If missing, the algorithm will chose the top strata. Currently 
#' not programmed for direction = 'down'
#' @param direction Specify the direction the algorithm should work: "up" to
#' fill up from 0 or a minimum value or "down" to remove one unit at a time
#' from a maximum starting point.
#' @return 
#' \item{alloc}{A vector with the new sample sizes in each strata. }
#' \item{totvar}{Total variance}
#' 
#' @export
#' @examples{
#'  
#' # call in test dataset
#' data(priceData)
#' 
#' # Calculate index for consumer groups
#' CalcInd(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", wVar = "weight", 
#'         consumVar = "coicop", type = "dutot")
#' 
#' # Calculate s2 for index
#' s2 <- CalcIndS2(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", 
#'                 type = "carli")$s2
#' 
#' # Calculate bhg matrix
#' ngh <- as.matrix(table(priceData$nace3, priceData$varenr))
#' mh <- matrix(rep(table(priceData$nace3), 200), 5, 200)
#' bhg <- ngh/mh 
#' 
#' # Specify variance function for n (number of observations) - unweighted
#' VarP <- function(n){ return(sum(s2 / n , na.rm = TRUE)) }
#' 
#' # Run algorithm filling up one at a time for sample size of 300
#' a <- FillStrataInd(totm = 300, bhg, totVar = VarP, min_n = 1, steps = 1, 
#'  direction = "up")
#' a
#' 
#' # Run algorithm filling down one at a time
#' max_n <- table(priceData$nace3)
#' a <- FillStrataInd(totm = 300, bhg = bhg, totVar = VarP, min_n = 1, max_n = max_n, 
#'  steps = 1, direction = "down")
#' a
#' }
FillStrataInd <- function(totm, bhg, totVar, min_n, max_n, steps = 1, rand, direction = "up"){
  if (missing(min_n)) min_n <- rep(1, nrow(bhg))
  if (length(min_n) == 1) min_n <- rep(min_n, nrow(bhg))
  if (missing(max_n) & direction == "down") stop("max_n must be specified for down direction")
  if (direction == "down" & !missing(rand)) stop("rand option currently not available for down direction")
  if (missing(max_n)) max_n <- rep(Inf, nrow(bhg))
  if (!is.vector(max_n)) max_n <- as.vector(max_n)
  if (any(min_n == 0)){
    min_n[min_n==0] <- 1
  }
  
  if (direction == "up") {
    mstart <- pmax(1, min_n)
    varvect <- NULL
    while(sum(mstart) < totm){
      nexp <- colSums(mstart * bhg)
      var1 <- totVar(n = nexp)
      varvect <- c(varvect, var1)
      vtemp <- sapply(1:nrow(bhg), FUN = function(x) {
        nexp1 <- nexp + bhg[x, ]
        totVar(n = nexp1) 
      })
      
      d <- var1 - vtemp
      top <- order(-d)
      if (missing(rand)){
        top <- top[mstart[top] < max_n[top]]
        top <- top[steps]
        if (any(d[top] < 0)) warning(paste("step to total: ", sum(mstart),  " increased the minimizing function", sep=""))
      } else {
        top <- top[mstart[top] < max_n[top]][1:rand]
        d[top < 0] <- 0
        p <- d[top]/sum(d[top])
        if(all(is.na(p))) { #what about if more than steps are <0?
          top <- top[steps] #force to add one if all options are a bad solution!#?
          warning(paste("step to total: ", sum(mstart),  " increased the minimizing function", sep=""))
        } else {
          top <- sample(top, size = steps, prob = p)}
      }
      mstart[top] <- mstart[top] + 1
    }
    
  } else if (direction == "down") {
    mstart <- max_n
    while(sum(mstart) > totm){
      nexp <- colSums(mstart * bhg)
      var1 <- totVar(n = nexp)
      vtemp <- sapply(1:nrow(bhg), FUN = function(x) {
        nexp1 <- nexp - bhg[x, ]
        totVar(n = nexp1) 
      })
      d <- var1 - vtemp
      top <- order(-d)
      top <- top[mstart[top] > min_n[top]]
      top <- top[steps]
      mstart[top] <- mstart[top] - 1
    }
  }
  
  names(mstart) <- rownames(bhg)
  nexp <- colSums(mstart * bhg)
  varf <- totVar(n = nexp)
  return(list(alloc = SpecialRound(mstart), totvar = varf))
}



#' Optimal allocation of sample sizes by swapping
#' 
#' This function finds an optimal allocation by swapping one unit at a time
#' between two strata. Beta version: use with caution - needs some extra testing
#' 
#' 
#' @param m Starting values for sample size in each stratum. This should sum to
#' the desired sample size.
#' @param bhg A matrix giving the average number of observations in each
#' elementary group in each stratum. See Zhang 2015 for more details.
#' @param totVar The variance to the index as a function of n (number of
#' observations). See example (?).
#' @param min_n A minimum sample size in each stratum. This can be a single
#' number used in all strata or a vector of numbers (one for each stratum).
#' @param max_n A maximum sample size in each stratum. This can be a single
#' number used in all strata or a vector of numbers (one for each stratum).
#' @param iter Number of unsuccessful iterations for the algorithm to try
#' before stopping. Default is 100.
#' @return Returns a vector with the new sample sizes in each strata
#' 
#' @export
#' 
#' @examples{
#'  
#' # call in test dataset
#' data(priceData)
#' 
#' # Calculate s2 for index
#' s2 <- CalcIndS2(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", 
#'                 type = "carli")$s2
#' 
#' # Calculate bhg matrix
#' ngh <- table(priceData$nace3, priceData$varenr)
#' mh <- matrix(rep(table(priceData$nace3), 200), 5, 200)
#' bhg <- ngh/mh 
#' 
#' # Specify variance function for n (number of observations) - unweighted
#' VarP <- function(n){
#'   return(sum(s2 / n , na.rm = TRUE))
#' }
#' 
#' # Start values for sample size in each strata with equal numbers (adding to 300) 
#' m <- rep(300/5, 5)
#' 
#' # Run Swap strata
#' a <- SwapStrataInd(m = m, bhg = bhg, totVar = VarP, iter = 100)
#' a
#' }
#' 
SwapStrataInd <- function(m, bhg, totVar, min_n, max_n, iter = 100){
  if (missing(min_n)) min_n <- rep(1, length(m))
  if (missing(max_n)) max_n <- rep(Inf, length(m))
  i <- 1:length(m)
  mstart <- m
  teller <- 0
  teller2 <- 0
  olds2 <- totVar(colSums(mstart * bhg))
  while(teller < iter){
    ss <- sample(i, 2)
    mnew <- mstart
    mnew[ss[1]] <- mnew[ss[1]] + 1
    mnew[ss[2]] <- mnew[ss[2]] - 1
    if (all(mnew >= min_n) & all(mnew <= max_n)) {
      nys2 <- totVar(colSums(mnew * bhg))
      if (olds2 > nys2) {
        mstart <- mnew
        olds2 <- nys2
        teller <- 0
        teller2 <- teller2 + 1
      } else{
        teller <- teller + 1
      }
    }
  }
  cat("There has been ", teller2, " succesful iterations. \n", sep="") 
  return(mstart)
}


#' Internal allocation function
#' 
#' Not sure if used in price index ...
#'
#' @param s2 Sigma squared values
#' @param w Weights vector
#' @param ai Adjustment factor
#' @param totn Total sample size
#'
#' @return ni 
AllocTot <- function(s2, w, ai, totn){
  ws <- w * sqrt(s2) / sqrt(ai)
  ni <- as.vector(totn * ws / sum(ws, na.rm=T))
  return(ni)
}

#' Internal allocation M function
#' 
#' Not sure if actually in use in price index
#'
#' @param bhgData Data giving the average number of price observations in the stratum for group.
#' @param n0 Allocation
#'
#' @return m
AllocM <- function(bhgData, n0){
  bhg <- apply(bhgData, 1:2, mean)
  vh <- rep(NA, nrow(bhg))
  for (i in 1:nrow(bhg)){
    vbh <- stats::cov(t(bhgData[i, , ]))
    vh[i] <- t(rep(1, ncol(bhg)) %*% vbh %*% rep(1, ncol(bhg)))
  }
  invvh <- solve(diag(vh)) # "solve" takes inverse of matrix
  m <- invvh %*% bhg %*% solve((t(bhg) %*% invvh %*% bhg)) %*% n0
  return(m)
}


#' Rounding strata sample sizes
#' 
#' Function for rounding strata sample sizes to ensure the total sample size
#' remains the same
#' 
#' 
#' @param x vector of unrounded strata sample sizes
#' @return vector with round strata sizes where the total sample size is held
#' constant.
#' 
#' @export
#' 
#' @examples{
#'  
#' jj <- c(4.2, 4.2, 4.2, 4.3, 4.3, 4.4, 4.4)
#' SpecialRound(jj)
#' }
SpecialRound <- function (x){
  uk <- x - floor(x)
  difference <- sum(x) - floor(sum(x))
  mynumber <- ifelse(difference < 0.5, ifelse(isTRUE(all.equal(sum(uk), round(sum(uk)))), 
                                              round(sum(uk)), floor(sum(uk))), ceiling(sum(uk)))	
  order.x <- order(-uk)
  x <- x[order(-uk)]
  x <- c(ceiling(x[if(mynumber==0) 0 else seq(mynumber)]), 
         floor(x[(mynumber + 1):length(x)])) 
  return(x[order(order.x)])
}
