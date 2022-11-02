
#' Jevons index caluclation
#'
#' @param x Vector of base prices.
#' @param y Vector of prices in the statistical time period. 
#'
#' @return Price index (L-type)
#' @export
Jevons <- function(x, y){
  z <- y / x
  zz <- exp(mean(log(z[is.finite(log(z))]), na.rm=T)) #Tian Yi Stack overflow - ta ut 0 og NA verdi
  return(zz)
}

#' Dutot index calculation
#'
#' @param x Vector of base prices.
#' @param y Vector of prices in the statistical time period.
#' @param na.rm Logical on whether to remove missing values for x or y in the calculation.
#' @param ze.rm Logical on whether to remove missing values for the instrument variable for the calculations.
#'
#' @return Price index (L-type)
#' @export
Dutot <- function(x, y, na.rm = TRUE, ze.rm = TRUE){
  if (ze.rm) {
    zero <- (x != 0)
    x <- x[zero]
    y <- y[zero]
  }
  if (na.rm){
    naVect <- (!is.na(x) & !is.na(y))
    x <- x[naVect]
    y <- y[naVect]
  }
  return((sum(y, na.rm = T) / sum(x, na.rm = T)))
}

#' Carli index calculation
#'
#' @param x Vector of base prices.
#' @param y Vector of prices in the statistical time period. 
#' @param na.rm Logical on whether to remove missing values for x or y in the calculation.
#' @param ze.rm Logical on whether to remove missing values for the instrument variable for the calculations. 
#'
#' @return Price index (L-type)
#' @export
Carli <- function(x, y, na.rm = TRUE, ze.rm = TRUE){
  if (ze.rm) {
    zero <- (x != 0)
    x <- x[zero]
    y <- y[zero]
  }
  if (na.rm){
    naVect <- (!is.na(x) & !is.na(y))
    x <- x[naVect]
    y <- y[naVect]
  }
  return(sum(y / x) / length(y))
}


#' Calculation of the estimate for a price index
#' 
#' Calculation of a price index
#'
#' @param data The dataset
#' @param baseVar The variable name for the base goods price.
#' @param pVar The variable name for the goods price.
#' @param type The type of index to calculate. Choose from: 'carli', 'dutot', 'jevons'.
#' @param groupVar The variable used for grouping observations - sometimes called the elementary group
#' @param consumVar The variable used for grouping elementary groups. This may be the publishing level.
#' @param wVar The variable used to weight the elementary groups up to the consumer publishing level. 
#'
#' @return   The estimates for the index are returned as a vector of length equal to the number of groups (in groupVar or consumVar if specified).
#' @export
#'
#' @examples{
#' data(priceData)
#' # Calculate index for consumer groups
#' CalcInd(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", wVar = "weight", 
#'   consumVar = "coicop", type = "dutot")
#' CalcInd(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", wVar = "weight", 
#'   consumVar = "coicop", type = "carli")
#' CalcInd(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", wVar = "weight", 
#'   consumVar = "coicop", type = "jevons")
#'   
#' # Calculate index for elemenatry groups (weighted)
#' CalcInd(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", wVar = "weight", 
#'    type = "dutot")
#'    
#'    }
CalcInd <- function (data, baseVar, pVar, type, groupVar, consumVar = NULL, 
                     wVar = NULL) 
{
  if (!is.factor(data[, groupVar])) {
    data[, groupVar] <- factor(data[, groupVar])
  }
  if (is.null(consumVar) | missing(consumVar)) {
    warning("No consumer group variable was specified so an index was calculated for each elementary group.", 
            call. = FALSE)
  }
  if (is.null(wVar)) {
    warning("No weight variable was specified so all elements have been given a weight = 1", 
            call. = FALSE)
    ww <- rep(1, nrow(data))
  } else {
    ww <- data[, wVar]
  }
  
  # Check all weights are equal within groups
  wi <- ave(ww, data[, groupVar], FUN = mean)
  if (!all(wi == ww)){
    warning("Not all weights were equal within groups!! A mean weight is being used")
  }
  
  ### Lager sortering inn her eller lage en id
  
  # Check weights add to 1
  wg <- tapply(wi, data[, groupVar], FUN=mean)
  while (!isTRUE(all.equal(sum(wg), 1))){
    warning("Elementary group weights did not add to one and have been scaled.")
    wg <- wg/ sum(wg)
  }
  groups <- levels(data[, groupVar])
  
  
  #ind <- array(NA, length(groups), 1)
  ind <- rep(NA, length(groups))
  for (i in 1:length(groups)) {
    d <- data[, groupVar] == groups[i]
    if (type == "jevons") {
      Pi <- Jevons(data[d, baseVar], data[d, pVar])}
    if (type == "carli") {
      Pi <- Carli(data[d, baseVar], data[d, pVar])}
    if (type == "dutot") {
      Pi <- Dutot(data[d, baseVar], data[d, pVar])}
    
    #wi <- mean(ww[d])
    #ind[i] <- Pi * wi ##### Skal dette være wg???
    ind[i] <- Pi * wg[i] ### check this - legge inn sortering først!
  }
  if (!is.null(consumVar) | !missing (consumVar)) {
    tab <- unique(data.frame(data[, groupVar], data[, consumVar]))
    tab <- tab[order(tab[, 1]), ]
    if (length(tab[,2]) != length(ind)){
      ind <- ind[!is.na(ind)]
    }
    if (any(duplicated(tab[, 1]))) 
      stop("Some elementary groups contain elements that are found in several consumer groups. These should be exclusive.")
    return(tapply(ind, tab[, 2], sum)) ### ikke vektet til 1!!
  }
  else {
    return(ind)
  }
}



#' Calculation of variance/sigma squared for price index
#' 
#' Calculation of sigma squared for a price index.
#' 
#' 
#' @param data The dataset
#' @param baseVar The variable name for the base goods price.
#' @param pVar The variable name for the goods price.
#' @param groupVar The variable used for grouping observations - sometimes called the elementary group
#' @param type The type of index to calculate. Choose from: 'carli', 'dutot', 'jevons'
#' @return 
#'   \item{s2}{sigma squred values for each strata group}
#'   \item{ai}{adjustment factor used in calculations} 
#'   \item{zmat}{Matrix of z values which can be used to check for outliers}
#'   
#' @export
#'   
#' @examples {
#' # Call in test dataset for an index
#' data(priceData)
#' 
#' # Calculate s2 for an index
#' ss <- CalcIndS2(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "varenr", 
#' type = "carli")
#' ss <- CalcIndS2(data = priceData, baseVar = "b1", pVar = "p1", groupVar = "coicop", 
#' type = "jevons")
#' 
#' # print out s2 values
#' ss$s2
#' }
#' 
CalcIndS2 <- function (data, baseVar, pVar, groupVar, 
                       type = c("carli", "dutot", "jevons")) {
  if (type == "Carli") type <- "carli"
  if (type == "Dutot") type <- "dutot"
  if (type == "Jevons") type <- "jevons"
  if (is.null(type) | !type %in% c("carli", "dutot", "jevons")) 
    stop("you need to specify a type which is 'carli', 'dutot' or 'jevons'")
  x <- data[, baseVar]
  y <- data[, pVar]
  gg <- data[, groupVar]
  if (!is.factor(gg)) gg <- factor(gg)
  group <- levels(gg)
  s2 <- rep(NA, length(group))
  ai <- rep(NA, length(group))
  zmat <- NULL
  for (i in 1:length(group)) {
    d <- (gg == group[i])
    if (type == "carli") {
      Pi <- Carli(x[d], y[d])
      zij <- y[d]/x[d] - Pi
      ai[i] <- 1
    }
    if (type == "dutot") {
      Pi <- Dutot(x[d], y[d])
      zij <- y[d] - x[d] * Pi
      ai[i] <- mean(x[d[!is.na(x) & x != 0]])
    }
    if (type == "jevons") {
      Pi <- Jevons(x[d], y[d])
      zij <- log(y[d]/x[d]) - log(Pi)
      ai[i] <- 1/Pi^2
    }
    s2[i] <- stats::var(zij)
    if (length(zij) > 0) zmat <- rbind(zmat, data.frame(zij, group[i]))
  }
  colnames(zmat)[2] <- "group"
  names(s2) <- group
  names(ai) <- group
  return(list(s2 = s2, ai = ai, zmat = zmat))
}
