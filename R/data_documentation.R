
#' Price test dataset
#' 
#' This dataset is a test dataset containing a small sample of 1000 goods
#' and prices. Variables include goods price over 3 periods and the base price
#' for the three equivalent periods.
#'
#' @usage data(priceData)
#' @format Dataset containing the variables::
#' \describe{
#'   \item{dufNr}{company number}
#'   \item{nace5}{industry group (5 digits)} 
#'   \item{nace3}{industry group (3digits)} 
#'   \item{varenr}{identity number for the specific goods}
#'   \item{coicop}{grouping variable for goods which are similar. This is the publishing level.} 
#'   \item{weight}{Variable specifying the weight that should be given based on the type of goods.} 
#'   \item{p1}{price in the first period}
#'   \item{p2}{price in the second period} 
#'   \item{p3}{price in the third period}
#'   \item{b1}{base price which should be used in the first period}
#'   \item{b2}{base price which should be used in the second period}
#'   \item{b3}{base price which should be used in the third period}
#' }
"priceData"
