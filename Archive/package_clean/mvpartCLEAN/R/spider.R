#'
#' Spider Data
#' 
#' Data set on abundances of spiders and environmental predictors. All variables
#' are rated on a 0-9 scale.
#' 
#' @docType data
#' 
#' @name spider
#' 
#' @usage data(spider)
#' 
#' @format A data frame with 28 observations with 12 species and six
#' environmental predictors.
#' 
#' @details Provided with the original package. The first 12 columns appears to
#' represent spider species since their names look like abbreviated binomial
#' names (for instance, the first name, arct.lute, might be Arctosa lutetiana,
#' whereas the second name, pard.lugu, might be Pardosa lugubris). The last six
#' columns appear to be abiotic (two of them: water, sand) and biotic (four of
#' them: moss, reft, twigs, herbs) environmental descriptors.
#' 
#' @source Van der Aart and Smeeck-Enserink 1975.
#' 
#' @references
#' Van der Aart, P. J. and N. Smeeck-Enserink. 1975. Correlations between
#' distributions of hunting spiders (Lycosidae, Ctenidae) and environmental
#' characteristics in a dune area. Netherlands Journal of Zoology 25: 1-45.
#' 
#' These data were analysed using multivariate trees in De'ath, G. 2002.
#' Multivariate Regression Trees: A New Technique for Modelling
#' Species-Environment Relationships. Ecology 83(4): 1103-1117
#' 
#' @examples ## Load and print the data frame:
#' data("spider")
#' spider
#' 
#' @keywords spider
#' 
NULL
