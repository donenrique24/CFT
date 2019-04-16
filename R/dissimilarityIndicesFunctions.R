########################################################
# R functions to estimate dissimilarity indices.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2019
########################################################


#'
#' Data from Urban Environments in Nancy, France
#'
#' A subset of a two-phase inventory that was carried out
#' in Nancy, Eastern France, in 2015
#'
#' @docType data
#'
#' @usage data(subsetUrbanEnvironmentNancy)
#'
#' @keywords datasets
#'
#' @examples
#' data(subsetUrbanEnvironmentNancy)
"subsetUrbanEnvironmentNancy"



#'
#' Instantiate an Estimator of Dissimilarity Indices
#'
#' It creates a java.object that will compute the estimate of dissimilarity
#' indices of Simpson, Sorensen and nestedness.
#'
#' @return a java.object instance
#'
#' @export
createDissimilarityIndicesEstimator <- function() {
  J4R::checkIfExtensionsContain(myJavaLibrary = "mrnf-foresttools.jar",
                                packageName = "CFT",
                                automaticRestart = TRUE)
  print("Instantiating the estimator of dissimilarity indices...")
  msi <- J4R::createJavaObject("canforservutility.biodiversity.indices.MultipleSiteIndex")
  print("Done.")
  return(msi)
}



#'
#' Creates a Sample
#'
#' It creates a sample from which the dissimilarity indices
#' can be estimated.
#'
#'@param dataSet a data.frame object
#'@param plotIdField a character string which stands for the field that contains the plot ids.
#'@param speciesIdField a character string which stands for the field that contains the species names.
#'
#'@return a java.object instance that represents a Map object in the Java environment
#'
#'@export
createSample <- function(dataSet, plotIdField, speciesIdField) {
  sample <- J4R::createJavaObject("java.util.HashMap")

  for (plotId in unique(dataSet[,plotIdField])) {

    releve.i <- dataSet[which(dataSet[,plotIdField] == plotId),]
    java.plot <- J4R::createJavaObject("java.util.ArrayList")
    speciesListBefore <- releve.i[,speciesIdField]
    speciesListAfter <- speciesListBefore[which(!is.na(speciesListBefore))]
    if (length(speciesListAfter) < length(speciesListBefore) && length(speciesListBefore) > 1) {
      stop(paste("There seems to be some NA in the species list of plot ", plotId, sep=""))
    } else {
      if (length(speciesListAfter) > 0) {
        J4R::callJavaMethod(java.plot, "add", as.character(speciesListAfter))
      } else {
        print(paste("This plot is empty ", plotId, sep=""))
      }
      J4R::callJavaMethod(sample, "put", as.character(plotId), java.plot)
    }
  }
  return(sample)
}



#'
#' Estimate Dissimilarity Indices
#'
#' It estimates the dissimilarity indices of Simpson, Sorensen and
#' nestedness from a sample.
#'
#' @param dissimilarityEstimator a java.object that can estimate the dissimilarity indices. It
#' should be generated using the createDissimilarityIndicesEstimator function
#' @param sample a java.object instance that stands for a Map in the Java environment. It should be
#' generated using the createSample function
#' @param populationSize the number of units (plots) in the population
#'
#' @return a data.frame object with the estimated dissimilarity indices and their standard errors
#'
#' @export
getDissimilarityEstimates <- function(dissimilarityEstimator, sample, populationSize) {
  n <- J4R::callJavaMethod(sample, "size")
  messageToBeDisplayed <- paste("Estimating dissimilarity from a sample of ", n, " plots...", sep="")
  if (n > 500) {
    messageToBeDisplayed <- paste(messageToBeDisplayed, " This may take some time!", sep="")
  }
  message(messageToBeDisplayed)
  leaveOneOutMode <- J4R::createJavaObject("canforservutility.biodiversity.indices.MultipleSiteIndex$Mode", "LeaveOneOut")
  resultMap <- J4R::callJavaMethod(dissimilarityEstimator, "getDissimilarityIndicesMultiplesiteEstimator", sample, as.integer(populationSize), TRUE, leaveOneOutMode)

  alphaEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.MultipleSiteIndex$DiversityIndex", "Alpha")
  gammaEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.MultipleSiteIndex$DiversityIndex", "Gamma")
  betaEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.MultipleSiteIndex$DiversityIndex", "Beta")

  alphaEstimate <- J4R::callJavaMethod(resultMap, "get", alphaEnum)
  gammaEstimate <- J4R::callJavaMethod(resultMap, "get", gammaEnum)
  betaEstimate <- J4R::callJavaMethod(resultMap, "get", betaEnum)

  Alpha <- J4R::callJavaMethod(J4R::callJavaMethod(alphaEstimate, "getMean"), "getSumOfElements")
  Gamma <- J4R::callJavaMethod(J4R::callJavaMethod(gammaEstimate, "getMean"), "getSumOfElements")

  meanBetaEstimate <- J4R::callJavaMethod(betaEstimate, "getMean")
  varBetaEstimate <- J4R::callJavaMethod(betaEstimate, "getVariance")

  Simpson <- J4R::callJavaMethod(meanBetaEstimate, "getSumOfElements", as.integer(0), as.integer(0), as.integer(0), as.integer(0))
  varSimpson <- J4R::callJavaMethod(varBetaEstimate, "getSumOfElements", as.integer(0), as.integer(0), as.integer(0), as.integer(0))
  stdErrSimpson <- varSimpson^.5

  Sorensen <- J4R::callJavaMethod(meanBetaEstimate, "getSumOfElements", as.integer(1), as.integer(1), as.integer(0), as.integer(0))
  varSorensen <- J4R::callJavaMethod(varBetaEstimate, "getSumOfElements", as.integer(1), as.integer(1), as.integer(1), as.integer(1))
  stdErrSorensen <- varSorensen^.5

  Nestedness <- J4R::callJavaMethod(meanBetaEstimate, "getSumOfElements", as.integer(2), as.integer(2), as.integer(0), as.integer(0))
  varNestedness <- J4R::callJavaMethod(varBetaEstimate, "getSumOfElements", as.integer(2), as.integer(2), as.integer(2), as.integer(2))
  stdErrNestedness <- varNestedness^.5

  return(data.frame(n, Simpson, varSimpson, stdErrSimpson, Sorensen, varSorensen, stdErrSorensen, Nestedness, varNestedness, stdErrNestedness, Alpha, Gamma))
}
