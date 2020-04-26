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
#' It creates a java.object that can compute the estimate of dissimilarity
#' indices of Simpson, Sorensen and nestedness.
#'
#' @return a java.object instance
#'
#' @export
createDissimilarityIndicesEstimator <- function() {
  .connectToCFT()
  print("Instantiating the estimator of dissimilarity indices...")
  msi <- J4R::createJavaObject("canforservutility.biodiversity.indices.MultipleSiteIndex")
  print("Done.")
  return(msi)
}



#'
#' Create a Sample
#'
#' It creates a sample from which the dissimilarity indices can be estimated.
#'
#'@param dataSet a data.frame object
#'@param plotIdField a character string which stands for the field that contains the plot ids.
#'@param speciesIdField a character string which stands for the field that contains the species names.
#'
#'@return a java.object instance that represents a Map object in the Java environment
#'
#'@export
createSample <- function(dataSet, plotIdField, speciesIdField) {
  .connectToCFT()
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
#' This function computes an estimate of the dissimilarity indices of Simpson, Sorensen and
#' nestedness from a sample.
#'
#' The processing of this function is done in Java using the J4R package. In order to use this function,
#' the user must first run the createDissimilarityIndicesEstimator function which creates a Java object that
#' can estimate the dissimilarity. The result of the createDissimilarityIndicesEstimator function must be
#' stored in a variable and passed to this function.
#'
#' A sample of plots with species observation must also be intantiated using the createSample function.
#' The result of the createSample function is the second parameter of this function.
#'
#' Finally, the user must provide the population size, that is the number of plots that fit in this population.
#'
#'
#' @param dissimilarityEstimator a java.object that can estimate the dissimilarity indices. It
#' should be generated using the createDissimilarityIndicesEstimator function
#' @param sample a java.object instance that stands for a Map in the Java environment. It should be
#' generated using the createSample function
#' @param populationSize the number of units (plots) in the population
#'
#' @return a data.frame object with the estimated dissimilarity indices and their standard errors
#'
#' @examples
#'
#' ### An example using the subsetUrbanEnvironmentNancy dataset ###
#'
#' dataReleves <- CFT::subsetUrbanEnvironmentNancy
#'
#' dissEst <- createDissimilarityIndicesEstimator()
#' strataList <- unique(dataReleves$Stratum)
#' output <- NULL
#' baselga <- NULL
#' stratum <- strataList[1]
#'
#' for (stratum in strataList) {
#'   releve.s <- dataReleves[which(dataReleves$Stratum == stratum),]
#'   if (stratum == "forest") {
#'     populationSize <- 3089 * 10000 / (pi * 5^2)
#'   } else if (stratum == "parking") {
#'     populationSize <- 501 * 10000 / (pi * 5^2)
#'   } else {
#'     populationSize <- 100000
#'   }
#'
#'   sample <- createSample(releve.s, "CODE_POINT", "Espece")
#'   indices <- getDissimilarityEstimates(dissEst, sample, populationSize)
#'   indices$stratum <- stratum
#'   output <- rbind(output, indices)
#'   baselga.s <- getBaselgaDissimilarityIndices(dissEst, sample)
#'   baselga.s$stratum <- stratum
#'   baselga <- rbind(baselga, baselga.s)
#' }

#' @seealso createDissimilarityIndicesEstimator
#' @seealso createSample
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
  dissimilarityEstimates <- J4R::callJavaMethod(dissimilarityEstimator, "getDissimilarityIndicesMultiplesiteEstimator", sample, as.integer(populationSize), TRUE, leaveOneOutMode)

  simpsonEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.DiversityIndices$BetaIndex", "Simpson")
  sorensenEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.DiversityIndices$BetaIndex", "Sorensen")
  nestednessEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.DiversityIndices$BetaIndex", "Nestedness")

  alphaEstimate <- J4R::callJavaMethod(dissimilarityEstimates, "getAlphaDiversity")
  gammaEstimate <- J4R::callJavaMethod(dissimilarityEstimates, "getGammaDiversity")
  simpsonEstimate <- J4R::callJavaMethod(dissimilarityEstimates, "getBetaDiversity", simpsonEnum)
  sorensenEstimate <- J4R::callJavaMethod(dissimilarityEstimates, "getBetaDiversity", sorensenEnum)
  nestednessEstimate <- J4R::callJavaMethod(dissimilarityEstimates, "getBetaDiversity", nestednessEnum)

  Alpha <- J4R::callJavaMethod(J4R::callJavaMethod(alphaEstimate, "getMean"), "getSumOfElements")
  Gamma <- J4R::callJavaMethod(J4R::callJavaMethod(gammaEstimate, "getMean"), "getSumOfElements")

  Simpson <- J4R::callJavaMethod(J4R::callJavaMethod(simpsonEstimate, "getMean"), "getSumOfElements")
  varSimpson <- J4R::callJavaMethod(J4R::callJavaMethod(simpsonEstimate, "getVariance"), "getSumOfElements")
  stdErrSimpson <- varSimpson^.5

  Sorensen <- J4R::callJavaMethod(J4R::callJavaMethod(sorensenEstimate, "getMean"), "getSumOfElements")
  varSorensen <- J4R::callJavaMethod(J4R::callJavaMethod(sorensenEstimate, "getVariance"), "getSumOfElements")
  stdErrSorensen <- varSorensen^.5

  Nestedness <- J4R::callJavaMethod(J4R::callJavaMethod(nestednessEstimate, "getMean"), "getSumOfElements")
  varNestedness <- J4R::callJavaMethod(J4R::callJavaMethod(nestednessEstimate, "getVariance"), "getSumOfElements")
  stdErrNestedness <- varNestedness^.5

  return(data.frame(n, Simpson, varSimpson, stdErrSimpson, Sorensen, varSorensen, stdErrSorensen, Nestedness, varNestedness, stdErrNestedness, Alpha, Gamma))
}


#'
#' Calculate Baselga's Dissimilarity Indices
#'
#' This function computes Baselga's multiple-site dissimilarity indices of Simpson, Sorensen and
#' nestedness from a set of assemblages.
#'
#' It assumes that the sample is exhaustive, that it is a census of the  population. If the sample is
#' not exhaustive (the vast majority of the cases), then the getDissimilarityEstimates function should
#' be used.
#'
#' @param dissimilarityEstimator a java.object that can compute or estimate the dissimilarity indices. It
#' should be generated using the createDissimilarityIndicesEstimator function
#' @param sample a java.object instance that stands for a Map in the Java environment. It should be
#' generated using the createSample function
#' @return a data.frame object with the estimated dissimilarity indices and their standard errors
#'
#' @seealso getDissimilarityEstimates
#'
#' @export
getBaselgaDissimilarityIndices <- function(dissimilarityEstimator, sample) {
  n <- J4R::callJavaMethod(sample, "size")
  messageToBeDisplayed <- paste("Calculating Baselga's multiple-site dissimilarity indices from a sample of ", n, " plots...", sep="")
  if (n > 500) {
    messageToBeDisplayed <- paste(messageToBeDisplayed, " This may take some time!", sep="")
  }
  message(messageToBeDisplayed)
  dissimilarityIndices <- J4R::callJavaMethod(dissimilarityEstimator, "getMultiplesiteDissimilarityIndices", sample)

  simpsonEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.DiversityIndices$BetaIndex", "Simpson")
  sorensenEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.DiversityIndices$BetaIndex", "Sorensen")
  nestednessEnum <- J4R::createJavaObject("canforservutility.biodiversity.indices.DiversityIndices$BetaIndex", "Nestedness")

  Simpson <- J4R::callJavaMethod(dissimilarityIndices, "getBetaDiversity", simpsonEnum)
  Sorensen <- J4R::callJavaMethod(dissimilarityIndices, "getBetaDiversity", sorensenEnum)
  Nestedness <- J4R::callJavaMethod(dissimilarityIndices, "getBetaDiversity", nestednessEnum)

  return(data.frame(Simpson, Sorensen, Nestedness))
}


