########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2019
########################################################

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFT!")
  packageStartupMessage("The CFT package implements some biometric models applied to Canadian forests!")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is part of the path and")
  packageStartupMessage("that the J4R package has been installed on your computer.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/mrnfforesttools/wiki/CFT/ .")
}

.onLoad <- function(libname, pkgname) {
  .checkIfJ4RisAvailable()
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
  .internalShutdown()
}

.onDetach <- function(libpath) {
  .internalShutdown()
}

.internalShutdown <- function() {
  J4R::shutdownJava()
}

.checkIfJ4RisAvailable <- function() {
  minimumVersion <- c(1,0,5)
  versionString <- paste(minimumVersion, collapse = ".")
  if (length(find.package("J4R", quiet= TRUE)) == 0 || utils::packageVersion("J4R") < versionString) {
    message(paste("Installing J4R version", versionString))
    urlString <- paste("https://sourceforge.net/projects/repiceasource/files/J4R_", versionString, ".tar.gz", sep="")
    utils::install.packages(urlString, repos = NULL,  type="source")
  }
}

#'
#' Extends the shutdownJava function of the J4R package
#'
#' @export
shutdownJava <- function() {
  J4R::shutdownJava()
}

.connectToCFT <- function() {
  if (!J4R::isConnectedToJava()) {
    J4R::connectToJava()
  }
  if (!J4R::checkIfClasspathContains("mrnf-foresttools.jar")) {
    J4R::addUrlToClassPath("mrnf-foresttools.jar", packageName = "CFT")
  }
}

.addToArray <- function(refArray, array) {
  if (length(refArray) != length(array)) {
    stop("Incompatible array length!")
  } else {
    for (i in 1:length(array)) {
      refArray[[i]] <- c(refArray[[i]], array[[i]])
    }
  }
  return(refArray)
}

.convertJavaDataSetIntoDataFrame <- function(dataSetObject) {
  refArray <- NULL
  observations <- J4R::callJavaMethod(dataSetObject, "getObservations")
  observations <- J4R::getAllValuesFromListObject(observations)
  for (obs in observations) {
    array <- J4R::callJavaMethod(obs, "toArray")
    array <- as.list(J4R::getAllValuesFromArray(array))
    if (is.null(refArray)) {
      refArray <- array
    } else {
      refArray <- .addToArray(refArray, array)
    }
  }
  dataFrame <- NULL
  for (i in 1:length(refArray)) {
    dataFrame <- as.data.frame(cbind(dataFrame, refArray[[i]]))
  }
  colnames(dataFrame) <- J4R::getAllValuesFromListObject(J4R::callJavaMethod(dataSetObject, "getFieldNames"))
  return(dataFrame)
}
