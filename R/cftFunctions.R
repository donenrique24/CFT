########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2019
########################################################

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFT!")
  packageStartupMessage("The CFT package implements some biometric models applied to Canadian forests!")
  packageStartupMessage("Please, make sure that Java (version 8 or later) is installed on your computer.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/mrnfforesttools/wiki/CFT/ .")
}


.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onUnload <- function(libpath) {
  shutdownJava()
}

.onDetach <- function(libpath) {
  shutdownJava()
}


#'
#' Extends the shutdownJava function of the J4R package
#'
#' @export
shutdownJava <- function() {
  if (J4R::isConnectedToJava()) {
    J4R::shutdownJava()
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


.loadREpicea <- function() {
  if (!J4R::checkIfClasspathContains("repicea.jar")) {
    J4R::addUrlToClassPath("repicea.jar", packageName = "CFT")
  }
  if (.getSimpleJavaVersion() > 8) {
    if (!J4R::checkIfClasspathContains("istack-commons-runtime.jar")) {
      J4R::addUrlToClassPath("istack-commons-runtime.jar", packageName = "CFT")
    }
    if (!J4R::checkIfClasspathContains("jakarta.activation-api.jar")) {
      J4R::addUrlToClassPath("jakarta.activation-api.jar", packageName = "CFT")
    }
    if (!J4R::checkIfClasspathContains("jakarta.xml.bind-api.jar")) {
      J4R::addUrlToClassPath("jakarta.xml.bind-api.jar", packageName = "CFT")
    }
    if (!J4R::checkIfClasspathContains("jaxb-runtime.jar")) {
      J4R::addUrlToClassPath("jaxb-runtime.jar", packageName = "CFT")
    }
  }
}

.getSimpleJavaVersion <- function() {
  version <- suppressMessages(J4R::getJavaVersion()$version)
  dotIndices <- gregexpr("\\.", version)
  firstDot <- dotIndices[[1]][1]
  firstInt <- as.integer(substr(version, 1, firstDot-1))
  if (firstInt == 1) {
    secondDot <- dotIndices[[1]][2]
    secondInt <- as.integer(substr(version, firstDot + 1, secondDot - 1))
    return(secondInt)
  } else {
    return(firstInt)
  }
}

.connectToCFT <- function() {
  if (!J4R::isConnectedToJava()) {
    J4R::connectToJava()
  }
  .loadREpicea()
  .loadMrnfForesttools()
}


.loadMrnfForesttools <- function() {
  if (!J4R::checkIfClasspathContains("mrnf-foresttools.jar")) {
    J4R::addUrlToClassPath("mrnf-foresttools.jar", packageName = "CFT")
  }
}

