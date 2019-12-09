########################################################
# Client for BioSim
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: October 2019
########################################################


#'
#' A list of two plots located in southern Quebec
#'
#' @docType data
#'
#' @usage data(twoLocationsInSouthernQuebec)
#'
#' @keywords datasets
#'
#' @examples
#' data(twoLocationsInSouthernQuebec)
"twoLocationsInSouthernQuebec"

#'
#' The list of all months
#'
#' @export
allMonths <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

.createBioSimPlots <- function(latDeg, longDeg, elevM) {
  if (length(latDeg) == length(longDeg)) {
    if (length(latDeg) == length(elevM)) {
      jList <- J4R::createJavaObject("java.util.ArrayList")
      jPlots <- J4R::createJavaObject("canforservutility.biosim.BioSimPlot", latDeg, longDeg, elevM)
      J4R::callJavaMethod(jList, "add", jPlots)
      return(jList)
    }
  }
  stop("createBioSimPlots: The arguments of the function are not of the same length!")
}


.createVariableList <- function(variables) {
  myVariables <- J4R::createJavaObject("canforservutility.biosim.BioSimEnums$Variable", variables)
  jList <- J4R::createJavaObject("java.util.ArrayList")
  J4R::callJavaMethod(jList, "add", myVariables)
  return(jList)
}

#'
#' Return the normals for a period
#'
#' If the argument averageOverTheseMonths is left NULL or empty, the monthly normals are provided. If
#' this argument is filled with some months, then the normal are aggregated over these months.
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000" or "1981_2010")
#' @param variables the variables of interest typically a vector such as c("TN", "TX", "P") for minimum temperature, maximum temperature and precipitation
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA, in which case BioSim relies on a digital elevation model)
#' @param averageOverTheseMonths a vector with some months if there is a need for aggregating the climate varibles
#'
#' @return a data.frame object
#'
#' @export
getNormals <- function(period, variables, id, latDeg, longDeg, elevM, averageOverTheseMonths) {
  if (length(id) != length(latDeg)) {
    stop("The arguments id, latDeg, longDeg and elevM must have the same length!")
  }
  if (!is.null(averageOverTheseMonths) && length(averageOverTheseMonths) > 0) {
    for (month in averageOverTheseMonths) {
      if (!(month %in% allMonths)) {
        stop(paste("The month", month, "is not recognized!"))
      }
    }
  }
  J4R::checkIfExtensionsContain(myJavaLibrary = "mrnf-foresttools.jar",
                                packageName = "CFT",
                                automaticRestart = TRUE)
  jPlots <- .createBioSimPlots(latDeg, longDeg, elevM)

  jAverageOverTheseMonths <- J4R::createJavaObject("java.util.ArrayList")
  isSummarized <- F
  if (!is.null(averageOverTheseMonths) && length(averageOverTheseMonths) > 0) {
    J4R::callJavaMethod(jAverageOverTheseMonths, "add", J4R::createJavaObject("canforservutility.biosim.BioSimEnums$Month", averageOverTheseMonths))
    isSummarized <- T
  }
  jPeriod <- J4R::createJavaObject("canforservutility.biosim.BioSimEnums$Period", paste("FromNormals", period, sep=""))
  jVariables <- .createVariableList(variables)
  maps <- J4R::callJavaMethod("canforservutility.biosim.BioSimClient", "getNormals", jPeriod, jVariables, jPlots, jAverageOverTheseMonths)
  listOfPlots <- J4R::getAllValuesFromListObject(jPlots)
  listOfVariables <- J4R::getAllValuesFromListObject(jVariables)

  latDeg <- J4R::callJavaMethod(listOfPlots, "getLatitudeDeg")
  longDeg <- J4R::callJavaMethod(listOfPlots, "getLongitudeDeg")
  elevM <- J4R::callJavaMethod(listOfPlots, "getElevationM")

  if (isSummarized) {
    myDataFrame <- data.frame(id, latDeg, longDeg, elevM)
    i <- 0
    for (plot in listOfPlots) {
      i <- i + 1
      for (variable in listOfVariables) {
        variableName <- J4R::callJavaMethod(variable, "name")
        myDataFrame[i, variableName] <- J4R::callJavaMethod(J4R::callJavaMethod(maps,"get", plot), "get", variable)
      }
    }
  } else {
    myDataFrame <- NULL
    refDataFrame <- data.frame(id, latDeg, longDeg, elevM)
    listOfMonths <- J4R::getAllValuesFromArray(J4R::callJavaMethod("canforservutility.biosim.BioSimEnums$Month", "values"))
    for (i in 1:length(listOfPlots)) {
      plot <- listOfPlots[[i]]
      for (month  in listOfMonths) {
        subDataFrame <- refDataFrame[i,]
        monthName <- J4R::callJavaMethod(month, "name")
        subDataFrame[1, "month"] <- monthName
        innerMap <- J4R::callJavaMethod(J4R::callJavaMethod(maps,"get", plot), "get", month)
        for (variable in listOfVariables) {
          variableName <- J4R::callJavaMethod(variable, "name")
          subDataFrame[1, variableName] <- J4R::callJavaMethod(innerMap, "get", variable)
        }
        myDataFrame <- rbind(myDataFrame, subDataFrame)
      }
    }
  }
  return(myDataFrame)
}

#'
#' Return the annual normals for a period
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000" or "1981_2010")
#' @param variables the variables of interest typically a vector such as c("TN", "TX", "P") for minimum temperature, maximum temperature and precipitation
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA, in which case BioSim relies on a digital elevation model)
#'
#' @return a data.frame object
#'
#' @export
getAnnualNormals <- function(period, variables, id, latDeg, longDeg, elevM) {
  return(getNormals(period, variables, id, latDeg, longDeg, elevM, CFT::allMonths))
}

#'
#' Return the monthly normals for a period
#'
#' @param period a string representing the period (either "1951_1980", "1961_1990", "1971_2000" or "1981_2010")
#' @param variables the variables of interest typically a vector such as c("TN", "TX", "P") for minimum temperature, maximum temperature and precipitation
#' @param id a vector with the ids of the plots
#' @param latDeg the latitudes of the plots
#' @param longDeg the longitudes of the plots
#' @param elevM the elevations of the plots (can contain some NA, in which case BioSim relies on a digital elevation model)
#'
#' @return a data.frame object
#'
#' @export
getMonthlyNormals <- function(period, variables, id, latDeg, longDeg, elevM) {
  return(getNormals(period, variables, id, latDeg, longDeg, elevM, NULL))
}
