########################################################
# Basic R function for the package.
# Author: Mathieu Fortin, Canadian Wood Fibre Centre
# Date: April 2019
########################################################

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFT!")
  packageStartupMessage("The CFT package implements some biometric models applied to Canadian forests!")
  packageStartupMessage("Please, make sure that Java (version 8 or greater) is part of the path and")
  packageStartupMessage("that the J4R package has been installed on your computer.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/mrnfforesttools/wiki/CFT/ .")
}

.onLoad <- function(libname, pkgname) {
  .checkIfJ4RisAvailable()
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.checkIfJ4RisAvailable <- function() {
  if (length(find.package("J4R", quiet= TRUE)) == 0) {
    install.packages("https://sourceforge.net/projects/repiceasource/files/J4R_1.0.0.tar.gz/download", repos = NULL,  type="source")
  }
}
