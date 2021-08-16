#######################
# Volume model (Fortin et al. 2007)
#######################



speciesCodes <- c("BOG", "BOJ", "BOP", "CET", "CHR", "EPB", "EPN", "EPR",
                  "ERR", "ERS", "FRA", "FRN", "HEG", "MEL", "ORA", "OSV",
                  "PEB", "PEG", "PET", "PIB", "PIG", "PIR", "PRU", "SAB",
                  "THO", "TIL")

#'
#' Calculate the volume of a list of trees
#'
#'
#' @param speciesCode a three-slot species code (e.g. "SAB" or "EPB")
#' @param dbhCm diameter at breast height (cm)
#' @param heightM tree height (m)
#'
#' @return the underbark merchantable volume (dm3)
#'
#' @export
getUnderbarkMerchantableVolumeDm3 <- function(speciesCode, dbhCm, heightM) {
  .connectToCFT()
  areRecognized <- speciesCode %in% speciesCodes
  if (any(!areRecognized)) {
    indices <- which(!areRecognized)
    notRecognized <- unique(speciesCode[indices])
    if (length(notRecognized) == 1) {
      stop(paste("This species code:", notRecognized, "is not recognized! Please use the getUnderbarkMerchantableVolumeSpeciesList function to get the list of possible codes."))
    } else {
      stop(paste("These species codes:", paste(notRecognized, collapse = ","), "are not recognized! Please use the getUnderbarkMerchantableVolumeSpeciesList function to get the list of possible codes."))
    }
  }
  uselessStand <- J4R::createJavaObject("quebecmrnfutility.predictor.volumemodels.merchantablevolume.VolumableStandImpl")
  predictor <- J4R::createJavaObject("quebecmrnfutility.predictor.volumemodels.merchantablevolume.MerchantableVolumePredictor")
  trees <- J4R::createJavaObject("quebecmrnfutility.predictor.volumemodels.merchantablevolume.VolumableTreeImpl", speciesCode, dbhCm, heightM)
  volume <- predictor$predictTreeCommercialUnderbarkVolumeDm3(uselessStand, trees)
  return(volume)
}


#'
#' Provide the list of species codes
#'
#' Provide all of the possible species code that can be used with the
#' getUnderbarkMerchantableVolumeDm3 function.
#'
#' @export
getUnderbarkMerchantableVolumeSpeciesList <- function() {
  return(speciesCodes)
}
