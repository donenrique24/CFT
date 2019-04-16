############################################
# Simple tests for dissmilarity indices
# Mathieu Fortin - April 2019
############################################

dataReleves <- CFT::subsetUrbanEnvironmentNancy

dissEst <- createDissimilarityIndicesEstimator()

strataList <- unique(dataReleves$Stratum)

output <- NULL
for (stratum in strataList) {
  releve.s <- dataReleves[which(dataReleves$Stratum == stratum),]
  if (stratum == "forest") {
    populationSize <- 3089 * 10000 / (pi * 5^2)
  } else if (stratum == "parkings") {
    populationSize <- 501 * 10000 / (pi * 5^2)
  } else {
    populationSize <- 100000
  }

  sample <- createSample(releve.s, "CODE_POINT", "Espece")
  indices <- getDissimilarityEstimates(dissEst, sample, populationSize)
  indices$stratum <- stratum
  output <- rbind(output, indices)
}

test_that("Testing forests", {
  expect_equal(abs(output[which(output$stratum == "forest"), "Simpson"] - 0.2603477) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "stdErrSimpson"] - 0.02001768) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Sorensen"] - 0.4025105) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "stdErrSorensen"] - 0.01579914) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Nestedness"] - 0.1421628) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "stdErrNestedness"] - 0.02596182) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Alpha"] - 20.42857) < 1E-5, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Gamma"] - 138.8242) < 1E-4, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Simpson"] - 0.2996380) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "stdErrSimpson"] - 0.03164019) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Sorensen"] - 0.4674877) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "stdErrSorensen"] - 0.01100303) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Nestedness"] - 0.1678498) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "stdErrNestedness"] - 0.04033406) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Alpha"] - 13.09091) < 1E-5, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Gamma"] - 181.9091) < 1E-4, TRUE)
})

J4R::shutdownJava()
