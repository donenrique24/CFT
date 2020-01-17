############################################
# Simple tests for dissmilarity indices
# Mathieu Fortin - April 2019
############################################

dataReleves <- CFT::subsetUrbanEnvironmentNancy

dissEst <- createDissimilarityIndicesEstimator()

strataList <- unique(dataReleves$Stratum)

output <- NULL
baselga <- NULL
stratum <- strataList[1]

for (stratum in strataList) {
  releve.s <- dataReleves[which(dataReleves$Stratum == stratum),]
  if (stratum == "forest") {
    populationSize <- 3089 * 10000 / (pi * 5^2)
  } else if (stratum == "parking") {
    populationSize <- 501 * 10000 / (pi * 5^2)
  } else {
    populationSize <- 100000
  }

  sample <- createSample(releve.s, "CODE_POINT", "Espece")
  indices <- getDissimilarityEstimates(dissEst, sample, populationSize)
  indices$stratum <- stratum
  output <- rbind(output, indices)
  baselga.s <- getBaselgaDissimilarityIndices(dissEst, sample)
  baselga.s$stratum <- stratum
  baselga <- rbind(baselga, baselga.s)
}

test_that("Testing forests", {
  expect_equal(abs(output[which(output$stratum == "forest"), "Simpson"] - 0.2603477) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "stdErrSimpson"] - 0.02001768) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Sorensen"] - 0.4025105) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "stdErrSorensen"] - 0.01579914) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Nestedness"] - 0.1421628) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "stdErrNestedness"] - 0.02596182) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Alpha"] - 20.42857) < 1E-5, TRUE)
  expect_equal(abs(output[which(output$stratum == "forest"), "Gamma"] - 138.8225) < 1E-4, TRUE)

  expect_equal(abs(output[which(output$stratum == "parking"), "Simpson"] - 0.2996533) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "stdErrSimpson"] - 0.03163888) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Sorensen"] - 0.4675060) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "stdErrSorensen"] - 0.01100832) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Nestedness"] - 0.1678526) < 1E-6, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "stdErrNestedness"] - 0.04033552) < 1E-8, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Alpha"] - 13.09091) < 1E-5, TRUE)
  expect_equal(abs(output[which(output$stratum == "parking"), "Gamma"] - 181.8853) < 1E-4, TRUE)

  expect_equal(abs(baselga[which(baselga$stratum == "forest"), "Simpson"] - 0.6894977) < 1E-6, TRUE)
  expect_equal(abs(baselga[which(baselga$stratum == "forest"), "Sorensen"] - 0.8095238) < 1E-6, TRUE)
  expect_equal(abs(baselga[which(baselga$stratum == "forest"), "Nestedness"] - 0.12002609) < 1E-6, TRUE)

  expect_equal(abs(baselga[which(baselga$stratum == "parking"), "Simpson"] - 0.8531856) < 1E-6, TRUE)
  expect_equal(abs(baselga[which(baselga$stratum == "parking"), "Sorensen"] - 0.9226277) < 1E-6, TRUE)
  expect_equal(abs(baselga[which(baselga$stratum == "parking"), "Nestedness"] - 0.06944214) < 1E-6, TRUE)
})

shutdownJava()
