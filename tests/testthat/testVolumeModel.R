##########################
# Test for volume model
##########################



volumeSAB <- getUnderbarkMerchantableVolumeDm3("SAB", 16, 13)

test_that("Testing volume SAB", {
  expect_equal(volumeSAB, 105.5542, tolerance = 1E-4)
})

volumeSAB <- getUnderbarkMerchantableVolumeDm3("SAB", c(16, 25), c(13,18))

test_that("Testing volume SAB", {
  expect_equal(volumeSAB[1], 105.5542, tolerance = 1E-4)
  expect_equal(volumeSAB[2], 361.4733, tolerance = 1E-4)
})

test_that("Testing available species codes", {
          expect_equal(length(getUnderbarkMerchantableVolumeSpeciesList()), 26)
})


out <- tryCatch(
    {
      getUnderbarkMerchantableVolumeDm3("XXX", 16, 13)
    },
    error=function(cond) {
      return("failed")
    }
)

test_that("Testing that unrecognized code fails", {
  expect_equal(out, "failed")
})


out <- tryCatch(
  {
    getUnderbarkMerchantableVolumeDm3(c("SAB", "XXX", "XX2", "XX2", "EPR"), 16, 13)
  },
  error=function(cond) {
    return("failed")
  }
)

test_that("Testing that unrecognized code fails", {
  expect_equal(out, "failed")
})


#load("./tests/testthat/DummyData.RData")
load("DummyData.RData")

volPredNew <- getUnderbarkMerchantableVolumeDm3(Dummy$ESSENCE, Dummy$DHP, Dummy$Hauteur)

#save(volPred, file = "./tests/testthat/refVolPred.RData")

load("refVolPred.RData")

test_that("Testing that Gabriel's bug (REpicea Bug #63) is fixed", {
  expect_equal(any(abs(volPred - volPredNew) > 0), F)
})

shutdownJava()
