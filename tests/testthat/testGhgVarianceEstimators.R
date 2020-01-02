############################################
# Simple tests for variance estimators
# Mathieu Fortin - December 2019
############################################

###### Case study 1 ######

V <- createEstimate(207*10^6, 81*10^12)
D <- createEstimate(0.4, 0.0004)
BEF <- createEstimate(1.559, 0.00389)
CF <- createEstimate(.5105, 0.0001)

C <- multiplyEstimates(V, D, BEF, CF, factor=44/12)
C.naive <- multiplyEstimates(V, D, BEF, CF, factor=44/12, method="Naive")
C.ipcc <- multiplyEstimates(V, D, BEF, CF, factor=44/12, method="Propagation")
C.mc <- multiplyEstimates(V, D, BEF, CF, factor=44/12, method="MonteCarlo", realMC=100000)
C.mcRes <- multiplyEstimates(V, D, BEF, CF, factor=44/12, method="RescaledMonteCarlo", realMC=100000)

test_that("Testing Goodman's variance estimator - Case study 1", {
  expect_equal(C@mean, 241625980.2)
  expect_equal(C@variance, 371348178205139)
})

test_that("Testing naive variance estimator - Case study 1", {
  expect_equal(C.naive@mean, 241625980.2)
  expect_equal(C.naive@variance, 372988916711894)
})

test_that("Testing ipcc propagation-based variance estimator - Case study 1", {
  expect_equal(C.ipcc@mean, 241625980.2)
  expect_equal(C.ipcc@variance, 372167842569301)
})

test_that("Testing ipcc Monte Carlo variance estimator - Case study 1", {
  expect_equal(abs(1 - C.mc@mean/241625980.2) < 1E-3, TRUE)
  expect_equal(abs(1 - C.mc@variance/372988916711894) < 1E-2, TRUE)
})

test_that("Testing rescaled Monte Carlo variance estimator - Case study 1", {
  expect_equal(abs(1 - C.mcRes@mean/241625980.2) < 1E-3, TRUE)
  expect_equal(abs(1 - C.mcRes@variance/371348178205139) < 1E-2, TRUE)
})

###### Case study 2 ######

P <- createEstimate(.228, .1216^2, dist="lognormal")
BD <- createEstimate(.145, .018^2)
G <- createEstimate(2546, 75.32^2)

E <- multiplyEstimates(P,BD,G,factor = 10)
E.naive <- multiplyEstimates(P,BD,G,factor = 10, method="Naive")
E.ipcc <- multiplyEstimates(P, BD, G, factor = 10, method="Propagation")
E.mc <- multiplyEstimates(P, BD, G, factor = 10, method="MonteCarlo", realMC=100000)
E.mcRes <- multiplyEstimates(P, BD, G, factor = 10, method="RescaledMonteCarlo", realMC=100000)

test_that("Testing Goodman's variance estimator - Case study 2", {
  expect_equal(E@mean, 841.7076)
  expect_equal(E@variance, 209769.904267563)
})

test_that("Testing naive variance estimator - Case study 2", {
  expect_equal(E.naive@mean, 841.7076)
  expect_equal(E.naive@variance, 216352.716885812)
})

test_that("Testing ipcc propagation-based variance estimator - Case study 2", {
  expect_equal(E.ipcc@mean, 841.7076)
  expect_equal(E.ipcc@variance, 213058.592681011)
})

test_that("Testing ipcc Monte Carlo variance estimator - Case study 2", {
  expect_equal(abs(1 - E.mc@mean/841.7076) < 4E-3, TRUE)
  expect_equal(abs(1 - E.mc@variance/216352.716885812) < 5E-2, TRUE)
})

test_that("Testing rescaled Monte Carlo variance estimator - Case study 2", {
  expect_equal(abs(1 - E.mcRes@mean/841.7076) < 4E-3, TRUE)
  expect_equal(abs(1 - E.mcRes@variance/209769.904267563) < 5E-2, TRUE)
})

