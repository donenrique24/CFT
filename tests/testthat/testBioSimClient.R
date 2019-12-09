###################################
# Tests for the BioSim Client
# Mathieu Fortin - Nov 2019
###################################


# id <- c("Quebec", "Sorel")
# latDeg <- c(46.87, 46.03)
# longDeg <- c(-71.25, -73.12)
# elevM <- c(114, 15)
# twoLocationsInSouthernQuebec <- data.frame(id, latDeg, longDeg, elevM)
# save(file = "./data/twoLocationsInSouthernQuebec.RData", twoLocationsInSouthernQuebec)

locations <- CFT::twoLocationsInSouthernQuebec

variables <- c("TN","TX","P")
averageOverTheseMonths <- c()

normals <- getAnnualNormals("1981_2010", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM)

test_that("Testing that 1981-2010 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.1383562) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.331781) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1319) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 1.69589) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 11.37041) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 1033.4) < 1E-4, TRUE)
})

normals <- getAnnualNormals("1971_2000", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM)

test_that("Testing that 1971-2000 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.44) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.129589) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1284.8) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 1.071507) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 11.06356) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 978.4) < 1E-4, TRUE)
})

normals <- getAnnualNormals("1961_1990", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM)

test_that("Testing that 1961-1990 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.7452055) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.13726) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1235.3) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 0.6646575) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 10.9737) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 948.4) < 1E-4, TRUE)
})

normals <- getAnnualNormals("1951_1980", variables, locations$id, locations$latDeg, locations$longDeg, locations$elevM)

test_that("Testing that 1951-1980 annual normals for Quebec and Sorel can be properly retrieved", {
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TN"] - -0.7167123) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"TX"] - 9.057808) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Quebec"),"P"] - 1200.1) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TN"] - 0.6835616) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"TX"] - 10.91753) < 1E-4, TRUE)
  expect_equal(abs(normals[which(normals$id == "Sorel"),"P"] - 952.2) < 1E-4, TRUE)
})

J4R::shutdownJava()



