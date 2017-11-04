context("ridgereg")

data("iris")

Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("lenreg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~SSSpecies, data=iris,lambda=0))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal~Species, data=irfsfdis))
})


test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Species, data=iris, lambda=0)
  
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})

test_that("print_QR() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Species, data=iris,lambda=0)
  
  expect_output(ridgereg_mod$print_QR(),"ridgereg\\(formula = Petal\\.Length ~ Species, data = iris\\, lambda = 0)")
  expect_output(ridgereg_mod$print_QR()," Speciesversicolor Speciesvirginica")
})

test_that("predict_QR() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Species, data=iris,lambda=0)
  expect_equal(round(unname(ridgereg_mod$predict_QR()[c(1,64,106)]),2), c(-2.3,0.50, 1.79))
})

test_that("coef_QR() method works", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Species, data=iris,lambda=0)
  
  expect_true(all(round(unname(ridgereg_mod$coef_QR()),2) %in% c(1.32, 1.93)))
})
