## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(ridge)
library(mlbench)
library(caret) 
library(leaps)
data("BostonHousing")

## ------------------------------------------------------------------------
data("BostonHousing")
names(BostonHousing)
train_index <- caret::createDataPartition(BostonHousing$age, p = .75,
                                         list = FALSE,
                                         times= 1)
train_data <- BostonHousing[train_index, ]
test_data <- BostonHousing[-train_index, ]


head(train_data)
head(test_data)

## ------------------------------------------------------------------------
ridge <- caret::train(crim~.,
                      data = train_data,
                      method='lm',
                      trControl = trainControl(method = "cv")
)

print(ridge)

## ------------------------------------------------------------------------
lflmGrid <- expand.grid(nvmax=1:(ncol(train_data)-1))

ridge <- caret::train(crim~.,
                      data = train_data,
                      method='leapForward',
                      tuneGrid = lflmGrid
)
print(ridge)

## ------------------------------------------------------------------------
ridge <- list(type="Regression", 
              library="ridge",
              loop=NULL,
              prob=NULL)

ridge$parameters <- data.frame(parameter="lambda",
                               class="numeric",
                               label="lambda")


ridge$grid <- function (x, y, len = NULL, search = "grid"){
  data.frame(lambda = lambda)
} 

ridge$fit <- function (x, y, wts, param, lev, last, classProbs, ...) {
  dat <- if (is.data.frame(x)) 
    x
  else as.data.frame(x)
  dat$.outcome <- y
  out <- ridgereg$new(.outcome ~ ., data=dat ,lambda = param$lambda, normalize=normalize, ...)
  
  out
}

ridge$predict <- function (modelFit, newdata, submodels = NULL) {
  if (!is.data.frame(newdata)) 
    newdata <- as.data.frame(newdata)
  newdata <- scale(newdata)
  modelFit$predict(newdata)
}

## ------------------------------------------------------------------------
library(MASS)
fitControl <- caret::trainControl(method = "cv",
                                  number = 10)
lambdaGrid <- expand.grid(lambda = c(0,.01,.02,.03,.04))
ridge <- caret::train(crim~.,
                      data = train_data,
                      method='ridge',
                      trControl = fitControl,
                      tuneGrid = lambdaGrid,
                      preProcess=c('center', 'scale')
)
predict(ridge$finalModel, type='coef', mode='norm')$coefficients[13,]
ridge.pred <- predict(ridge, test_data)
avgErrror<-2*sqrt(mean(ridge.pred - test_data$crim)^2)
print(ridge)

