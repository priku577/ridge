## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(mlbench)
library(caret)
require(ridge)
library(methods)
data("BostonHousing")

## ---- echo=FALSE, eval=TRUE----------------------------------------------
ridgereg <- setRefClass("ridgereg", 
                        fields = list(formula="formula",
                                      data="data.frame",
                                      reg_coe="matrix",
                                      fit_val="matrix",
                                      data_name="character",
                                      lambda="numeric"),
                        
                        methods = list(
                          initialize = function(formula, data, lambda=0){
                            
                            formula <<- formula
                            data <<- data
                            lambda <<- lambda
                            
                            data_name <<- deparse(substitute(data))
                            
                            #getting y variable from the formula
                            get_y <- all.vars(formula)[1]
                            
                            #defining X and y
                            y <- data[[get_y]]
                            X <- model.matrix(formula, data)
                            
                            #Normalize
                            p <- ncol(X)
                            X <- cbind(X[,1], scale(X[,-1]))
                            
                            #Ridge regression
                            # reg_coe <<- solve(t(X)%*%X+lambda*diag(p)) %*% t(X)%*%y
                            # dimnames(reg_coe) <<- list(c("(Intercept)", all.vars(formula)[-1]), NULL)

                            # fit_val <<- X%*%reg_coe
                            
                            #QR Decomposition
                            y<-as.matrix(data[get_y],ncol=1)
                            X_qr<-rbind(X,sqrt(lambda)*diag(p))
                            y_qr<-rbind(y,matrix(data = 0, nrow = p, ncol = 1))
                            
                            QR <- qr((X_qr))
                            Q <- qr.Q(QR)
                            R <- qr.R(QR)
                            
                            reg_coe <<- qr.solve(R) %*% t(Q) %*% as.matrix(y_qr)
                            dimnames(reg_coe) <<- list(c("(Intercept)", all.vars(formula)[-1]), NULL)
                            
                            fit_val <<- X %*% reg_coe
                          },
                          
                          results = function(){
                            results <- list()
                            
                            results$coef <- reg_coe
                            results$fitted <- fit_val
                            return(results)
                            
                          },
                          
                          print = function(){
                            "Print out the coefficients and coefficient names"
                            
                            cat("Call: \n ")
                            cat(paste0("ridgereg(formula = ",format(formula),", data = ",data_name,", lambda = ",lambda,")\n\n"))
                            cat("Coefficients: \n")
                            cat(" ",row.names(reg_coe), "\n    ", sep = "  ")
                            cat(t(reg_coe[,1]), sep="    ")
                            
                          },
                          
                          
                          predict = function(newdata=NULL){ #if newdata is used, it should be a data frame
                            "Prints out the predicted values or if newdata is used, prints out predicted values for the new data set"
                            
                            if(is.null(newdata)){
                              result <- structure(c(results()[[2]]), names=(1:length(results()[[2]])))
                            } else{
                              X<-model.matrix(object=formula, data=newdata)
                              X[,2:ncol(X)] <- scale(X[,-1])
                              result <- (X %*% results()[[1]])[,1]
                            }
                            return(result)
                          },
                          
                          # predict = function(){
                          #   "Return the predicted values y_hat"
                          #   return(fit_val)
                          # },
                          
                          coef = function(){
                            "Return the coefficients as a named vector"
                            return(reg_coe)
                          }
                        )
)

## ------------------------------------------------------------------------
head(BostonHousing)

## ------------------------------------------------------------------------
set.seed(12345)
inTrain <- createDataPartition(BostonHousing$medv, p = .7, list = FALSE)
training <- BostonHousing[ inTrain,]
testing  <- BostonHousing[-inTrain,]

#For the 10-fold cross-validation
ctrl <- caret::trainControl(method = "repeatedcv",
  number = 10, # k=10
  repeats = 10) # repeat 10 times

## ------------------------------------------------------------------------
BostonHousing$chas <- as.numeric(BostonHousing$chas) - 1
BostonHousing <- scale(BostonHousing)

## ------------------------------------------------------------------------
set.seed(12345)
lm <- caret::train(medv ~ ., 
            data = training,
            method = "lm",
            trControl = ctrl)
lm

## ------------------------------------------------------------------------
set.seed(12345)
lflmGrid <- expand.grid(nvmax=1:(ncol(training)-1))
lflm <- caret::train(medv ~ ., 
              data = training,
              method = "leapForward",
              trControl = ctrl,
              tuneGrid = lflmGrid)

lflm

summary(lflm)

## ---- fig.height= 7, fig.width= 7----------------------------------------
p_data <- as.data.frame(cbind(predict(lflm), scale(resid(lflm))))

plot1 <- ggplot(p_data, aes(x = p_data[,1], y = p_data[,2])) + geom_point() +
          geom_smooth(method = "loess", color = "red") + xlab("Fitted") +
          ylab("Residuals") + ggtitle("Resid vs Fitted with 11 factors") +
          theme(plot.title = element_text(hjust = 0.5))

plot(plot1)

## ---- warning=FALSE------------------------------------------------------
#To use rigdereg() function
rglm <- list(type=c("Classification", "Regression"),
            library="ridge",
            loop=NULL,
            prob=NULL)

#Parameter setting
rglm$label<-"Ridge regression by mlr package"

rglm$parameters <- data.frame(parameter = "lambda",
                  class = "numeric",
                  label = "lambda")

rglm$fit <- function(x, y, lambda, param, lev, last, classProbs, ...) { 
  dat <- as.data.frame(x)
  
  respvector <- NULL
  respname <- NULL
  respnum <- NULL

  for(i in 1:ncol(x)){
    if(identical(y,dat[,i])){
    respvector <- dat[,i]
    respname <- names(x)[i]
    respnum <- i
    }
  }

  formula <- paste(respname,"~", sep="")

  if(ncol(x) > 1){
    for(i in 1:ncol(x)-1){
            formula <- paste(formula, "+", names(dat)[i], sep="")
    }
  }

  formula <- as.formula(formula)
  
  ridgereg$new(formula = formula, data=dat,lambda= param$lambda)
 }
 
rglm$predict <- function(modelFit, newdata, preProc=NULL, submodels = NULL){
  if (!is.data.frame(newdata)) 
    newdata <- as.data.frame(newdata)
  modelFit$predict(newdata)
}

rglm$sort  <- function(x) x[order(-x$lambda),]


rglm$grid <- function(x, y, len=NULL, search="grid"){
  data.frame(lambda=seq(from=0, to=5, by=1))
}

set.seed(12345)
rglm_fit <- caret::train(x = training,
                  y = training$medv,
                  method = rglm,
                  trControl = ctrl)

rglm_fit

## ------------------------------------------------------------------------
#Linear regression
lm$results[,1:4]

#Regression with forward selection
lflm$results[11,1:4]

#Ridge regression
rglm_fit$results[4,1:4]

