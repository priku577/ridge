#' ridgereg using QR
#'
#' @field formula formula which the model will process.. 
#' @field data data.frame input data. 
#' @field parsedata character parse into input data. 
#' @field beta_ridge_QR matrix result for regression coefficient. 
#' @field ybar_ridge_QR matrix result for fitted value. 
#' @field names vector for containing the names of coefficients. 
#'
#' @return  print_QR() prints out the coefficients and coefficient names,
#'         predict_QR() returns the predicted values of ybar,
#'         coef_QR() returns the coefficients as a named vector,
#' @export
#'
#' @examples  ridgereg_mod <- ridgereg$new(Petal.Length~Species,data=iris, lambda=1.2)
#'  ridgereg_mod$print_QR()
#'  ridgereg_mod$predict_QR()
#'  ridgereg_mod$coef_QR()
#'  
ridgereg <- setRefClass( Class = "ridgereg",
                         fields = list(formula = "formula",data = "data.frame",
                                       parsedata = "character",
                                       beta_ridge_QR ="matrix",ybar_ridge_QR="matrix", 
                                       names="vector"),
                         methods=list(
                           initialize= function(formula,data,lambda,normalise = TRUE){
                             formula  <<- formula
                             data <<- data
                             parsedata <<- deparse(substitute(data))
                             x <- model.matrix(formula, data)
                             x<- ((x[,-1]-mean(x[,-1])) / sd(x[,-1]))
                             x<-x-mean(x)/sd(x)
                             lambda<-lambda
                             yy  <- all.vars(expr = formula)[1]
                             y <- as.matrix(data[, yy])
                             x_norm<-x
                             #using QR decompositon
                             identity_matrix<- diag(ncol(x_norm))
                             QR_ridge<- qr(x_norm)
                             Q<-qr.Q (QR_ridge)
                             R<-qr.R(QR_ridge)
                             RR<- ((R %*% t(Q) %*% Q) + lambda *(t(Q) %*% Q %*% solve(t(R))))
                             
                             beta_ridge_QR<<- backsolve(RR, crossprod(Q,y))
                             names<<-c(colnames(R))
                             ybar_ridge_QR<<- (x_norm %*% beta_ridge_QR)
                             
                           },     
                           
                           print_QR= function(){
                             
                             
                             cat("\n","Call:","\n",
                                 paste("ridgereg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],
                                       ", ", "data = ", parsedata, ", lambda = 0)",sep = "", collapse = "\n" ),
                                 "\n","Coefficients:","\n",
                                 format(names, justify = "centre",width = 12),"\n",
                                 format(round(beta_ridge_QR,2), justify = "centre",width = 12))
                             
                           },
                           
                           predict_QR =function(){
                             cat("\n \n Predicted values or fitted values using QR decomposition:","\n\n")
                             return(as.vector(round(ybar_ridge_QR, 2)))
                           },
                           
                           coef_QR = function(){
                             cat("\n \nRegressions coefficients using QR decomposition:","\n\n")
                             ridge_coef_QR <-as.vector(round(beta_ridge_QR,2))
                             names(ridge_coef_QR)<-names
                             return (ridge_coef_QR)
                           }
                           
                         ))

#ridgereg_mod <- ridgereg$new(Petal.Length~Species,data=iris, lambda=1.2)
#ridgereg_mod$print_QR()