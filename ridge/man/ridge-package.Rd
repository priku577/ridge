\name{ridge-package}
\alias{ridge-package}
\alias{ridge}
\docType{package}
\title{
\packageTitle{ridge}
}
\description{
\packageDescription{ridge}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ridge}
\packageIndices{ridge}

}
\author{
\packageAuthor{ridge}

Maintainer: \packageMaintainer{ridge}
}
\references{

}

\keyword{ ridge }

\examples{
 data(iris)
 ridgereg_mod <- ridgereg$new(Petal.Length~Species,data=iris,lambda =.25)
ridgereg_mod$print_QR()
ridgereg_mod$coef_QR()
ridgereg_mod$predict_QR()

}
