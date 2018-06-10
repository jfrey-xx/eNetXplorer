\name{eNetXplorer-package}
\alias{eNetXplorer-package}
\docType{package}
\title{explores elastic net families for generalized linear models}
\description{
Provides a quantitative toolkit to explore elastic net families and to uncover correlates contributing to prediction under a cross-validation framework. Fits linear, binomial (logistic) and multinomial models.
}
\details{
\tabular{ll}{
Package: \tab eNetXplorer\cr
Type: \tab Package\cr
Version: \tab 1.0.1\cr
Date: \tab 2018-06-10\cr
License: \tab GPL-3\cr
}
This package provides a full pipeline of analysis: 
\code{\link{eNetXplorer}} takes in \code{x}, \code{y} data to generate a family of elastic net models over a range of \code{alpha} values; 
\code{\link{summary}} generates a summary of results in tabular format; \code{\link{plot}} provides a variety of plots to visualize results; \code{\link{summaryPDF}} generates a 
report in PDF format; and \code{\link{export}} creates plain text output files for downstream 
processing.
}
\author{Julian Candia and John S. Tsang\cr 
Maintainer: Julian Candia \email{julian.candia@nih.gov}}
\references{Candia J and Tsang JS (2018)
  \emph{eNetXplorer: an R package for the quantitative exploration of elastic net families 
  for generalized linear models}, bioRxiv 305870 (under review).
}
\keyword{ package }
\seealso{
\code{\link{eNetXplorer}}, \code{\link{summary}}, \code{\link{plot}}, \code{\link{summaryPDF}}, \code{\link{export}}
}
\examples{
\donttest{
data(QuickStartEx)
fit = eNetXplorer(x=QuickStartEx$predictor,y=QuickStartEx$response,
family="gaussian",n_run=20,n_perm_null=10,seed=111)
summary(fit)
plot(x=fit,plot.type="measuredVsOOB",alpha.index=4)
suppressWarnings(summaryPDF(x=fit,path=tempdir()))
export(x=fit,path=tempdir())
}
}
