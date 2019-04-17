\name{summaryPDF}
\alias{summaryPDF}
\title{
generates PDF report with summary of main results
}
\description{
This function generates a PDF report that contains a plot of model performance across the \code{alpha} range, 
followed by plots showing detailed results for each value of \code{alpha}.
}
\usage{
summaryPDF(x, path, filename="eNetXplorerSummary.pdf")
}
\arguments{
  \item{x}{\code{eNetXplorer} object.}
  \item{path}{Directory path for output PDF file.}
  \item{filename}{Name for output PDF file.}
}
\author{Julian Candia and John S. Tsang\cr 
Maintainer: Julian Candia \email{julian.candia@nih.gov}}
\seealso{\code{\link{eNetXplorer}}, \code{\link{plot}}
}
\examples{
\donttest{
data(QuickStartEx)
fit = eNetXplorer(x=QuickStartEx$predictor,y=QuickStartEx$response, 
family="gaussian",n_run=20,n_perm_null=10,seed=111)
summaryPDF(x=fit,path=tempdir())
}
}
