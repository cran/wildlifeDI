% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conPairs.R
\name{conPairs}
\alias{conPairs}
\title{Identify contact pairs}
\usage{
conPairs(ltraj)
}
\arguments{
\item{ltraj}{an object of the class \code{ltraj} which is output from the function \code{conProcess} or \code{conPhase}.}
}
\value{
A data frame, where each row represents one of the two fixes in each unique contact pair.
}
\description{
Create a dataframe where each row represents a single contact pair.
}
\details{
This function is used to extract contact pairs following use of the \code{conProcess} or \code{conPhase} function. The returned data frame has two new columns: contact_orig_rowid - the original row id of that particular fix, and contact_pair_id - a unique identifier to show which two fixes are represented by a pair of contacts. The number of unique pairs of contacts is then the highest number in this column, and will be equal to half the number of rows in the data frame.
}
\examples{
\dontrun{
data(does)
doecons <- conProcess(does,tc=15*60,dc=50)
doephas <- conPhase(doecons,pc=60*60)
prs <- conPairs(doephas)
head(prs)
}

}
\seealso{
conProcess, conPhase
}
\keyword{contacts}
