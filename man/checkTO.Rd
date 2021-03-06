% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkTO.R
\name{checkTO}
\alias{checkTO}
\title{Check for temporal overlap}
\usage{
checkTO(traj1, traj2)
}
\arguments{
\item{traj1}{an object of the class \code{ltraj} which contains the time-stamped
movement fixes of the first object. Note this object must be a \code{type II
ltraj} object. For more information on objects of this type see \code{help(ltraj)}.}

\item{traj2}{same as \code{traj1}.}
}
\value{
A \code{list} of with three pieces of information, whether the two trajectories overlap (\code{$TO}) a logical vector, the beginning (\code{$TOstart}), and end (\code{$TOend}) of the overlap period, stored as \code{POSIX} objects.
}
\description{
The function \code{checkTO} is a simple function for identifying if, and for how long, two telemetry datasets overlap (temporally) with each other. The function returns a list with three pieces of information: a logical variable indicating if the two trajectories overlap temporally, and timings of the beginning and end of the overlap period.
}
\details{
The function \code{checkTO} can be used to identify if, when, and for how long two telemetry datasets overlap temporally.
}
\examples{
data(deer)
deer37 <- deer[1]
deer38 <- deer[2]
spts <- checkTO(deer37, deer38)

}
\seealso{
GetSimultaneous, GetTO
}
\keyword{processing}
