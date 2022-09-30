# This is package documentation for wildlifeDI.
# roxygen will use this file to create a NAMESPACE file.
# Of importance is the @import command, as it lists package dependencies.

#' @title wildlifeDI: Calculate Indices of Dynamic Interaction for Wildlife Tracking Data
#'
#' @description Dynamic interaction refers to spatial-temporal associations in the movements of two (or more) animals. This package provides tools for calculating a suite of indices used for quantifying dynamic interaction with wildlife telemetry data. For more information on each of the methods employed see the references within. The package (as of version 0.3) also has new tools for automating contact analysis in large tracking datasets. The package draws heavily on the classes and methods developed in the 'adehabitat' packages.
#'
#' @details The package \code{wildlifeDI} allows users to compute a number of currently available indices of dynamic 
#' interaction useful for wildlife telemetry studies. The currently available methods include:
#' \itemize{
#'    \item{Prox - Proximity analysis (Bertrand et al. 1996)}
#'    \item{Ca - Coefficient of Association (Bauman 1998)}
#'    \item{Don - Doncaster's measure of dynamic interaction (Doncaster 1990)}
#'    \item{Lixn - Minta's measures of spatial-temporal interaction (Minta 1992)}
#'    \item{Cs - Coefficient of Sociality (Kenward et al. 1993)}
#'    \item{HAI - Half-weight Association Index (Atwood and Weeks Jr. 2003)}
#'    \item{Cr - Correlation coefficient (Shirabe 2006)}
#'    \item{DI - Dynamic interaction index (Long and Nelson 2013)}
#'    \item{IAB - Interaction statistic (Benhamou et al. 2014)} 
#' }
#' The package \code{wildlifeDI} also provides useful functionality for identifying which fixes are temporally simultaneous, required for many of the above methods, using the function \code{GetSimultaneous}, along with other functions for exploring spatial-temporal interactions patterns in wildlife telemetry data.
#' 
#' When citing this package please use see citation ('wildlifeDI), also please cite the appropriate papers associated with individual methods being used.
#' 
#' As of version 0.4.1 the package also includes a number of new functions for performing contact analysis with larger tracking datasets. These functions use a prefix 'con' to distinguish them from other functions in the package.
#' 
#' The functions in \code{wildlifeDI} utilize the \code{ltraj} objects from the package \code{adehabitat}. For more information on objects of this type see \code{help(ltraj)}.
#'
#' @author Jed Long
#' 
#' @import adehabitatLT sf sp stats graphics
#
#' @docType package
#' @name wildlifeDI-package
NULL
