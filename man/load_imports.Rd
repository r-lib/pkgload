% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imports-env.R
\name{load_imports}
\alias{load_imports}
\title{Load all of the imports for a package}
\usage{
load_imports(path = ".")
}
\description{
The imported objects are copied to the imports environment, and are not
visible from \code{R_GlobalEnv}. This will automatically load (but not attach)
the dependency packages.
}
\keyword{internal}
