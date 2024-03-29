% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shims.R
\name{system.file}
\alias{system.file}
\alias{shim_system.file}
\title{Replacement version of system.file}
\usage{
shim_system.file(..., package = "base", lib.loc = NULL, mustWork = FALSE)
}
\arguments{
\item{...}{character vectors, specifying subdirectory and file(s)
    within some package.  The default, none, returns the
    root of the package.  Wildcards are not supported.}

\item{package}{a character string with the name of a single package.
    An error occurs if more than one package name is given.}

\item{lib.loc}{a character vector with path names of \R libraries.
    See \sQuote{Details} for the meaning of the default value of \code{NULL}.}

\item{mustWork}{logical.  If \code{TRUE}, an error is given if there
    are no matching files.}
}
\description{
This function is meant to intercept calls to \code{\link[base:system.file]{base::system.file()}},
so that it behaves well with packages loaded by devtools. It is made
available when a package is loaded with \code{\link[=load_all]{load_all()}}.
}
\details{
When \code{system.file} is called from the R console (the global
environment), this function detects if the target package was loaded with
\code{\link[=load_all]{load_all()}}, and if so, it uses a customized method of searching
for the file. This is necessary because the directory structure of a source
package is different from the directory structure of an installed package.

When a package is loaded with \code{load_all}, this function is also inserted
into the package's imports environment, so that calls to \code{system.file}
from within the package namespace will use this modified version. If this
function were not inserted into the imports environment, then the package
would end up calling \code{base::system.file} instead.
}
