% Part of the dcemri package for R
% Distributed under the BSD license: see dcemri/COPYING
%
% $Id: read_analyze.Rd 191 2009-08-25 15:12:31Z bjw34032 $

\name{Read Analyze}
\alias{read.analyze.hdr}
\alias{read.analyze.img}
\title{Read Analyze Format Headers and Binary Image Files}
\description{
  These functions read in the header information and multidimensional
  array from a binary file in Analyze 7.5 format.
}
\usage{
read.analyze.hdr(fname, gzipped=TRUE, verbose=FALSE, warn=-1)
read.analyze.img(fname, gzipped=TRUE, signed=FALSE, verbose=FALSE,
                 warn=-1)
}
\arguments{
  \item{fname}{Pathname of the Analyze pair of files .img and .hdr
    without the suffix.}
  \item{gzipped}{Allows the import/export of compressed (.gz) files
    (default = \code{TRUE}).}
  \item{signed}{Only changed to \code{FALSE} when reading in unsigned
    single-byte integers.}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{options} for more details.}
}
\details{
  Both functions utilize the internal \code{readBin} and \code{rawToChar}
  command in order to efficiently extract information from a binary
  file.  The types of data are limited to 1- and 2-byte integers, 4-byte
  floats and 8-byte doubles.
}
\value{
  The \code{read.analyze.hdr} function returns a list containing all the
  fields from the header file.  The function \code{read.analyze.img}
  returns a multidimensional array taken from the binary imaging data.
}
\references{
  ANALYZE 7.5 \url{http://www.mayo.edu/bir/PDF/ANALYZE75.pdf}
}
\seealso{
  \code{\link{read.hdr}}, \code{\link{read.img}},
  \code{\link{read.nifti.hdr}}, \code{\link{read.nifti.img}}
}
% \examples{}
\author{Brandon Whitcher, Volker Schmid}
\keyword{file}
