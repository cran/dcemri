% Part of the dcemri package for R
% Distributed under the BSD license: see dcemri/COPYING
%
% $Id: read_nifti.Rd 115 2009-08-12 16:27:14Z bjw34032 $

\name{Read NIfTI}
\alias{read.nifti.hdr}
\alias{read.nifti.img}
\title{Read NIfTI Format Headers and Image Files}
\description{
  These functions read in the header information and multidimensional
  array from a binary file in NIfTI-1 format.
}
\usage{
read.nifti.hdr(fname, onefile=TRUE, gzipped=TRUE, verbose=FALSE,
               warn=-1)
read.nifti.img(fname, onefile=TRUE, gzipped=TRUE, verbose=FALSE,
               warn=-1)
}
\arguments{
  \item{fname}{Pathname of the NIfTI file(s).}
  \item{onefile}{Allows the scanning of single-file format (.nii) or
    dual-file format (.hdr and .img) NIfTI files.}
  \item{gzipped}{Allows the importation of compressed (.gz) files.}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{options} for more details.}
}
\value{
  \code{read.nifti.hdr} returns a list containing all the fields from
  the header file.  \code{read.nifti.img} returns a multidimensional
  array depending on the fields obtained from the header file.
}
\details{
  Both functions utilize the internal \code{readBin} and \code{readChar}
  command to efficiently extract information from the binary file(s).
  The types of data are limited to 1- and 2-byte integers, 4-byte floats
  and 8-byte doubles.
}
\references{
  NIfTI-1 \url{http://nifti.nimh.nih.gov/}
}
\seealso{
  \code{\link{read.analyze.hdr}}, \code{\link{read.analyze.img}},
  \code{\link{write.nifti.img}}
}
% \examples{}
\author{Brandon Whitcher}
\keyword{file}
