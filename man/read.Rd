% Part of the dcemri package for R
% Distributed under the BSD license: see dcemri/COPYING
% 
% $Id: read.Rd 191 2009-08-25 15:12:31Z bjw34032 $

\name{Read Headers and Images}
\alias{read.hdr}
\alias{read.img}
\alias{make.hdr}
\title{Read Headers and Binary Image Files for Medical Imaging Data}
\description{
  These functions read in the header information and/or multidimensional
  array from a binary file in ANALYZE or NIfTI formats.
}
\usage{
read.hdr(fname, verbose=FALSE, warn=-1)
read.img(fname, verbose=FALSE, warn=-1, ...)
make.hdr(X, Y, Z, T, datatype, type)
}
\arguments{
  \item{fname}{is the pathname of the Analyze pair of files (.img and
    .hdr) or the single NIfTI file (.nii).}
  \item{verbose}{is a logical variable (default = \code{FALSE}) that
    allows text-based feedback during execution of the function.}
  \item{X,Y,Z,T}{are the dimenisions (three spatial and one temporal)
    for the header file.}
  \item{datatype}{is a character string describing the image format.
    Valid character strings include: \dQuote{uint1}, \dQuote{uint8},
      \dQuote{int16}, \dQuote{int32}, \dQuote{float32},
      \dQuote{float64}.}
  \item{type}{is a character string declaring either the Analyze or
    NIfTI format (default = \code{analyze}).}
  \item{warn}{is a number to regulate the display of warnings (default =
    -1).  See \code{options} for more details.}
  \item{...}{is for additional parameters that may be passed to
    \code{read.nifti.img}.}
}
\details{
  The function \code{read.hdr} decides whether the input file should be
  read using \code{read.analyze.hdr} or \code{read.nifti.hdr} and if
  compression is required.  A similar description is valid for
  the \code{read.img} function.  They are essentially \dQuote{wrapper}
  functions for calls to the functions that are specific to the Analyze
  or NIfTI formats.

  The \code{make.hdr} function provides an empty header, in Analyze or
  NIfTI format, to use for data exportation.  Note, it is probably
  better to read in a pre-existing header file and modify it as
  required.
}
\value{
  The \code{read.hdr} function returns a list containing all the fields
  from the header file.  The \code{read.img} function returns a
  multidimensional array taken from the binary imaging data.
}
\references{
  ANALYZE 7.5 \url{http://www.mayo.edu/bir/PDF/ANALYZE75.pdf}

  NIfTI-1 \url{http://nifti.nimh.nih.gov/}
}
\seealso{
  \code{\link{read.analyze.hdr}}, \code{\link{read.nifti.hdr}},
  \code{\link{read.analyze.img}}, \code{\link{read.nifti.img}},
}
% \examples{}
\author{Brandon Whitcher, Volker Schmid}
\keyword{file}
