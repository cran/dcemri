##
##
## Copyright (c) 2009, Brandon Whitcher and Volker Schmid
## All rights reserved.
## 
## Redistribution and use in source and binary forms, with or without
## modification, are permitted provided that the following conditions are
## met:
## 
##     * Redistributions of source code must retain the above copyright
##       notice, this list of conditions and the following disclaimer. 
##     * Redistributions in binary form must reproduce the above
##       copyright notice, this list of conditions and the following
##       disclaimer in the documentation and/or other materials provided
##       with the distribution.
##     * The names of the authors may not be used to endorse or promote
##       products derived from this software without specific prior
##       written permission.
## 
## THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
## "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
## LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
## A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
## HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
## SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
## LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
## DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
## THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
## (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
## OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
## 
## $Id: read.R 191 2009-08-25 15:12:31Z bjw34032 $
##

read.hdr <- function(fname, verbose=FALSE, warn=-1) {

  cat.file <- function(fname, thefile, fill=TRUE)
    cat(paste("  fname =", fname, "\n  file =", thefile), fill=fill)

  ## Strip any extensions
  fname <- sub(".gz", "", fname)
  fname <- sub(".hdr", "", fname)
  fname <- sub(".nii", "", fname)

  fname.hdr <- paste(fname, "hdr", sep=".")
  fname.hdr.gz <- paste(fname, "hdr.gz", sep=".")
  if (file.exists(fname.hdr) && file.exists(fname.hdr.gz)) {
    stop("-- Both compressed and uncompressed files exist! --")
  } else {
    ## If uncompressed files exist, then upload!
    if (file.exists(fname.hdr)) {
      if (verbose) cat.file(fname, fname.hdr)
      hdr <- read.analyze.hdr(fname, gzipped=FALSE, verbose=verbose,
                              warn=warn)
      return(hdr)
    }
    ## If compressed files exist, then upload!
    if (file.exists(fname.hdr.gz)) {
      if (verbose) cat(fname, fname.hdr.gz)
      aim <- read.analyze.hdr(fname, gzipped=TRUE, verbose=verbose,
                              warn=warn)
      return(hdr)
    }
  }
  fname.nii <- paste(fname, "nii", sep=".")
  fname.nii.gz <- paste(fname, "nii.gz", sep=".")
  if (file.exists(fname.nii) && file.exists(fname.nii.gz)) {
    stop("-- Both compressed and uncompressed files exist! --")
  } else {
    ## If uncompressed files exist, then upload!
    if (file.exists(fname.nii)) {      
      if (verbose) cat.file(fname, fname.nii)
      hdr <- read.nifti.hdr(fname, gzipped=FALSE, verbose=verbose,
                            warn=warn)
      return(hdr)
    }
    ## If compressed files exist, then upload!
    if (file.exists(fname.nii.gz)) {
      if (verbose) cat.file(fname, fname.nii.gz)
      hdr <- read.nifti.hdr(fname, gzipped=TRUE, verbose=verbose,
                            warn=warn)
      return(hdr)
    }
  }
  ## If you get this far...
  stop(paste(fname, "is not recognized."))
}

read.analyze.hdr <- function(fname, gzipped=TRUE, verbose=FALSE, warn=-1) {

  ## Warnings?
  oldwarn <- options()$warn
  options(warn=warn)

  if (gzipped)
    fid <- gzfile(paste(fname, ".hdr.gz", sep=""), "rb")
  else
    fid <- file(paste(fname, ".hdr", sep=""), "rb")

  ## Test for endian properties
  endian <- .Platform$endian
  size.of.hdr <- readBin(fid, integer(), size=4)
  if (size.of.hdr != 348) {
    close(fid)
    endian <- "swap"
    if(gzipped)
      fid <- gzfile(paste(fname, ".hdr.gz", sep=""), "rb")
    else
      fid <- file(paste(fname, ".hdr", sep=""), "rb")
    size.of.hdr <- readBin(fid, integer(), size=4, endian=endian)
    if(verbose)
      cat("  ENDIAN = swap", fill=TRUE)
  }
  
  db.type <- rawToChar(readBin(fid, "raw", n=10))
  db.name <- rawToChar(readBin(fid, "raw", n=18))
  
  extents <- readBin(fid, integer(), size=4, endian=endian)
  session.error <- readBin(fid, integer(), size=2, endian=endian)
  
  regular <- rawToChar(readBin(fid, "raw", n=1))
  hkey.un0 <- rawToChar(readBin(fid, "raw", n=1))
  
  dim <- readBin(fid, integer(), 8, size=2, endian=endian)
  
  vox.units <- rawToChar(readBin(fid, "raw", n=4))
  cal.units <- rawToChar(readBin(fid, "raw", n=8))
  
  skip <- readBin(fid, integer(), size=2, endian=endian)
  datatype <- readBin(fid, integer(), size=2, endian=endian)
  bitpix <- readBin(fid, integer(), size=2, endian=endian)
  dim.un0 <- readBin(fid, integer(), size=2, endian=endian)
  pixdim <- readBin(fid, numeric(), 8, size=4, endian=endian)
  vox.offset <- readBin(fid, numeric(), size=4, endian=endian)

  skip <- readBin(fid, numeric(), n=3, size=4, endian=endian)

  cal.max <- readBin(fid, numeric(), size=4, endian=endian)
  cal.min <- readBin(fid, numeric(), size=4, endian=endian)
  compressed <- readBin(fid, numeric(), size=4, endian=endian)
  verified <- readBin(fid, numeric(), size=4, endian=endian)
  glmax <- readBin(fid, integer(), size=4, endian=endian)
  glmin <- readBin(fid, integer(), size=4, endian=endian)
 
  descrip <- rawToChar(readBin(fid, "raw", n=80))
  aux.file <- rawToChar(readBin(fid, "raw", n=24))
  ## This problem appeared in R-2.2.1 with error message:
  ## Error in readChar(fid, n = 1) : invalid UTF-8 input in readChar()
  ## So... readChar() has been replaced with readBin(..., raw(), ...)
  ## as suggested by Duncan Murdoch.
  ## 27 Apr 06
  ## Must use --disable-mbcs when compiling R to turn off UTF-8
  ## 31 May 06
  ## I am replacing all readChar() calls with
  ##   rawToChar(readBin(..., "raw", ...))
  ## 13 Feb 09 
  orient <- rawToChar(readBin(fid, "raw", n=1))
  originator <- rawToChar(readBin(fid, "raw", n=10))
  generated <- rawToChar(readBin(fid, "raw", n=10))
  scannum <- rawToChar(readBin(fid, "raw", n=10))
  patient.id <- rawToChar(readBin(fid, "raw", n=10))
  exp.date <- rawToChar(readBin(fid, "raw", n=10))
  exp.time <- rawToChar(readBin(fid, "raw", n=10))
  hist.un0 <- rawToChar(readBin(fid, "raw", n=3))
  
  views <- readBin(fid, integer(), size=4, endian=endian)
  vols.added <- readBin(fid, integer(), size=4, endian=endian)
  start.field <- readBin(fid, integer(), size=4, endian=endian)
  field.skip <- readBin(fid, integer(), size=4, endian=endian)
  omax <- readBin(fid, integer(), size=4, endian=endian)
  omin <- readBin(fid, integer(), size=4, endian=endian)
  smax <- readBin(fid, integer(), size=4, endian=endian)
  smin <- readBin(fid, integer(), size=4, endian=endian)
  
  close(fid)
  
  output <- list("size.of.hdr" = size.of.hdr, "endian" = endian,
                 "extents" = extents, "session.error" = session.error,
                 "dim" = dim, "datatype" = datatype, "bitpix" = bitpix,
                 "dim.un0" = dim.un0, "pixdim" = pixdim,
                 "vox.offset" = vox.offset, "cal.max" = cal.max,
                 "cal.min" = cal.min, "compressed" = compressed,
                 "verified" = verified, "glmax" = glmax, "glmin" = glmin,
                 "views" = views, "vols.added" = vols.added,
                 "start.field" = start.field, "field.skip" = field.skip,
                 "omax" = omax, "omin" = omin, "smax" = smax, "smin" = smin,
                 "descrip" = descrip, "db.type" = db.type,
                 "db.name" = db.name, "regular" = regular,
                 "hkey.un0" = hkey.un0, "vox.units" = vox.units,
                 "cal.units" = cal.units, "aux.file" = aux.file,
                 "orient" = orient, "originator" = originator,
                 "generated" = generated, "scannum" = scannum,
                 "patient.id" = patient.id, "exp.date" = exp.date,
                 "exp.time" = exp.time, "hist.un0" = hist.un0)

  ## Warnings?
  options(warn=oldwarn)
  return(output)
}

make.hdr <- function(X, Y, Z, T, datatype, type="analyze") {

  if (X < 1 || Y < 1 || Z < 1 || T < 0)
    stop("Array dimensions are incorrect")

  endian <- .Platform$endian

  if (type %in% "analyze") {
    if (!(datatype %in% c("uint1","uint8","int16","int32","float","double")))
      stop(paste("Unrecognized Analyze data type:", datatype))
    size.of.hdr <- as.integer(348)
    db.type <- rep("", 10)
    db.name <- rep("", 18)
    extents <- session.error <- integer(1)
    datatype <- switch(type,
                       "int1" = 1,
                       "int8" = 2,
                       "int16" = 4,
                       "int32" = 8,
                       "float" = 16,
                       "double" = 64)
    bitpix <- switch(type,
                     "int1" = 1,
                     "int8" = 8,
                     "int16" = 16,
                     "int32" = 32,
                     "float" = 32,
                     "double" = 64)
    regular <- "r"
    hkey.un0 <- ""
    dim <- as.integer(c(4,X,Y,Z,T,0,0,0))
    vox.units <- rep("", 4)
    cal.units <- rep("", 8)
    dim.un0 <- integer(1)
    pixdim <- numeric(8)
    vox.offset <- cal.max <- cal.min <- compressed <- verified <- 0.0
    glmax <- glmin <- integer(1)
    descrip <- rep("", 80)
    aux.file <- rep("", 24)
    orient <- ""
    originator <- generated <- scannum <- rep("", 10)
    patient.id <- exp.date <- exp.time <- rep("", 10)
    hist.un0 <- rep("", 3)
    views <- vols.added <- start.field <- field.skip <- integer(1)
    omax <- omin <- smax <- smin <- integer(1)
    hdr <- list(size.of.hdr, endian, extents, session.error, dim, datatype,
                bitpix, dim.un0, pixdim, vox.offset, cal.max, cal.min,
                compressed, verified, glmax, glmin, views, vols.added,
                start.field, field.skip, omax, omin, smax, smin, descrip,
                db.type, db.name, regular, hkey.un0, vox.units,
                cal.units, aux.file, orient, originator, generated,
                scannum, patient.id, exp.date, exp.time, hist.un0)

    names(hdr) <-
      c("size.of.hdr", "endian", "extents", "session.error", "dim", "datatype",
        "bitpix", "dim.un0", "pixdim", "vox.offset", "cal.max", "cal.min",
        "compressed", "verified", "glmax", "glmin", "views", "vols.added",
        "start.field", "field.skip", "omax", "omin", "smax", "smin",
        "descrip",  "db.type", "db.name", "regular", "hkey.un0", "vox.units",
        "cal.units", "aux.file","orient","originator", "generated",
        "scannum", "patient.id", "exp.date", "exp.time", "hist.un0")
  } else {
    ## type = "nifti"
    if (!(datatype %in% c("UNKNOWN","BINARY","CHAR","SHORT","INT","FLOAT",
                          "COMPLEX","DOUBLE","RGB")))
      stop(paste("Unrecognized NIfTI data type:", datatype))
    sizeof.hdr <- as.integer(348)
    ## was header_key substruct in Analyze 7.5
    data.type <- datatype
    db.name <- rep("", 18)
    extents <- session.error <- integer(1)
    regular <- ""
    dim.info <- integer(1)
    ## was image_dimension substruct in Analyze 7.5
    dim <- as.integer(c(4,X,Y,Z,T,0,0,0))
    intent.p1 <- intent.p2 <- intent.p3 <- numeric(1)
    intent.code <- integer(1)
    datatype <- switch(type,
                       "UNKNOWN" = 0,
                       "BINARY" = 1,
                       "CHAR" = 2,
                       "SHORT" = 4,
                       "INT" = 8,
                       "FLOAT" = 16,
                       "COMPLEX" = 32,
                       "DOUBLE" = 64,
                       "RGB" = 128)
    bitpix <- switch(type,
                     "UNKNOWN" = 0,
                     "BINARY" = 1,
                     "CHAR" = 8,
                     "SHORT" = 16,
                     "INT" = 32,
                     "FLOAT" = 32,
                     "COMPLEX" = 64,
                     "DOUBLE" = 64,
                     "RGB" = 24)
    slice.start <- integer(1)
    pixdim <- as.numeric(c(0,1,1,1,1,0,0,0))
    vox.offset <- 352
    scl.slope <- scl.inter <- numeric(1)
    slice.end <- slice.code <- xyzt.units <- integer(1)
    cal.max <- cal.min <- numeric(1)
    slice.duration <- numeric(1)
    toffset <- numeric(1)
    glmax <- glmin <- numeric(1)
    ## was data_history substruct in Analyze 7.5
    descrip <- rep("", 80)
    aux.file <- rep("", 24)
    qform.code <- sform.code <- integer(1)
    quatern.b <- quatern.c <- quatern.d <- numeric(1)
    qoffset.x <- qoffset.y <- qoffset.z <- numeric(1)
    srow.x <- srow.y <- srow.z <- numeric(4)
    intent.name <- rep("", 16)
    magic <- "n+1"
    extender <- integer(4)
    
    hdr <-
      list(sizeof.hdr, endian, data.type, db.name, extents, session.error,
           regular, dim.info, dim, intent.p1, intent.p2, intent.p3,
           intent.code, datatype, bitpix, slice.start, pixdim, vox.offset,
           scl.slope, scl.inter, slice.end, slice.code, xyzt.units,
           cal.max, cal.min, slice.duration, toffset, glmax, glmin,
           descrip, aux.file, qform.code, sform.code, quatern.b,
           quatern.c, quatern.d, qoffset.x, qoffset.y, qoffset.z, srow.x,
           srow.y, srow.z, intent.name, magic, extender)
    names(hdr) <-
      c("sizeof.hdr", "endian", "data.type", "db.name", "extents",
        "session.error", "regular", "dim.info", "dim", "intent.p1",
        "intent.p2", "intent.p3", "intent.code", "datatype", "bitpix",
        "slice.start", "pixdim", "vox.offset", "scl.slope", "scl.inter",
        "slice.end", "slice.code", "xyzt.units", "cal.max", "cal.min",
        "slice.duration", "toffset", "glmax", "glmin", "descrip",
        "aux.file", "qform.code", "sform.code", "quatern.b", "quatern.c",
        "quatern.d", "qoffset.x", "qoffset.y", "qoffset.z", "srow.x",
        "srow.y", "srow.z", "intent.name", "magic", "extender")
  }
  return(hdr)
}

read.img <- function(fname, verbose=FALSE, warn=-1, ...) {
  ## Check if any file extensions are present
  ANLZ <- ifelse(length(grep("img", fname)) != 0, TRUE, FALSE)
  NIFTI <- ifelse(length(grep("nii", fname)) != 0, TRUE, FALSE)

  cat.file <- function(fname, thefile, fill=TRUE)
    cat(paste("  fname =", fname, "\n  file =", thefile), fill=fill)

  ## Strip any extensions
  fname <- sub(".gz", "", fname)
  fname <- sub(".img", "", fname)
  fname <- sub(".nii", "", fname)

  fname.img <- paste(fname, "img", sep=".")
  fname.img.gz <- paste(fname, "img.gz", sep=".")
  if (file.exists(fname.img) && file.exists(fname.img.gz)) {
    stop("-- Both compressed and uncompressed files exist! --")
  } else {
    ## If uncompressed files exist, then upload!
    if (file.exists(fname.img)) {
      if (verbose) cat.file(fname, fname.img)
      img <- read.analyze.img(fname, gzipped=FALSE, verbose=verbose,
                              warn=warn)
      return(img)
    }
    ## If compressed files exist, then upload!
    if (file.exists(fname.img.gz)) {
      if (verbose) cat.file(fname, fname.img.gz)
      aim <- read.analyze.img(fname, gzipped=TRUE, verbose=verbose,
                              warn=warn)
      return(img)
    }
  }
  fname.nii <- paste(fname, "nii", sep=".")
  fname.nii.gz <- paste(fname, "nii.gz", sep=".")
  if (file.exists(fname.nii) && file.exists(fname.nii.gz)) {
    stop("-- Both compressed and uncompressed files exist! --")
  } else {
    ## If uncompressed files exist, then upload!
    if (file.exists(fname.nii)) {      
      if (verbose) cat.file(fname, fname.nii)
      img <- read.nifti.img(fname, gzipped=FALSE, verbose=verbose,
                            warn=warn, ...)
      return(img)
    }
    ## If compressed files exist, then upload!
    if (file.exists(fname.nii.gz)) {
      if (verbose) cat.file(fname, fname.nii.gz)
      img <- read.nifti.img(fname, gzipped=TRUE, verbose=verbose,
                            warn=warn, ...)
      return(img)
    }
  }
  ## If you get this far...
  stop(paste(fname, "is not recognized."))
}

read.analyze.img <- function(fname, gzipped=TRUE, signed=FALSE,
                             verbose=FALSE, warn=-1) {
  ##
  ## Datatype Table
  ## -----------------------------------------
  ##   0 = DT_NONE or DT_UNKOWN
  ##   1 = DT_BINARY (1 bit per voxel)
  ##   2 = DT_UNSIGNED_CHAR (8 bits per voxel)
  ##   4 = DT_SIGNED_SHORT (16 bits per voxel)
  ##   8 = DT_SINGED_INT (32 bits per voxel)
  ##  16 = DT_FLOAT (32 bits per voxel)
  ##  32 = DT_COMPLEX (2 x 32 bit single)
  ##  64 = DT_DOUBLE (64 bits per voxel)
  ## 128 = DT_RGB
  ## 255 = DT_ALL
  ##

  ## Warnings?
  oldwarn <- options()$warn
  options(warn=warn)

  ## Read in data from the header file
  hdr <- read.analyze.hdr(fname, gzipped, verbose)
  n <- prod(hdr$dim[2:5])
  size <- hdr$bitpix / 8
  what <- integer()
  switch(as.character(hdr$datatype),
         "2" = { signed <- FALSE },
         "4" = { signed <- TRUE  },
         "8" = { signed <- TRUE  },
         "16" = { what <- numeric() ; signed <- TRUE },
         "32" = {
           signed <- TRUE
           n <- prod(hdr$dim[2:5]) * 2
           size <- hdr$bitpix / 8 / 2
           what <- numeric()
         },
         "64" = { what <- double() ; signed <- TRUE },
         stop(paste("Data type ", hdr$datatype, "unsupported in ", fname,
                    ".img", sep=""))
         )
  ## Read in data from the image file
  if(gzipped)
    fid <- gzfile(paste(fname, ".img.gz", sep=""), "rb")
  else
    fid <- file(paste(fname, ".img", sep=""), "rb")
  image.data <- readBin(fid, what=what, n=n, size=size,
                        signed=signed, endian=hdr$endian)
  close(fid)
  ## Place vector into four-dimensional array
  if(hdr$datatype != 32)
    image.data <- array(image.data, hdr$dim[2:5])
  else {
    odd <- seq(1, n, by=2)
    even <- seq(2, n, by=2)
    image.vec <- complex(real=image.data[odd], imag=image.data[even])
    image.data <- array(image.vec, hdr$dim[2:5])
  }
  ## Warnings?
  options(warn=oldwarn)
  return(image.data)
}

read.nifti.hdr <- function(fname, onefile=TRUE, gzipped=TRUE,
                           verbose=FALSE, warn=-1) {

  ## Convert codes to names
  convert.datatype <- function(dt) {
    ## defgroup NIFTI1_DATATYPE_ALIASES
    ## brief aliases for the nifti1 datatype codes
    switch(as.character(dt),
           "2" = "UINT8",
           "4" = "INT16",
           "8" = "INT32",
           "16" = "FLOAT32",
           "32" = "COMPLEX64",
           "64" = "FLOAT64",
           "128" = "RGB24",
           "256" = "INT8",
           "512" = "UINT16",
           "768" = "UINT32",
           "1024" = "INT64",
           "1280" = "UINT64",
           "1536" = "FLOAT128",
           "1792" = "COMPLEX128",
           "2048" = "COMPLEX256")
  }
  convert.intent <- function(ic) {
    ## defgroup NIFTI1_INTENT_CODES
    ##-------- These codes are for probability distributions -----------
    ## Most distributions have a number of parameters, below denoted
    ## by p1, p2, and p3, and stored in
    ##  - intent_p1, intent_p2, intent_p3 if dataset doesn't have 5th
    ##    dimension
    ##  - image data array                if dataset does have 5th
    ##    dimension
    ##
    ## Functions to compute with many of the distributions below can
    ## be found in the CDF library from U Texas.
    ##
    ## Formulas for and discussions of these distributions can be
    ## found in the following books:
    ##
    ##  [U] Univariate Discrete Distributions,
    ##      NL Johnson, S Kotz, AW Kemp.
    ##
    ##  [C1] Continuous Univariate Distributions, vol. 1,
    ##       NL Johnson, S Kotz, N Balakrishnan.
    ##
    ##  [C2] Continuous Univariate Distributions, vol. 2,
    ##       NL Johnson, S Kotz, N Balakrishnan.
    ##
    switch(as.character(ic),
           "0" = "None",
           "2" = "Correl",
           "3" = "Ttest",
           "4" = "Ftest",
           "5" = "Zscore",
           "6" = "Chisq",
           "7" = "Beta",
           "8" = "Binom",
           "9" = "Gamma",
           "10" = "Poisson",
           "11" = "Normal",
           "12" = "Ftest_Nonc",
           "13" = "Chisq_Nonc",
           "14" = "Logistic",
           "15" = "Laplace",
           "16" = "Uniform",
           "17" = "Ttest_Nonc",
           "18" = "Weibull",
           "19" = "Chi",
           "20" = "Invgauss",
           "21" = "Extval",
           "22" = "Pval",
           "23" = "Logpval",
           "24" = "Log10pval",
           "1001" = "Estimate",      # estimate of some parameter
           "1002" = "Label",         # index into some set of labels
           "1003" = "Neuroname",     # index into the NeuroNames labels set
           "1004" = "Genmatrix",     # M x N matrix at each voxel
           "1005" = "Symmatrix",     # N x N symmetric matrix at each voxel
           "1006" = "Dispvect",      # a displacement field
           "1007" = "Vector",        # a displacement vector
           "1008" = "Pointset",      # a spatial coordinate
           "1009" = "Triangle",      # triple of indexes 
           "1010" = "Quaternion",    # a quaternion
           "1011" = "Dimless")       # Dimensionless value - no params
  }
  convert.form <- function(fc) {
    ## defgroup NIFTI1_XFORM_CODES
    ## brief nifti1 xform codes to describe the "standard" 
    ## coordinate system
    switch(as.character(fc),
           "0" = "Unkown",        # Arbitrary coordinates (Method 1)
           "1" = "Scanner_Anat",  # Scanner-based anatomical coordinates
           "2" = "Aligned_Anat",  # Coordinates aligned to another file's,
                                  # or to anatomical "truth"
           "3" = "Talairach",     # Coordinates aligned to Talairach-
                                  # Tournoux Atlas; (0,0,0)=AC, etc.
           "4" = "MNI_152")       # MNI 152 normalized coordinates
  }
  convert.units <- function(units) {
    ## defgroup NIFTI1_UNITS
    ## brief nifti1 units codes to describe the unit of measurement for
    ## each dimension of the dataset
    switch(as.character(units),
           "0" = "Unkown",        # unspecified units
           "1" = "meter",         # meters
           "2" = "mm",            # millimeters
           "3" = "micron",        # micrometers
           "8" = "sec",           # seconds
           "16" = "msec",         # milliseconds
           "24" = "usec",         # microseconds
           "32" = "Hz",           # Hertz
           "40" = "ppm",          # parts per million
           "48" = "rads")         # radians per second
  }
  convert.slice <- function(sc) {
    switch(as.character(sc),
           "0" = "Unknown",
           "1" = "Seq_Inc",       # sequential increasing
           "2" = "Seq_Dec",       # sequential decreasing
           "3" = "Alt_Inc",       # alternating increasing
           "4" = "Alt_Dec",       # alternating decreasing
           "5" = "Alt_Inc2",      # alternating increasing #2
           "6" = "Alt_Dec2")      # alternating decreasing #2
  }
  ## Bitwise conversion subroutines
  xyzt2space <- function (xyzt) {
    ## define XYZT_TO_SPACE(xyzt)       ( (xyzt) & 0x07 )
    require("bitops")
    bitAnd(xyzt, 7)
  }
  xyzt2time <- function (xyzt) {
    ## define XYZT_TO_TIME(xyzt)        ( (xyzt) & 0x38 )
    require("bitops")
    bitAnd(xyzt, 56)
  }
  dim2freq <- function(di) {
    ## define DIM_INFO_TO_FREQ_DIM(di)   ( ((di)     ) & 0x03 )
    require("bitops")
    bitAnd(di, 3)
  }
  dim2phase <- function(di) {
    ## define DIM_INFO_TO_PHASE_DIM(di)  ( ((di) >> 2) & 0x03 )
    require("bitops")
    bitAnd(bitShiftR(di, 2), 3)
  }
  dim2slice <- function(di) {
    ## define DIM_INFO_TO_SLICE_DIM(di)  ( ((di) >> 4) & 0x03 )
    require("bitops")
    bitAnd(bitShiftR(di, 4), 3)
  }
  quaternion2rotation <- function(b, c, d) {
    a <- sqrt(1 - (b*b+c*c+d*d))
    R <- matrix(c(a*a+b*b-c*c-d*d, 2*b*c+2*a*d, 2*b*d-2*a*c,  # column 1
                  2*b*c-2*a*d, a*a+c*c-b*b-d*d, 2*c*d+2*a*b,  # column 2
                  2*b*d+2*a*c, 2*c*d-2*a*b, a*a+d*d-c*c-b*b), # column 3
                3, 3)
    return(R)
  }

  ## Warnings?
  oldwarn <- options()$warn
  options(warn=warn)

  ## Open appropriate file
  if(gzipped) {
    suffix <- ifelse(onefile, "nii.gz", "hdr.gz")
    fname <- paste(fname, suffix, sep=".")
    fid <- gzfile(fname, "rb")
  }
  else {
    suffix <- ifelse(onefile, "nii", "hdr")
    fname <- paste(fname, suffix, sep=".")
    fid <- file(fname, "rb")
  }

  ## Test for endian properties
  endian <- .Platform$endian
  sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
  if(sizeof.hdr != 348) {
    close(fid)
    endian <- "swap"
    if(gzipped)
      fid <- gzfile(fname, "rb")
    else
      fid <- file(fname, "rb")
    sizeof.hdr <- readBin(fid, integer(), size=4, endian=endian)
    if(verbose) cat("  ENDIAN = swap", fill=TRUE)
  }

  ## was header_key substruct in Analyze 7.5
  data.type <- rawToChar(readBin(fid, "raw", n=10))
  db.name <- rawToChar(readBin(fid, "raw", n=18))
  extents <- readBin(fid, integer(), size=4, endian=endian)
  session.error <- readBin(fid, integer(), size=2, endian=endian)
  regular <- rawToChar(readBin(fid, "raw", n=1))
  dim.info <- readBin(fid, integer(), size=1, signed=FALSE, endian=endian)

  ## was image_dimension substruct in Analyze 7.5
  dim <- readBin(fid, integer(), 8, size=2, endian=endian)
  intent.p1 <- readBin(fid, numeric(), size=4, endian=endian)
  intent.p2 <- readBin(fid, numeric(), size=4, endian=endian)
  intent.p3 <- readBin(fid, numeric(), size=4, endian=endian)
  intent.code <- readBin(fid, integer(), size=2, endian=endian)
  datatype <- readBin(fid, integer(), size=2, endian=endian)
  bitpix <- readBin(fid, integer(), size=2, endian=endian)
  slice.start <- readBin(fid, integer(), size=2, endian=endian)
  pixdim <- readBin(fid, numeric(), 8, size=4, endian=endian)
  vox.offset <- readBin(fid, numeric(), size=4, endian=endian)
  scl.slope <- readBin(fid, numeric(), size=4, endian=endian)
  scl.inter <- readBin(fid, numeric(), size=4, endian=endian)
  slice.end <- readBin(fid, integer(), size=2, endian=endian)
  slice.code <- readBin(fid, integer(), size=1, signed=FALSE, endian=endian)
  xyzt.units <- readBin(fid, integer(), size=1, signed=FALSE, endian=endian)
  cal.max <- readBin(fid, numeric(), size=4, endian=endian)
  cal.min <- readBin(fid, numeric(), size=4, endian=endian)
  slice.duration <- readBin(fid, numeric(), size=4, endian=endian)
  toffset <- readBin(fid, numeric(), size=4, endian=endian)
  glmax <- readBin(fid, integer(), size=4, endian=endian)
  glmin <- readBin(fid, integer(), size=4, endian=endian)

  ## was data_history substruct in Analyze 7.5
  descrip <- rawToChar(readBin(fid, "raw", n=80))
  aux.file <- rawToChar(readBin(fid, "raw", n=24))

  qform.code <- readBin(fid, integer(), size=2, endian=endian)
  sform.code <- readBin(fid, integer(), size=2, endian=endian)

  quatern.b <- readBin(fid, numeric(), size=4, endian=endian)
  quatern.c <- readBin(fid, numeric(), size=4, endian=endian)
  quatern.d <- readBin(fid, numeric(), size=4, endian=endian)
  qoffset.x <- readBin(fid, numeric(), size=4, endian=endian)
  qoffset.y <- readBin(fid, numeric(), size=4, endian=endian)
  qoffset.z <- readBin(fid, numeric(), size=4, endian=endian)

  srow.x <- readBin(fid, numeric(), 4, size=4, endian=endian)
  srow.y <- readBin(fid, numeric(), 4, size=4, endian=endian)
  srow.z <- readBin(fid, numeric(), 4, size=4, endian=endian)

  intent.name <- rawToChar(readBin(fid, "raw", n=16))

  magic <- rawToChar(readBin(fid, "raw", n=4))
  if (!(magic %in% c("n+1","ni1")))
    stop(" -- Unrecognized \"magic\" field! --")

  extender <- readBin(fid, integer(), 4, size=1, signed=FALSE, endian=endian)
  if (extender[1] != 0)
    stop("WARNING: Header extensions exist!")
  
  close(fid)

  ## Additional fields...
  slice.dim <- dim2slice(dim.info)
  slice.name <-
    ifelse(slice.code != 0 && slice.dim != 0 && slice.duration > 0,
           convert.slice(slice.code), "Unknown")

  nhdr <- list("sizeof.hdr" = sizeof.hdr, "endian" = endian,
               "db.name" = db.name, "extents" = extents,
               "session.error" = session.error, "regular" = regular,
               "dim.info" = dim.info, "dim" = dim, "intent.p1" = intent.p1,
               "intent.p2" = intent.p2, "intent.p3" = intent.p3,
               "intent.code" = intent.code, "datatype" = datatype,
               "bitpix" = bitpix, "slice.start" = slice.start,
               "pixdim" = pixdim, "vox.offset" = vox.offset,
               "scl.slope" = scl.slope, "scl.inter" = scl.inter,
               "slice.end" = slice.end, "slice.code" = slice.code,
               "xyzt.units" = xyzt.units, "cal.max" = cal.max,
               "cal.min" = cal.min, "slice.duration" = slice.duration,
               "toffset" = toffset, "glmax" = glmax, "glmin" = glmin,
               "descrip" = descrip, "aux.file" = aux.file,
               "qform.code" = qform.code, "sform.code" = sform.code,
               "quatern.b" = quatern.b, "quatern.c" = quatern.c,
               "quatern.d" = quatern.d, "qoffset.x" = qoffset.x,
               "qoffset.y" = qoffset.y, "qoffset.z" = qoffset.z,
               "srow.x" = srow.x, "srow.y" = srow.y, "srow.z" = srow.z,
               "intent.name" = intent.name, "magic" = magic,
               "extender" = extender, 
               "freq.dim" = dim2freq(dim.info),
               "phase.dim" = dim2phase(dim.info),
               "slice.dim" = slice.dim,
               "intent" = convert.intent(intent.code),
               "slice.name" = slice.name,
               "data.type" = ifelse(data.type == "",
                 convert.datatype(datatype), ""),
               "vox.units" = convert.units(xyzt2space(xyzt.units)),
               "time.units" = convert.units(xyzt2time(xyzt.units)),
               "qform.name" = convert.form(qform.code),
               "sform.name" = convert.form(sform.code),
               "qfac" = pixdim[1],
               "R" = quaternion2rotation(quatern.b, quatern.c, quatern.d))

  ## Warnings?
  options(warn=oldwarn)
  return(nhdr)
}

read.nifti.img <- function(fname, onefile=TRUE, gzipped=TRUE,
                           verbose=FALSE, warn=-1, ignoreQform=FALSE,
                           ignoreSform=FALSE) {
  ## --- the original ANALYZE 7.5 type codes ---
  ## DT_NONE                    0
  ## DT_UNKNOWN                 0     # what it says, dude
  ## DT_BINARY                  1     # binary (1 bit/voxel)
  ## DT_UNSIGNED_CHAR           2     # unsigned char (8 bits/voxel)
  ## DT_SIGNED_SHORT            4     # signed short (16 bits/voxel)
  ## DT_SIGNED_INT              8     # signed int (32 bits/voxel)
  ## DT_FLOAT                  16     # float (32 bits/voxel)
  ## DT_COMPLEX                32     # complex (64 bits/voxel)
  ## DT_DOUBLE                 64     # double (64 bits/voxel)
  ## DT_RGB                   128     # RGB triple (24 bits/voxel)
  ## DT_ALL                   255     # not very useful (?)
  ## ----- another set of names for the same ---
  ## DT_UINT8                   2
  ## DT_INT16                   4
  ## DT_INT32                   8
  ## DT_FLOAT32                16
  ## DT_COMPLEX64              32
  ## DT_FLOAT64                64
  ## DT_RGB24                 128
  ## ------------------- new codes for NIFTI ---
  ## DT_INT8                  256     # signed char (8 bits)
  ## DT_UINT16                512     # unsigned short (16 bits)
  ## DT_UINT32                768     # unsigned int (32 bits)
  ## DT_INT64                1024     # long long (64 bits)
  ## DT_UINT64               1280     # unsigned long long (64 bits)
  ## DT_FLOAT128             1536     # long double (128 bits)
  ## DT_COMPLEX128           1792     # double pair (128 bits)
  ## DT_COMPLEX256           2048     # long double pair (256 bits)
  
  ## Read in data from the header file
  nhdr <- read.nifti.hdr(fname, onefile, gzipped, verbose, warn)

  if(nhdr$datatype == 2) {
    ## 1 byte unsigned integer
    what <- integer()
    size <- nhdr$bitpix / 8
    signed <- FALSE
  }
  else {
    if(nhdr$datatype == 4 | nhdr$datatype == 8) {
      ## 1 or 2 byte short integers
      what <- integer()
      size <- nhdr$bitpix / 8
      signed <- TRUE
    }
    else {
      if(nhdr$datatype == 16) {
        ## 4 byte floats
        what <- numeric()
        size <- nhdr$bitpix / 8
        signed <- TRUE
      }
      else {
        if(nhdr$datatype == 64) {
          ## 8 byte doubles
          what <- double()
          size <- nhdr$bitpix / 8
          signed <- TRUE
        }
        else {
          if(nhdr$datatype == 512) {
            ## 2 byte unsigned short integers
            what <- integer()
            size <- nhdr$bitpix / 8
            signed <- FALSE
          }
          else
            stop(paste("Data type ", nhdr$datatype, "unsupported in ", fname,
                       ".img", sep=""))
        }
      }
    }
  }

  ## Warnings?
  oldwarn <- options()$warn
  options(warn=warn)

  n <- prod(nhdr$dim[2:5])
  ## Open appropriate file
  if(onefile) {
    if(gzipped)
      fid <- gzfile(paste(fname, "nii.gz", sep="."), "rb")
    else
      fid <- file(paste(fname, "nii", sep="."), "rb")
    skip <- readBin(fid, integer(), nhdr$vox.offset, size=1,
                    endian=nhdr$endian)
    img <-  readBin(fid, what, n, size, signed, endian=nhdr$endian)
  }
  else {
    if(gzipped)
      fid <- gzfile(paste(fname, "img.gz", sep="."), "rb")
    else
      fid <- file(paste(fname, "img", sep="."), "rb")
    img <- readBin(fid, what, n, size, signed, endian=nhdr$endian)
  }
  close(fid)

  ## convert to four-dimensional array (depends on nhdr$dim)
  img.array <- array(img, nhdr$dim[2:5])

  ## check for qform and/or sform flags
  if (nhdr$qform.code > 0 && !ignoreQform) {
    if (verbose)
      print("NIfTI-1: qform_code > 0")
    ## qoffset <- c(nhdr$qoffset.x, nhdr$qoffset.x, nhdr$qoffset.x)
    R <- nhdr$R
    if (nhdr$qfac < 0)
      R[3,3] <- -R[3,3]
    if (all(abs(sign(R)) == diag(3))) {
      x <- 1:nrow(img.array) * R[1,1]
      y <- 1:ncol(img.array) * R[2,2]
      z <- 1:nsli(img.array) * R[3,3]
      img.array <- img.array[order(x),order(y),order(z),,drop=FALSE]
    } else {
      stop("NIfTI-1: Rotation matrix is NOT diagonal with +/- 1s")
    }
  }
  if (nhdr$sform.code > 0 && !ignoreSform) {
    if (verbose)
      print("NIfTI-1: sform_code > 0")
    x <- nhdr$srow.x[1] * 1:nrow(img.array) + nhdr$srow.x[4]
    y <- nhdr$srow.y[2] * 1:ncol(img.array) + nhdr$srow.y[4]
    z <- nhdr$srow.z[3] * 1:nsli(img.array) + nhdr$srow.z[4]
    img.array <- img.array[order(x),order(y),order(z),,drop=FALSE]
  }
  img.array <- img.array[nrow(img.array):1,,,,drop=FALSE]

  ## Warnings?
  options(warn=oldwarn)
  return(img.array)
}
