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
##
## Time-stamp: <>
##

write.analyze.img <- function(fname, hdr, img, type, gzipped=TRUE, warn=-1) {
  ## Warnings?
  oldwarn <- options()$warn
  options(warn=warn)
  ## Basic error checking
  if(length(img) != prod(hdr$dim[2:5]))
    stop("Header and image dimensions do not match")

  hdr$cal.min <- min(img)
  hdr$cal.max <- max(img)
  
  ## Deal with different types...
  switch(type,
         ## 1  bit
         "uint1" = {
           hdr$datatype <- as.integer(1)
           hdr$bitpix <- as.integer(1)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(1)
           hdr$funused1 <- 1
         },
         ## 8  bit
         "uint8" = {
           hdr$datatype <- as.integer(2)
           hdr$bitpix <- as.integer(8)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(255)
           hdr$funused1 <- 1
           img <- as.integer(img)
         }, # stop("You should write a float image, not integer"),
         ## 16 bit
         "int16" = {
           hdr$datatype <- as.integer(4)
           hdr$bitpix <- as.integer(16)
             if(abs(hdr$cal.min) > abs(hdr$cal.max))
               hdr$funused1 <- abs(hdr$cal.min) / (2^15-1)
             else
               hdr$funused1 <- abs(hdr$cal.max) / (2^15-1)
           hdr$glmin <- as.integer(hdr$funused1 * hdr$cal.min)
           hdr$glmax <- as.integer(hdr$funused1 * hdr$cal.max)
           img <- as.integer(img)
         },
         ## 32 bit
         "int32" = {
           hdr$datatype <- as.integer(8)
           hdr$bitpix <- as.integer(32)
           if(abs(hdr$cal.min) > abs(hdr$cal.max))
             hdr$funused1 <- abs(hdr$cal.min) / (2^31-1)
           else
             hdr$funused1 <- abs(hdr$cal.max) / (2^31-1)
           hdr$glmin <- round(hdr$funused1 * hdr$cal.min)
           hdr$glmax <- round(hdr$funused1 * hdr$cal.max)
           img <- as.integer(img)
         },
         ## float  (32 bit)
         "float" = {
           hdr$datatype <- as.integer(16)
           hdr$bitpix <- as.integer(32)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(0)
           hdr$funused1 <- 1
           img <- as.numeric(img)
         },
         ## double (64 bit)
         "double" = {
           hdr$datatype <- as.integer(64)
           hdr$bitpix <- as.integer(64)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(0)
           hdr$funused1 <- 1
           img <- as.numeric(img)
         },
         stop("Unrecognised precision (type = ?)"))
  
  ## Write image file...
  if(gzipped)
    fid <- gzfile(paste(fname, ".img.gz", sep=""), "wb")
  else
    fid <- file(paste(fname, ".img", sep=""), "wb")
  writeBin(img, fid, endian=hdr$endian, size=hdr$bitpix/8)
  close(fid)

  ## Write header file...
  null <- ""
  if(gzipped)
    fid <- gzfile(paste(fname, ".hdr.gz", sep=""), "wb")
  else
    fid <- file(paste(fname, ".hdr", sep=""), "wb")

  writeBin(hdr$size.of.hdr, fid, endian=hdr$endian, size=4)
  writeChar(hdr$db.type, fid, nchar=10, eos=NULL)
  writeChar(hdr$db.name, fid, nchar=18, eos=NULL)
  writeBin(hdr$extents, fid, endian=hdr$endian, size=4)
  writeBin(hdr$session.error, fid, endian=hdr$endian, size=2)
  writeChar(hdr$regular, fid, nchar=1, eos=NULL)
  writeChar(hdr$hkey.un0, fid, nchar=1, eos=NULL)
  writeBin(as.integer(hdr$dim), fid, endian=hdr$endian, size=2)

  for(i in 1:14)
    writeBin(null, fid, endian=hdr$endian)

  writeBin(hdr$datatype, fid, endian=hdr$endian, size=2)
  writeBin(hdr$bitpix, fid, endian=hdr$endian, size=2)
  writeBin(hdr$dim.un0, fid, endian=hdr$endian, size=2)
  writeBin(hdr$pixdim, fid, endian=hdr$endian, size=4)
  writeBin(hdr$vox.offset, fid, endian=hdr$endian, size=4)

  writeChar(hdr$vox.units, fid, nchar=4, eos=NULL)
  writeChar(hdr$cal.units, fid, nchar=8, eos=NULL)

  writeBin(as.integer(hdr$cal.max), fid, endian=hdr$endian, size=4)
  writeBin(as.integer(hdr$cal.min), fid, endian=hdr$endian, size=4)
  writeBin(hdr$compressed, fid, endian=hdr$endian, size=4)
  writeBin(hdr$verified, fid, endian=hdr$endian, size=4)
  writeBin(hdr$glmax, fid, endian=hdr$endian, size=4)
  writeBin(hdr$glmin, fid, endian=hdr$endian, size=4)

  writeChar(hdr$descrip, fid, nchar=80, eos=NULL)
  writeChar(hdr$aux.file, fid, nchar=24, eos=NULL)
  writeChar(hdr$orient, fid, nchar=1, eos=NULL)
  writeChar(hdr$originator, fid, nchar=10, eos=NULL)
  writeChar(hdr$generated, fid, nchar=10, eos=NULL)
  writeChar(hdr$scannum, fid, nchar=10, eos=NULL)
  writeChar(hdr$patient.id, fid, nchar=10, eos=NULL)
  writeChar(hdr$exp.date, fid, nchar=10, eos=NULL)
  writeChar(hdr$exp.time, fid, nchar=10, eos=NULL)
  writeChar(hdr$hist.un0, fid, nchar=3, eos=NULL)

  writeBin(hdr$views, fid, endian=hdr$endian, size=4)
  writeBin(hdr$vols.added, fid, endian=hdr$endian, size=4)
  writeBin(hdr$start.field, fid, endian=hdr$endian, size=4)
  writeBin(hdr$field.skip, fid, endian=hdr$endian, size=4)
  writeBin(hdr$omax, fid, endian=hdr$endian, size=4)
  writeBin(hdr$omin, fid, endian=hdr$endian, size=4)
  writeBin(hdr$smax, fid, endian=hdr$endian, size=4)
  writeBin(hdr$smin, fid, endian=hdr$endian, size=4)
  close(fid)
  options(warn=oldwarn)
  invisible()
}

write.nifti.img <- function(fname, hdr, img, type, gzipped=TRUE, warn=-1) {
  ## Warnings?
  oldwarn <- options()$warn
  options(warn=warn)
  ## Basic error checking
  if (length(dim(img))<4) {
    img<-array(img,dim=c(dim(img),rep(1,4-length(dim(img)))))
  }
  img<-img[dim(img)[1]:1,,,,drop=FALSE]
  if(length(img) != prod(hdr$dim[2:5]))
    stop("Header and image dimensions do not match")

  hdr$cal.min <- min(img, na.rm=TRUE)
  hdr$cal.max <- max(img, na.rm=TRUE)
  
  ## Deal with different types...
  switch(type,
         ## 1  bit
         "uint1" = {
           hdr$data.type <- "UINT1"
           hdr$data.type <- as.integer(1)
           hdr$bitpix <- as.integer(1)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(1)
           hdr$funused1 <- 1
         },
         ## 8  bit
         "uint8" = {
           hdr$data.type <- "UINT8"
           hdr$datatype <- as.integer(2)
           hdr$bitpix <- as.integer(8)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(255)
           hdr$funused1 <- 1
           img <- as.integer(img)
         }, # stop("You should write a float image, not integer"),
         ## 16 bit
         "int16" = {
           hdr$data.type <- "INT16"
           hdr$datatype <- as.integer(4)
           hdr$bitpix <- as.integer(16)
             if(abs(hdr$cal.min) > abs(hdr$cal.max))
               hdr$funused1 <- abs(hdr$cal.min) / (2^15-1)
             else
               hdr$funused1 <- abs(hdr$cal.max) / (2^15-1)
           hdr$glmin <- as.integer(hdr$funused1 * hdr$cal.min)
           hdr$glmax <- as.integer(hdr$funused1 * hdr$cal.max)
           img <- as.integer(img)
         },
         ## 32 bit integer
         "int32" = {
           hdr$data.type <- "INT32"
           hdr$datatype <- as.integer(8)
           hdr$bitpix <- as.integer(32)
           if(abs(hdr$cal.min) > abs(hdr$cal.max))
             hdr$funused1 <- abs(hdr$cal.min) / (2^31-1)
           else
             hdr$funused1 <- abs(hdr$cal.max) / (2^31-1)
           hdr$glmin <- round(hdr$funused1 * hdr$cal.min)
           hdr$glmax <- round(hdr$funused1 * hdr$cal.max)
           img <- as.integer(img)
         },
         ## 32 bit float
         "float" = {
           hdr$data.type <- "FLOAT32"
           hdr$datatype <- as.integer(16)
           hdr$bitpix <- as.integer(32)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(0)
           hdr$funused1 <- 1
           img <- as.numeric(img)
         },
         ## 64 bit double
         "double" = {
           hdr$data.type <- "FLOAT64"
           hdr$datatype <- as.integer(64)
           hdr$bitpix <- as.integer(64)
           hdr$glmin <- as.integer(0)
           hdr$glmax <- as.integer(0)
           hdr$funused1 <- 1
           img <- as.numeric(img)
         },
         stop("Unrecognised precision (type = ?)"))
  
  ## Write header file...
  null <- ""
  if(gzipped)
    fid <- gzfile(paste(fname, ".nii.gz", sep=""), "wb")
  else
    fid <- file(paste(fname, ".nii", sep=""), "wb")

  writeBin(hdr$sizeof.hdr, fid, endian=hdr$endian, size=4)
  writeChar(hdr$data.type, fid, nchar=10, eos=NULL)
  writeChar(hdr$db.name, fid, nchar=18, eos=NULL)
  writeBin(hdr$extents, fid, endian=hdr$endian, size=4)
  writeBin(hdr$session.error, fid, endian=hdr$endian, size=2)
  writeChar(hdr$regular, fid, nchar=1, eos=NULL)
  writeBin(hdr$dim.info, fid, endian=hdr$endian, size=1)

  writeBin(as.integer(hdr$dim), fid, endian=hdr$endian, size=2)
  writeBin(hdr$intent.p1, fid, endian=hdr$endian, size=4)
  writeBin(hdr$intent.p2, fid, endian=hdr$endian, size=4)
  writeBin(hdr$intent.p3, fid, endian=hdr$endian, size=4)
  writeBin(hdr$intent.code, fid, endian=hdr$endian, size=2)
  writeBin(hdr$datatype, fid, endian=hdr$endian, size=2)
  writeBin(hdr$bitpix, fid, endian=hdr$endian, size=2)
  writeBin(hdr$slice.start, fid, endian=hdr$endian, size=2)
  writeBin(hdr$pixdim, fid, endian=hdr$endian, size=4)
  writeBin(hdr$vox.offset, fid, endian=hdr$endian, size=4)
  writeBin(hdr$scl.slope, fid, endian=hdr$endian, size=4)
  writeBin(hdr$scl.inter, fid, endian=hdr$endian, size=4)
  writeBin(hdr$slice.end, fid, endian=hdr$endian, size=2)
  writeBin(hdr$slice.code, fid, endian=hdr$endian, size=1)
  writeBin(hdr$xyzt.units, fid, endian=hdr$endian, size=1)
  writeBin(hdr$cal.max, fid, endian=hdr$endian, size=4)
  writeBin(hdr$cal.min, fid, endian=hdr$endian, size=4)
  writeBin(hdr$slice.duration, fid, endian=hdr$endian, size=4)
  writeBin(hdr$toffset, fid, endian=hdr$endian, size=4)
  writeBin(hdr$glmax, fid, endian=hdr$endian, size=4)
  writeBin(hdr$glmin, fid, endian=hdr$endian, size=4)

  writeChar(hdr$descrip, fid, nchar=80, eos=NULL)
  writeChar(hdr$aux.file, fid, nchar=24, eos=NULL)

  writeBin(hdr$qform.code, fid, endian=hdr$endian, size=2)
  writeBin(hdr$sform.code, fid, endian=hdr$endian, size=2)
  
  writeBin(hdr$quatern.b, fid, endian=hdr$endian, size=4)
  writeBin(hdr$quatern.c, fid, endian=hdr$endian, size=4)
  writeBin(hdr$quatern.d, fid, endian=hdr$endian, size=4)
  writeBin(hdr$qoffset.x, fid, endian=hdr$endian, size=4)
  writeBin(hdr$qoffset.y, fid, endian=hdr$endian, size=4)
  writeBin(hdr$qoffset.z, fid, endian=hdr$endian, size=4)
  
  writeBin(hdr$srow.x, fid, endian=hdr$endian, size=4)
  writeBin(hdr$srow.y, fid, endian=hdr$endian, size=4)
  writeBin(hdr$srow.z, fid, endian=hdr$endian, size=4)

  writeChar(hdr$intent.name, fid, nchar=16, eos=NULL)

  writeChar(hdr$magic, fid, nchar=4, eos=NULL)
  
  writeBin(hdr$extender, fid, endian=hdr$endian, size=1)

  ## Write image file...
  if (hdr$qform.code > 0) {
    # if (verbose)
    stop("NIfTI-1: qform_code > 0")
  }
  if (hdr$sform.code > 0) {
    # if (verbose)
    print("NIfTI-1: sform_code > 0")
    x <- (1:hdr$dim[2] - hdr$srow.x[4]) / hdr$srow.x[1]
    y <- (1:hdr$dim[3] - hdr$srow.y[4]) / hdr$srow.y[2]
    z <- (1:hdr$dim[4] - hdr$srow.z[4]) / hdr$srow.z[3]
    img <- img[order(x),order(y),order(z),]
  }
  
  writeBin(img, fid, endian=hdr$endian, size=hdr$bitpix/8)
  close(fid)
  options(warn=oldwarn)
  invisible()
}
