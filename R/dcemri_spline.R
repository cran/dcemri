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
## $Id: dcemri_spline.R 191 2009-08-25 15:12:31Z bjw34032 $
##

dcemri.spline.single <- function(conc, time, D, time.input, p, rw, knots,
                                 k, A, t0.compute=FALSE, nlr=FALSE,
                                 nriters=500, thin=5, burnin=100,
                                 ab.hyper=c(1e-5,1e-5),
                                 ab.tauepsilon=c(1,1/1000), silent=0,
                                 multicore=FALSE, model=NULL, model.func=NULL,
                                 model.guess=NULL, samples=FALSE, B=NULL) {

  ## require("minpack.lm")
  require("minpack.lm")
  ## Sanity check: conc must not contain any missing values
  if (any(is.na(conc)))
    return(NA)

  T <- length(time)
  tau <- array(1000, c(p-rw,nriters))
  beta <- array(0, c(p,nriters))
  MAX <- array(0, c(nriters))
  tauepsilon <- array(1000, c(nriters))
  burnin <- min(burnin, nriters)
  result <- .C("dce_spline_run",
               as.integer(1),
               as.integer(burnin),
               as.integer(c(1,1,1,T)),
               as.double(conc),
               as.double(tau),
               as.double(tauepsilon),
               as.double(D),
               as.integer(rw),
               as.double(beta),
               as.double(c(ab.hyper[1], ab.hyper[2], ab.tauepsilon[1],
                           ab.tauepsilon[2])),
               as.integer(p),
               as.double(1:T),
               as.double(1:T),
               as.double(1:T),
               as.double(1:(T^2)),
               as.double(1:(T^2)),
               as.double(1:(T^2)),
               as.double(1:(T^2)),
               as.double(t(D)),
               as.integer(silent),
               PACKAGE="dcemri")

  tau <- array(result[[5]], c(p-rw,nriters))
  beta <- array(result[[9]], c(p,nriters))
  tauepsilon <- array(result[[6]], c(nriters))

  result <- .C("dce_spline_run",
               as.integer(nriters),
               as.integer(thin),
               as.integer(c(1,1,1,T)),
               as.double(conc),
               as.double(tau),
               as.double(tauepsilon),
               as.double(D),
               as.integer(rw),
               as.double(beta),
               as.double(c(ab.hyper[1], ab.hyper[2], ab.tauepsilon[1],
                           ab.tauepsilon[2])),
               as.integer(p),
               as.double(1:T),
               as.double(1:T),
               as.double(1:T),
               as.double(1:(T^2)),
               as.double(1:(T^2)),
               as.double(1:(T^2)),
               as.double(1:(T^2)),
               as.double(t(D)),
               as.integer(silent),
               PACKAGE="dcemri")

  tau <- array(result[[5]], c(p-rw,nriters))
  beta <- array(result[[9]], c(p,nriters))
  tauepsilon <- array(result[[6]], c(nriters))

  t0 <- time[1]
  if(t0.compute) {
    d <- array(NA, c(T, nriters))
    for (j in 1:nriters)
      d[,j] <- D %*% beta[,j]

    q05 <- function(x) {
      quantile(x, .005, na.rm=TRUE)
    }
    med.na <- function(x) {
      median(x, na.rm=TRUE)
    }

    d1 <- apply(d, 1, q05)
    d2 <- apply(d, 1, med.na)

    du <- min(which(d1 > 0))

    beta.abl <- beta.abl2 <- rep(0, p)

    B2 <- splineDesign(knots, time.input, k-2)
    B2 <- B2[,(1:p)+1]

    for (j in 1:nriters) {
      beta.abl <- 1:p
      for (q in 1:(p - 1))
	beta.abl[q] <- (beta[q+1, j] - beta[q, j]) * k / (knots[q+k+1] - knots[q+1])
      beta.abl[p] <- 0
      ABL2 <- A %*% B2 %*% beta.abl
      du2 <- time[du] - d2[du] / ABL2[du]  
      t0[j] <- du2
    }
    if (t0 < 0)
      t0 <- 0
    if (t0 > max(time))
      t0 <- 0
  }

  fitted <- list()
  for (i in 1:nriters)
    fitted[[i]] <- B%*% beta[,i]
  if (multicore) {
    ## require("multicore")
    require("multicore")
    MAX0 <- mclapply(fitted, max)
  } else {
    MAX0 <- lapply(fitted, max)
  }
  MAX <- NULL
  for (i in 1:nriters)
    MAX <- c(MAX, MAX0[[i]])

  parameters <- list()

  if (nlr) {
    if (model=="AATH") 
      model.guess[2] <- median(MAX)

    fcn <- function(p, time, x, N.Err, fcall, jcall) {
      (x - do.call("fcall", c(list(time=time), as.list(p))))
    }
    nls.lm.single <- function(fitted, par, fn, fcall, model, time) {
      fcall2 <- fcall
      if (length(fcall) > 1)
	fcall2 <- fcall[[1]]
      fit <- nls.lm(par=par, fn=fn, fcall=fcall2, time=time, x=fitted,
                    N.Err=sqrt(300), control=list(nprint=0, ftol=10^-20))
      if (model=="AATH" && fit$par$TC < 1e-6) {
	fit <- nls.lm(par=par[-3], fn=fn, fcall=fcall[[2]], time=time,
                      x=fitted, N.Err=sqrt(300),
                      control=list(nprint=0, ftol=10^-20))
	fit$par$TC <- 0
      }
      return(fit)
    }

    if (multicore) {
      ## require("multicore")
      require("multicore")
      response <- mclapply(fitted, nls.lm.single, par=model.guess,
                           fn=fcn, fcall=model.func, model=model,
                           time=time-t0)
    } else {
      response <- lapply(fitted, nls.lm.single, par=model.guess,
                         fn=fcn, fcall=model.func, model=model,
                         time=time-t0)
    }

    if (model=="AATH") {
      E <- F <- TC <- ve <- NULL
      for (i in 1:nriters) {
	E <- c(E, response[[i]]$par$E)
	F <- c(F, response[[i]]$par$F)
	TC <- c(TC, response[[i]]$par$TC)
	ve <- c(ve, response[[i]]$par$ve)
      }
      parameters <- list("E"=median(E), "F"=median(F), "TC"=median(TC),
                         "ve"=median(ve))
      if(samples) 
	parameters <- list("E.samples"=E, "F.samples"=F, "TC.samples"=TC,
                           "ve.samples"=ve)
    }
    if (model == "weinmann") {
      ktrans <- kep <- NULL
      for (i in 1:nriters) {
	ktrans <- c(ktrans,response[[i]]$par$logktrans)
	kep <- c(kep,response[[i]]$par$logkep)
      }
      parameters <- list("ktrans"=median(exp(ktrans)), "kep"=median(exp(kep)))
      if (samples) 
	parameters <- list("ktrans.sample"=exp(ktrans), "kep.sample"=exp(kep))
    }

  }

  list("beta"=beta, "tau"=tau, "tauepsilon"=tauepsilon, "t0"=t0,
       "Fp"=median(MAX), "Fp.samples"=MAX, "fitted"=fitted, "par"=parameters)
}


dcemri.spline <- function(conc, time, img.mask, time.input=time,
                          model="weinmann", aif="tofts.kermode",
                          user=NULL, aif.observed=NULL, nriters=500,
                          thin=5, burnin=100, ab.hyper=c(1e-5,1e-5),
                          ab.tauepsilon=c(1,1/1000), k=4, p=25, rw=2,
                          knots=NULL, nlr=FALSE, t0.compute=FALSE,
                          samples=FALSE, multicore=FALSE, verbose=FALSE,
                          ...) {

  ## dcemri.spline - a function for fitting Bayesian Penalty Splines to 
  ## DCE-MRI images and computing kinetic parameters
  ##
  ## authors: Volker Schmid, Brandon Whitcher
  ##
  ## input:
  ##        conc: array of Gd concentration,
  ##        time: timepoints of aquisition,
  ##        img.mask: array of voxels to fit,
  ##        D(=0.1): Gd dose in mmol/kg,
  ##        model: AIF... "weinman" or "parker",
  ##
  ## output: list with ktrans, kep, ve, std.error of ktrans and kep
  ##         (ktranserror and keperror)
  ##

  require("splines")
  if (nlr) require("minpack.lm")

  ##function to make precision matrix for random walk
  R <- function(taux,rw) {
    RR <- matrix(0,nrow=length(taux)+rw,ncol=length(taux)+rw)
    if (rw==0) {
      for (i in 1:length(taux))
	RR[i,i] <- taux[i]
    }
    if (rw==1) {
      for (i in 1:length(taux)) {
	RR[i,i] <- RR[i,i]+taux[i]
	RR[i+1,i+1] <- RR[i+1,i+1]+taux[i]
	RR[i+1,i] <- RR[i+1,i]-taux[i]
	RR[i,i+1] <- RR[i,i+1]-taux[i]
      }
    }
    if (rw==2) {
      for (i in 1:length(taux)) {
	RR[i,i] <- RR[i,i]+taux[i]
	RR[i+1,i+1] <- RR[i+1,i+1]+4*taux[i]
	RR[i+2,i+2] <- RR[i+2,i+2]+taux[i]
	RR[i+1,i] <- RR[i+1,i]-2*taux[i]
	RR[i,i+1] <- RR[i,i+1]-2*taux[i]
	RR[i+2,i+1] <- RR[i+2,i+1]-2*taux[i]
	RR[i+1,i+2] <- RR[i+1,i+2]-2*taux[i]
	RR[i+2,i] <- RR[i+2,i]+taux[i]
	RR[i,i+2] <- RR [i,i+2]+taux[i]
      }
    }
    return(RR)
  }

  ## main function

  knotpoints <- p
  if (is.null(knots)) {
    knots <- seq(
      min(time) - 1e-3 - (k - 1)*(max(time) - min(time))/(knotpoints - k + 1),
      1e-3 + max(time) + k*(max(time)-min(time))/(knotpoints - k + 1),
      (2e-3 + max(time) - min(time))/(knotpoints - k + 1))
  }
  mod <- model
  nvoxels <- sum(img.mask)
  I <- nrow(conc)
  J <- ncol(conc)
  K <- nsli(conc)
  T <- length(time)

  if (!is.numeric(dim(conc))) {
    I <- J <- K <- 1
  } else {
    if (length(dim(conc)) ==2) {
      J <- K <- 1
    }
  }

  if (verbose) cat("  Deconstructing data...", fill=TRUE)
  conc.mat <- matrix(conc[img.mask], nvoxels)
  conc.mat[is.na(conc.mat)] <- 0
  conc.list <- list()
  for (i in 1:nvoxels)
    conc.list[[i]] <- conc.mat[i,]

  switch(aif,
    tofts.kermode = {
      D <- 0.1; a1 <- 3.99; a2 <- 4.78; m1 <- 0.144; m2 <- 0.0111
      input <- D*(a1*exp(-m1*time)+a2*exp(-m2*time))
    },
    fritz.hansen = {
      D <- 1; a1 <- 2.4; a2 <- 0.62; m1 <- 3.0; m2 <- 0.016
      input <- D*(a1*exp(-m1*time)+a2*exp(-m2*time))	 
    },
    observed = {
      input = aif.observed
    },
    print("WARNING: AIF parameters must be specified!"))

  model.func <- model.guess <- NULL

  if (model=="weinmann") {
    ktrans <- kep <- list(par=rep(NA, nvoxels), error=rep(NA, nvoxels))
    sigma2 <- rep(NA, nvoxels)
    model.func <- function(time, logktrans, logkep) {
      ktrans <- exp(logktrans)
      kep <- exp(logkep)
      erg <- ktrans*exp(-kep*(time))
      eval(erg)
    }
    model.guess <- list("logktrans"=-1, "logkep"=0)
  }

  if (model=="AATH") {
    E <- F <- TC <- ve <- list(par=rep(NA, nvoxels), error=rep(NA, nvoxels))
    sigma2 <- rep(NA, nvoxels)

    model.func <- list()
    model.func[[1]] <- function(time, F, E, TC, ve) {
      TC2 <- 2*TC
      if (TC < 0)
        TC2 <- 0
      kep <- E*F/ve
      erg <- E*exp(-kep*(time-TC))
      ## erg[time<TC] <- 1 - time[time<TC2]*(1-E) / TC
      erg[time<TC] <- 1 # - time[time<TC2]*(1-E) / TC
      erg <- erg*F
      if (TC < 0)
	erg <- rep(-10^16, length(time))
      eval(erg)
    }
    model.func[[2]] <- function(time, F, E, ve) {
      kep <- E*F/ve
      erg <- E*exp(-kep*(time))
      erg <- erg*F
      eval(erg)
    }

    model.guess <- list("E"=.6,"F"=2,"TC"=0,"ve"=.05)
  }

  ##define B and A
  p <- length(knots) - k
  B <- splineDesign(knots, time.input, k, outer.ok=TRUE)
  if (sum(B[, dim(B)[2]]==0) == dim(B)[1])
    B <- B[,-dim(B)[2]]
  if (sum(B[,1]==0) == dim(B)[1])
    B <- B[,-1]
  p <- dim(B)[2]
  A <- matrix(0, nrow=length(time), ncol=length(time.input))
  ni <- time
  for (i in 1:length(time)) {
    for (j in 1:length(time.input)) {
      if (time.input[j] <= time[i])
	ni[i] <- j
    }
  }
  for (i in 1:length(time)) {
    for (j in 1:ni[i])
      A[i,j] <- input[1+ni[i]-j]
  }
  A <- A*mean(diff(time.input))
  A[is.na(A)] <- 0
  D <- A%*%B
  T <- length(time)

  if (verbose) cat("  Estimating the parameters...", fill=TRUE)

  if (!multicore) {
    fit <- lapply(conc.list, FUN=dcemri.spline.single, time=time, D=D,
                  time.input=time.input, p=p, rw=rw, knots=knots, k=k,
                  A=A, nriters=nriters, thin=thin, burnin=burnin,
                  ab.hyper=ab.hyper, ab.tauepsilon=ab.tauepsilon,
                  t0.compute=t0.compute, nlr=nlr, multicore=multicore,
                  model=model, model.func=model.func,
                  model.guess=model.guess, samples=samples, B=B)
  }
  else {
    require("multicore")
    fit <- mclapply(conc.list, FUN=dcemri.spline.single, time=time, D=D,
                    time.input=time.input, p=p, rw=rw, knots=knots, k=k,
                    A=A, nriters=nriters, thin=thin, burnin=burnin,
                    ab.hyper=ab.hyper, ab.tauepsilon=ab.tauepsilon,
                    t0.compute=t0.compute, nlr=nlr, multicore=multicore,
                    model=model, model.func=model.func,
                    model.guess=model.guess, samples=samples, B=B)
  }

  if (verbose) cat("  Reconstructing results...", fill=TRUE)

  t0 <- NULL
  for (k in 1:nvoxels)
    t0 <- c(t0, fit[[k]]$t0)
    t0.img <- array(NA, c(I,J,K))
  t0.img[img.mask] <- t0
  t0 <- t0.img

  Fp <- NULL
  for (k in 1:nvoxels)Fp <- c(Fp, fit[[k]]$Fp)
    Fp.img <- array(NA, c(I,J,K))
  Fp.img[img.mask] <- Fp
  Fp.samples <- array(NA, c(nvoxels, nriters))
  for (i in 1:nvoxels)
    Fp.samples[i,] <- fit[[k]]$Fp.samples
  if (samples) {
    Fp <- array(NA, c(I,J,K,nriters))
    for (j in 1:nriters)
      Fp[,,,j][img.mask] <- Fp.samples[,j]
  }

  if (nlr) {
    ktrans <- ve <- NULL
    if (model=="weinmann") {
      kep <- NULL
      if (samples)
	ktrans.samples <- array(NA, c(nvoxels,nriters))
      for (k in 1:nvoxels) {
	ktrans <- c(ktrans, fit[[k]]$par$ktrans)
	if (samples)
	  ktrans.samples[k,] <- fit[[k]]$par$ktrans.samples
      }
      if (samples)
	kep.samples <- array(NA, c(nvoxels,nriters))
      for (k in 1:nvoxels) {
	kep <- c(kep, fit[[k]]$par$kep)
	if (samples)
	  kep.samples[k,] <- fit[[k]]$par$kep.samples
      }
      ve = ktrans/kep
      if (samples)
	ve.samples <- ktrans.samples/kep.samples
    }
    if (model=="AATH") {
      E <- F <- TC <- NULL
      if (samples)
	E.samples <- array(NA,c(nvoxels,nriters))
      for (k in 1:nvoxels) {
	E <- c(E, fit[[k]]$par$E)
	if (samples)
	  E.samples[k,] <- fit[[k]]$par$E.samples
      }
      if (samples)
	F.sample <- array(NA,c(nvoxels,nriters))
      for (k in 1:nvoxels) {
	F <- c(F, fit[[k]]$par$F)
	if (samples)
	  F.samples[k,] <- fit[[k]]$par$F.samples
      }
      if (samples)
	TC.samples <- array(NA,c(nvoxels,nriters))
      for (k in 1:nvoxels) {
	TC <- c(TC, fit[[k]]$par$TC)
	if (samples)
	  TC.samples[k,] <- fit[[k]]$par$TC.samples
      }
      if (samples)
	ve.samples <- array(NA,c(nvoxels,nriters))
      for (k in 1:nvoxels) {
	ve <- c(ve, fit[[k]]$par$ve)
	if (samples)
	  ve[k,] <- fit[[k]]$par$ve.samples
      }
      ktrans <- E*F
      if (samples)
	ktrans.samples <- E.samples/F.samples
    }
  }


  beta.sample <- array(NA,c(nvoxels,p,nriters))
  for(k in 1:nvoxels) {
    beta.sample[k,,] <- fit[[k]]$beta
  }

  T2 <- length(time.input)
  response.sample <- array(NA,c(nvoxels,T2,nriters))
  for(k in 1:nvoxels) {
    for(j in 1:nriters) {
      response.sample[k,,j] <- fit[[k]]$fitted[[j]]
    }
  }

  fitted.sample <- array(NA,c(nvoxels,T,nriters))
  for (i in 1:nvoxels)
    for (j in 1:nriters)
      fitted.sample[i,,j] <- D%*%beta.sample[i,,j]

  beta <- array(NA, c(I,J,K,p,nriters))
  beta.med <- array(NA, c(I,J,K,p))
  fitted <- array(NA, c(I,J,K,T,nriters))
  fitted.med <- array(NA, c(I,J,K,T))
  response <- array(NA, c(I,J,K,T,nriters))
  response.med <- array(NA, c(I,J,K,T))

  if (nlr) {
    ktrans.med <- ve.med <- array(NA, c(I,J,K))
    ktrans.med[img.mask] <- ktrans
    ve.med[img.mask] <- ve
    if (model=="weinmann") {
      kep.med <- array(NA,c(I,J,K))
      kep.med[img.mask] <- kep
    }
    if (model=="AATH") {
      E.med <- F.med <- TC.med <- array(NA, c(I,J,K))
      E.med[img.mask] <- E
      F.med[img.mask] <- F
      TC.med[img.mask] <- TC
    } 
    if (samples) {
      ktrans <- ve <- array(NA, c(I,J,K,nriters))
      for (i in 1:nriters) {
	ktrans[,,,i][img.mask] <- ktrans.samples[,i]
	ve[,,,i][img.mask] <- ve.samples[,i]
      }
      if (model=="weinmann") {
	kep <- array(NA,c(I,J,K,nriters))
	for (i in 1:nriters) {
	  kep[,,,i][img.mask] <- kep.samples[,i]
	}
      }
      if (model=="AATH") {
	E <- F <- TC <- array(NA, c(I,J,K,nriters))
	for (i in 1:nriters) {
	  E[,,,i][img.mask] <- E.samples[,i]
	  F[,,,i][img.mask] <- F.samples[,i]
	  TC[,,,i][img.mask] <- TC.samples[,i]
	}
      }
    }
  }


  if (I > 1) {
    for (j in 1:p) {
      beta.med[,,,j][img.mask] <- apply(beta.sample[,j,], 1, median)
      for (i in 1:nriters)
        beta[,,,j,i][img.mask] <- beta.sample[,j,i]
    }
  } else {
    for (j in 1:p) {
      beta.med[,,,j][img.mask] <- median(beta.sample[,j,])
      for (i in 1:nriters)
        beta[,,,j,i][img.mask] <- beta.sample[,j,i]
    }
  }
  
  for (j in 1:T) {
    if (I > 1)
      response.med[,,,j][img.mask] <- apply(response.sample[,j,], 1, median)
    if (I == 1)
      response.med[,,,j][img.mask] <- median(response.sample[,j,])
    for (i in 1:nriters)
      response[,,,j,i][img.mask] <- response.sample[,j,i]
    if (I > 1)
      fitted.med[,,,j][img.mask] <- apply(fitted.sample[,j,], 1, median)
    if (I == 1)
      fitted.med[,,,j][img.mask] <- median(fitted.sample[,j,])
    for (i in 1:nriters)
      fitted[,,,j,i][img.mask] <- fitted.sample[,j,i]
  }

  return.list <- list("beta"=beta.med, "beta.sample"=beta,
                      "beta.test"=beta.sample, "fit"=fitted.med,
                      "fit.sample"=fitted, "response"=response.med,
                      "response.sample"=response, "Fp"=Fp.img, "A"=A,
                      "B"=B, "D"=D, "t0"=t0, "ourfit"=fit) 
  if (nlr) {
    switch (model,
            weinmann = { return.list[["kep"]] <- kep.med }, 
            AATH = { return.list <-
                       c(return.list, list("E"=E.med, "F"=F.med,
                                           "TC"=TC.med)) })
    return.list[["ktrans"]] <- ktrans.med
    return.list[["ve"]] <- ve.med
    if (samples) {
      return.list[["ktrans.sample"]] <- ktrans
      return.list[["ve.sample"]] <- ve
      switch (model,
              weinmann = { return.list[["kep.samples"]] <- kep },
              AATH = { return.list <-
                         c(return.list, list("E.samples"=E, "F.samples"=F,
                                             "TC.samples"=TC)) })
    }
  } 
  if (samples)
    return.list[["Fp.samples"]] <- Fp

  return(return.list)
}
