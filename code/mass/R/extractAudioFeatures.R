extractAudioFeatures <-
  function(wav.dir = getwd(),
           wav.fnames = NULL,
           windowSize = 25,
           windowShift = 12.5,
           windowType = 'HAMMING',
           derivatives = 2,
           verbose = 1,
           recursive = FALSE,
           formants = TRUE,
           pitch = TRUE,
           energy = TRUE,
           zcr = TRUE,
           ac = TRUE,
           mfcc = TRUE
           ){

    ## Function takes a directory of wav files and prepares them for
    ## hmm
    ## window length is in ms

    ## Move this to NAMESPACE and DESCRIPTION soon, for now load here
    ## (obviously bad practice)
    library(wrassp)
    library(tuneR)
    library(signal)

    if (!windowType %in% AsspWindowTypes()){
      stop('supported window types are ', paste(AsspWindowTypes(), collapse=', '))
    }

    if (windowType != 'HAMMING'){
      warning('only window type "HAMMING" supported for MFCC and TEO calculations; ignoring "windowType" for those features')
    }

    ## general parameters for feature extraction
    par <- list(windowSize = windowSize,
                windowShift = windowShift,
                window = windowType,
                toFile = FALSE
                )

    ## feature-specific parameters for feature extraction
    preemphasis = .97 # pre-emphasis filter
    numFormants = 3   # for formants/bandwidths
    gender = 'u'      # for setting min/max on F0; we may need to eliminate this to stop F0 from zeroing out
    order = 1         # for autocorrelation
    numcep = 13       # number of cepstra for MFCC

    ## files
    wav.dir <- gsub('/?$', '/', wav.dir) # add trailing '/' if missing

    if (is.null(wav.fnames)){
      if (recursive){
        wav.fnames <- list.files(wav.dir, recursive = TRUE, full.names = TRUE)
        wav.fnames <- wav.fnames[grepl(".wav", wav.fnames)]
      } else {
        wav.fnames <- Sys.glob(paste(wav.dir,'*.wav', sep = ''))
      }
    }
    out.names <- sub("(.*\\/)([^.]+)(\\.[[:alnum:]]+$)", "\\2", wav.fnames)
    out.list <- vector('list', length(out.names))
    names(out.list) <- out.names

    if (length(out.names) == 0){
      stop('no .wav files found in ', wav.dir)
    }

    description <- data.frame(row.names = out.names)
    description$filename <- NA
    description$duration <- NA
    description$sampling.rate <- NA
    description$nsamples <- NA

    ## go go gadget
    for (i in 1:length(out.list)){

      if (verbose>=1){
        cat('extracting features from', out.names[i], '\n')
      }

      fname <- wav.fnames[i]
      par$listOfFiles <- fname
      out <- list()

      ## origin file properties
      au <- read.AsspDataObj(fname)
      description$filename[i] <- fname
      description$duration[i] <- dur.AsspDataObj(au)
      description$sampling.rate[i] <- rate.AsspDataObj(au)
      description$nsamples[i] <- numRecs.AsspDataObj(au)

      ## formants
      if (formants){
        if (verbose >= 2){
          cat('  extracting formants\n')
        }
        fmBwVals <- do.call(forest, c(par, numFormants = numFormants, preemphasis=preemphasis, estimate=TRUE))
        out$formants <- fmBwVals$fm
        colnames(out$formants) <- paste('formant', 1:numFormants, sep='')

        ## formant bandwidths
        if (verbose >= 2){
          cat('  extracting formant bandwidths\n')
        }
        out$bandwidths <- fmBwVals$bw
        colnames(out$bandwidths) <- paste(colnames(out$formants), '_bw', sep='')
        rm(fmBwVals); gc()

      }

      ## fundamental frequency and pitch
      if (pitch){
        if (verbose >= 2){
          cat('  extracting frequency and pitch\n')
        }
        out$f0_ksv <- do.call(ksvF0,
                              args=c(par[-match(c('windowSize', 'window'),
                                                names(par))], # not used by ksvF0
                                     gender=gender) # ksvF0-specific args
                              )$F0

        out$f0_mhs <- do.call(mhsF0,
                              args=c(par[-match(c('windowSize', 'window'), names(par))], # not used by mhsF0
                                     gender=gender) # mhsF0-specific args
                              )$pitch

      }

      ## energy
      if (energy){
        if (verbose >= 2){
          cat('  extracting energy\n')
        }
        out$energy_dB <- do.call(rmsana, par)$rms
      }

      ## zero crossing rate
      if (zcr){
        if (verbose >= 2){
          cat('  extracting zero-crossing rate\n')
        }
        out$zcr <- do.call(zcrana, par[-match(c('window'), names(par))])$zcr
      }

      ## 1st order autocorrelation (higher orders are almost identical)
      if (ac){
        if (verbose >= 2){
          cat('  extracting autocorrelation\n')
        }
        out$autocorrelation <- do.call(acfana, c(par, analysisOrder=order))$acf
        out$autocorrelation <- out$autocorrelation[, 1:order, drop=FALSE] # keep as matrix if order==1
      }

      ## Teager energy operator, aggregated to frame level
      ## (first need frame ids for each window to align)
      if (energy){
        if (verbose >= 2){
          cat('  extracting Teager energy operator\n')
        }
        windowSizeSamples <- floor(windowSize / 1000 * description$sampling.rate[i])
        windowShiftSamples <- floor(windowShift / 1000 * description$sampling.rate[i])
        windowCount <- nrow(out[[1]])
        windowStarts <- (0:(windowCount-1)) * windowShiftSamples + 1  # first of windowShiftSamples in window

        teo_continuous <- c(0, # deriv can't be calculated for first frame...
                            au$audio[2:(description$nsamples[i]-1)]^2 -
                              au$audio[1:(description$nsamples[i]-2)]*au$audio[3:description$nsamples[i]],
                            0) # ... or last frame

        ## out$teo = sapply(windowStarts, function(windowStart){
        ##   weighted.mean(teo_continuous[windowStart + 0:(windowSizeSamples-1)]^2,
        ##                 w = hamming(windowSizeSamples))
        ## })
        out$teo = sapply(windowStarts, function(windowStart){
          mean(teo_continuous[windowStart + 0:(windowSizeSamples-1)]^2)
        })
        out$teo = as.matrix(log(out$teo, 10))
        rm(teo_continuous); gc()
      }

      if (mfcc){
        ## Mel-frequency cepstral coefficients
        if (verbose >= 2){
          cat('  extracting Mel-frequency cepstral coefficients\n')
        }
        bitdepth <- NA
        if (!is.null(attr(au, 'trackFormats'))){
          bitdepth <- as.numeric(gsub('^INT(\\d+)$', '\\1',
                                      attr(au, 'trackFormats')))
        }
        if (!is.na(bitdepth)){
          au.wave <- Wave(au$audio[,1],
                          samp.rate=description$sampling.rate[i],
                          bit = bitdepth)
        } else { # Wave will assume default bit depth == 16, with warning
          au.wave <- Wave(au$audio[,1],
                          samp.rate=description$sampling.rate[i])
        }
        ## melfcc trims first/last frames; put them back in for now
        if (windowCount > 3){
          out$mfcc <-
            rbind(
              rep(NA, numcep),
              melfcc(au.wave,
                     sr = description$sampling.rate[i],
                     numcep = numcep,
                     preemph = -preemphasis,
                     wintime = par$windowSize/1000,
                     hoptime = par$windowShift/1000),
              rep(NA, numcep)
            )
        } else {
          warning(sprintf('  audio file %s.wav too short: cannot extract Mel-frequency cepstral coefficients, substituting NAs', out.names[i]))
          out$mfcc <- matrix(NA, nrow = windowCount, ncol = numcep)
        }
      }


      ## discrete fourier transform (probably not needed)
      ## out$DFT_spectrum <- do.call(dftSpectrum, par[-match(c('windowSize'), names(par))])

      ## discard partial frames from wrassp
      nframes <- sapply(out, nrow)
      windowCount <- min(sapply(out, nrow))
      for (feature in 1:length(out)){

        if (nrow(out[[feature]]) > windowCount){
          out[[feature]] <- out[[feature]][1:windowCount, , drop=FALSE]
        }

        if (is.null(colnames(out[[feature]]))){
          if (ncol(out[[feature]]) == 1){
            colnames(out[[feature]]) <- names(out)[feature]
          } else {
            colnames(out[[feature]]) <- paste(names(out)[feature], 1:ncol(out[[feature]]), sep='')
          }
        }

      }

      ## interactions
      if ((energy && zcr) || (energy && pitc)){
        if (verbose >= 2){
          cat('  calculating interactions and derivatives\n')
        }
        out[['energyXzcr']] <- out$energy_dB * out$zcr
        colnames(out[['energyXzcr']]) <- 'energyXzcr'
        out[['teoXf0']] <- out$teo * out$f0_ksv
        colnames(out[['teoXf0']]) <- 'teoXf0'

      }

      ## first/second derivatives
      out <- do.call(cbind, out)

      if (!derivatives %in% 0:2){
        warning('"derivatives" must be in 0:2, ignoring')
        derivatives = 0
      }

      if (derivatives >= 1){
        d1 <- out
        d1[1,] <- NA
        if (nrow(d1) > 1){
          d1[2:nrow(d1),] <- out[2:nrow(d1),] - out[1:(nrow(d1)-1),]
        }
        colnames(d1) <- paste(colnames(out), '_d1', sep='')
      }

      if (derivatives == 2){
        d2 <- d1
        if (nrow(d2) > 1){
          d2[2,] <- NA
        }
        if (nrow(d2) > 2){
          d2[3:nrow(d2),] <- d1[3:nrow(d2),] - d1[2:(nrow(d1)-1),]
        }
        colnames(d2) <- paste(colnames(out), '_d2', sep='')
      }

      out <- cbind(out, if (derivatives >= 1) d1, if (derivatives == 2) d2)

      ## NA out first/last frames for which mfcc can't be calculated
      out[c(1, nrow(out)),] <- NA
      ## NA out initial frames for which derivatives can't be calculated
      if (derivatives > 0){
        out[1:(derivatives + 1),] <- NA  # extra frame due to missing mfccs
      }

      attr(out, 'derivatives') = derivatives
      attr(out, 'timestamps') = 0:(windowCount-1) * windowShift
      out.list[[i]] <- out

      rm(au, fname, windowSizeSamples, windowShiftSamples, windowCount, windowStarts, out)
      if (derivatives >= 1)
        rm(d1)
      if (derivatives == 2)
        rm(d2)

    }

    out.combined <-
      list(data = out.list,
           files = description,
           control = list(windowSize = windowSize,
                          windowShift = windowShift,
                          windowType = windowType,
                          derivatives = derivatives))
    class(out.combined) <- "preppedAudio"
    return(out.combined)
  }

print.preppedAudio <-
  function(x, ...){
    msg <- paste(as.character(nrow(x$files)),
                 ' prepped audio files with ',
                 dim(x$data[[1]])[2],
                 ' features.\n',
                 sep = '')
    cat(msg)
  }

segment <- function(prepped.audio, feature = 'zcr', threshold = .00001){
  out.list <- vector('list', nrow(prepped.audio$files))
  names(out.list) <- rownames(prepped.audio$files)
  for(i in 1:nrow(prepped.audio$files)){
    origin.features <- prepped.audio$data[[i]]
    feature.vals <- origin.features[,colnames(origin.features) == feature]
    not.silence <- segmentVector(feature.vals, threshold)
    return(not.silence)
    counter <- 1
    for(r in 1:nrow(not.silence)){
      inds <- unlist(not.silence[r,])
      out.list[[i]][[counter]] <- origin.features[inds[1]:inds[2],]
      counter <- counter + 1
    }
  }
  return(out.list)
}

segmentNAs <- function(prepped.audio, feature = 'zcr', threshold = .00001){
  out.list <- vector('list', nrow(prepped.audio$files))
  names(out.list) <- rownames(prepped.audio$files)
  for(i in 1:nrow(prepped.audio$files)){
    origin.features <- prepped.audio$data[[i]]
    feature.vals <- origin.features[,colnames(origin.features) == feature]
    silent.inds <- isSilent(feature.vals, threshold)
    counter <- 1
    print(silent.inds)
    if(nrow(silent.inds) > 0){ # if there are silent regions
      for(r in 1:nrow(silent.inds)){
        inds <- unlist(silent.inds[r,])
        origin.features[inds[1]:inds[2],] <- NA
      }
    }
    out.list[[i]] <- origin.features
  }
  return(out.list)
}

cppFunction('DataFrame notSilence(NumericVector x, double threshold) {
  x.push_back(-1);
  int n = x.size(), startind, endind;
  std::vector<int> startinds, endinds;
  bool insegment = false;
  for(int i=0; i<n; i++){
    if(!insegment){
      if(x[i] > threshold){
        startind = i + 1;
        insegment = true;}
    }else{
      if(x[i] < threshold){
        endind = i;
        insegment = false;
        startinds.push_back(startind);
        endinds.push_back(endind);
      }
    }
  }
  return DataFrame::create(_["start"]= startinds, _["end"]= endinds);
}')

cppFunction('DataFrame isSilent(NumericVector x, double threshold) {
  x.push_back(-1);
  int n = x.size(), startind, endind;
  std::vector<int> startinds, endinds;
  bool insegment = false;
  for(int i=0; i<n; i++){
    if(!insegment){
      if(x[i] <= threshold){
        startind = i + 1;
        insegment = true;}
    }else{
      if(x[i] >= threshold){
        endind = i;
        insegment = false;
        startinds.push_back(startind);
        endinds.push_back(endind);
      }
    }
  }
  return DataFrame::create(_["start"]= startinds, _["end"]= endinds);
}')
