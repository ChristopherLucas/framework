library(devtools)
install_version('quanteda',
                version = '2.1.2',
                repos = 'http://cran.us.r-project.org'
                )

library(data.table)
library(plyr)
library(ggplot2)
library(quanteda)
library(jsonlite)

options(stringsAsFactors = FALSE)



###########################
## functions & constants ##
###########################

`%.%` <- paste0

## raw and cut (single-speaker) wav files
wavs.dir <- '../data/wavs'

## intermediate files
intermediate.dir <- '../intermediate_files'

## professional transcription
ucsb.dir <- '../data/transcripts_ucsb'

## google speech
key <- '[YOUR API KEY HERE]'
gsbucket <- '[YOUR GS BUCKET WITH WAVS HERE]'
googlespeech.dir <- file.path(intermediate.dir, 'transcripts_googlespeech')



####################################
## prepare speaker-specific hints ##
####################################

## read and clean romney transcripts
romney.paths <- list.files(ucsb.dir, pattern = 'romney', full.names = TRUE)
romney.transcripts <- sapply(romney.paths, function(x) readLines(x)[-(1:4)])
romney.transcripts <- char_tolower(romney.transcripts)
romney.transcripts <- gsub('.', ' ', romney.transcripts, fixed = TRUE)
romney.tokens <- tokens(romney.transcripts,
                        remove_punct = TRUE,
                        remove_hyphens = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE
                        )
romney.ngrams <- tokens_ngrams(romney.tokens, n = 1:6)

## romney ngrams appearing in >= 10% unique docs
romney.dtm <- dfm(romney.ngrams, tolower = FALSE)
romney.dtm <- romney.dtm[,colMeans(romney.dtm > 0) > .1]

## prioritize longer and more frequent ngrams first
romney.ngrams <- colnames(romney.dtm)
romney.ngrams.words <- strsplit(romney.ngrams, '_')
romney.ngrams.length <- sapply(romney.ngrams.words, length)
romney.ngrams[romney.ngrams.length >= 4]
ind <- order(romney.ngrams.length, colSums(romney.dtm), decreasing = TRUE)
romney.ngrams <- romney.ngrams[ind]
romney.ngrams <- gsub('_', ' ', romney.ngrams)
romney.ngrams.words <- romney.ngrams.words[ind]

## construct hint phrases
romney.hints <- c()
romney.hintwords <- stopwords()
for (i in seq_along(romney.ngrams)){

  if (i %% 100 == 0)
    print(i)

  ## check if most words in this ngram have been covered already
  ##   (either by previous hints or by stopwords)
  covered.prop <- mean(romney.ngrams.words[[i]] %chin% romney.hintwords)
  if (covered.prop >= .5){
    next
  }

  ## if not, add phrase to hint list
  romney.hints <- c(romney.hints, romney.ngrams[i])
  romney.hintwords <- c(romney.hintwords, romney.ngrams.words[[i]])

  ## limit of 500 hints
  if (length(romney.hints) == 500){
    break
  }
}

## inspect hints
cat(strwrap(paste(romney.hints, collapse = ', '), width = 80), sep = '\n')
length(romney.hints)      # max 500
sum(nchar(romney.hints))  # max 10k



## read and clean obama transcripts
obama.paths <- list.files(ucsb.dir, pattern = 'obama', full.names = TRUE)
obama.transcripts <- sapply(obama.paths, function(x) readLines(x)[-(1:4)])
obama.transcripts <- char_tolower(obama.transcripts)
obama.transcripts <- gsub('.', ' ', obama.transcripts, fixed = TRUE)
obama.tokens <- tokens(obama.transcripts,
                        remove_punct = TRUE,
                        remove_hyphens = TRUE,
                        remove_symbols = TRUE,
                        remove_numbers = TRUE
                        )
obama.ngrams <- tokens_ngrams(obama.tokens, n = 1:6)

## obama ngrams appearing in >= 10% unique docs
obama.dtm <- dfm(obama.ngrams, tolower = FALSE)
obama.dtm <- obama.dtm[,colMeans(obama.dtm > 0) > .1]

## prioritize longer and more frequent ngrams first
obama.ngrams <- colnames(obama.dtm)
obama.ngrams.words <- strsplit(obama.ngrams, '_')
obama.ngrams.length <- sapply(obama.ngrams.words, length)
obama.ngrams[obama.ngrams.length >= 4]
ind <- order(obama.ngrams.length, colSums(obama.dtm), decreasing = TRUE)
obama.ngrams <- obama.ngrams[ind]
obama.ngrams <- gsub('_', ' ', obama.ngrams)
obama.ngrams.words <- obama.ngrams.words[ind]

## construct hint phrases
obama.hints <- c()
obama.hintwords <- stopwords()
for (i in seq_along(obama.ngrams)){

  if (i %% 100 == 0)
    print(i)

  ## check if most words in this ngram have been covered already
  ##   (either by previous hints or by stopwords)
  covered.prop <- mean(obama.ngrams.words[[i]] %chin% obama.hintwords)
  if (covered.prop >= .5){
    next
  }

  ## if not, add phrase to hint list
  obama.hints <- c(obama.hints, obama.ngrams[i])
  obama.hintwords <- c(obama.hintwords, obama.ngrams.words[[i]])

  ## limit of 500 hints
  if (length(obama.hints) == 500){
    break
  }
}

## inspect hints
cat(strwrap(paste(obama.hints, collapse = ', '), width = 80), sep = '\n')
length(obama.hints)      # max 500
sum(nchar(obama.hints))  # max 10k



###############################################
## send audio and hints to google speech api ##
###############################################

meta <- read.csv('../data/speech_meta.csv')
wav.paths <- list.files(wavs.dir, full.names = TRUE)
wav.fnames <- basename(wav.paths)
for (wav.fname in wav.fnames){

  cat(wav.fname, '\n')

  id <- gsub('_cut.wav', '', wav.fname)
  ind <- match(id, meta$vid.id) #  "ID_1" "ID_2" "ID_3" "ID_4" "ID_5" "ID_6"
  if (is.na(ind)){
    stop(id, ' not found')
  }
  wav.path <- gsbucket %.% wav.fname
  config.path <- file.path(googlespeech.dir, sprintf('%s_config.json', id))
  out.path <- file.path(googlespeech.dir, id %.% '.json')

  if (file.exists(out.path)){
    next
  }

  ## identify speaker in this video and look up speaker-specific hints
  if (grepl('\\bromney\\b', meta$vid.title[ind], ignore.case = TRUE)){
    hints <- romney.hints
  } else if (grepl('\\bobama\\b', meta$vid.title[ind], ignore.case = TRUE)){
    hints <- obama.hints
  }
  ## hints <- gsub("'", "\\\\'", hints)

  ## build up configuration with hints
  data <- sprintf(
    '{
      "config": {
        "languageCode": "en-US",
        "maxAlternatives": 30,
        "speechContexts": [{
          "phrases": [%s]
         }],
        "enableWordTimeOffsets": true,
        "model": "video",
        "useEnhanced": true
      },
      "audio": {
        "uri": "%s"
      }
    }',
    paste('"' %.% hints %.% '"', collapse = ', '),
    wav.path
  )
  writeLines(data, config.path)

  ## send to google speech api
  ## -H "Authorization: Bearer "$(gcloud auth application-default print-access-token) \
  command <- sprintf(
    "curl -X POST \\
      -H 'Content-Type: application/json; charset=utf-8' \\
      --data '@%s' \\
      'https://speech.googleapis.com/v1/speech:longrunningrecognize?key=%s'",
    config.path,
    key
  )

  response <- system(command, intern = TRUE)
  writeLines(response, out.path)
}



##################
## pull results ##
##################

out.paths <- list.files(googlespeech.dir, full.names = TRUE)
out.paths <- out.paths[-grep('config', out.paths)]
for (out.path in out.paths){
  result <- readLines(out.path)
  result <- fromJSON(result)
  if (is.null(result$done)){
    cat('pulling progress: ', '\n')
    result <- system(
      sprintf('gcloud ml speech operations describe %s', result$name),
      intern = TRUE
    )
    writeLines(result, out.path)
    result <- fromJSON(result)
  }
  cat(basename(out.path), ':', result$metadata$progressPercent, '%', '\n')
}
