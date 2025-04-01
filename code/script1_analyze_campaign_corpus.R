library(data.table)
library(dplyr)
library(jsonlite)
library(Rcpp)
library(lmtest)
library(ggplot2)
library(quanteda)
library(sandwich)
library(foreach)

options(stringsAsFactors = FALSE)

lightgray <- 'gray80'
medgray <- 'gray50'
darkgray <- 'gray20'
red <- '#A31F34'
blue <- '#4E84C4'



###############
## citations ##
###############

## Albugh, Quinn, Julie Sevenans and Stuart Soroka. 2013. Lexicoder Topic
## Dictionaries, June 2013 versions, McGill University, Montreal,
## Canada. (available at lexicoder.com)

## Benjamini & Yekutieli (2001)



###############
## constants ##
###############

wav.dir <- '../data/wavs'
intermediate.dir <- '../intermediate_files'
json.transcript.dir <- '../intermediate_files/transcripts_googlespeech'
output.dir <- '../figures'



###############
## functions ##
###############

`%.%` <- paste0



##############
## metadata ##
##############

meta <- fread('../data/speech_meta.csv')

# Original data source listed same recording twice, drop the repeated recording
meta <- meta[meta$vid.id != 'ID_11',]

transcript.paths <- file.path(json.transcript.dir, meta$vid.id %.% '.json')
wav.paths <- file.path(wav.dir, meta$vid.id %.% '_cut.wav')



######################
## read transcripts ##
######################

transcripts.path <- '../intermediate_files/transcripts.csv'

## compile transcripts
if (!file.exists(transcripts.path)){
    transcript.dfs <- list()
    for(i in 1:length(transcript.paths)){
        transcript.path = transcript.paths[i]
        result <- readLines(transcript.path)
        result <- fromJSON(result, simplifyVector = FALSE)
        transcript.utterances <- lapply(
            result$response$results,
            function(x){
                utterance <- x[[1]][[1]]$words
                data.frame(
                    speaker = rep(meta$speaker[i], length(utterance)),
                    date = rep(meta$date[i], length(utterance)),
                    speech = rep(meta$vid.title[i], length(utterance)),
                    id = rep(meta$id[i], length(utterance)),
                    wav = rep(file.path(wav.dir, meta$vid.id[i] %.% '_cut.wav'),
                              length(utterance)),
                    start.sec = sapply(utterance, `[[`, 'startTime'),
                    end.sec = sapply(utterance, `[[`, 'endTime'),
                    word = sapply(utterance, `[[`, 'word')
                )}
        )
        transcript.utterances <- transcript.utterances[lapply(transcript.utterances,nrow)>0]
        for (j in seq_along(transcript.utterances)){
            transcript.utterances[[j]]$utterance <- j
        }
        transcript.df <- rbindlist(transcript.utterances)
        transcript.df[, start.sec := as.numeric(gsub('s', '', start.sec))]
        transcript.df[, end.sec := as.numeric(gsub('s', '', end.sec))]
        transcript.dfs[[i]] <- transcript.df
    }
    transcripts <- rbindlist(transcript.dfs)
    setkey(transcripts, id, utterance, start.sec)
    fwrite(transcripts, transcripts.path)

} else {

  transcripts <- fread(transcripts.path)
  setkey(transcripts, id, utterance, start.sec)

}



##########################################
## extract audio features with old code ##
##########################################

audio.path <- file.path(intermediate.dir, 'audio_features.rds')
if (!file.exists(audio.path)){
  source('mass/R/extractAudioFeatures.R')
  audio <- extractAudioFeatures(wav.dir,
                                derivatives = 0,
                                formants = TRUE,
                                pitch = TRUE,
                                energy = TRUE,
                                zcr = TRUE,
                                ac = TRUE,
                                mfcc = FALSE
                                )
  saveRDS(audio, audio.path, compress = TRUE)
} else {
  audio <- readRDS(audio.path)
}

## relabel audio features with informative ids
names(audio$data) <- meta[
  data.table(vid.id = gsub('_cut', '', names(audio$data))),
  id,
  on = 'vid.id'
]



###############################################
## extract utterance-specific audio features ##
###############################################

pitch.floor <- 50     # min allowed pitch
pitch.ceiling <- 250  # max allowed pitch
jitter.max <- 20      # max allowed pitch diff between consecutive moments
energy.floor <- 50    # min allowed energy (else silence)

utterances.path <- '../intermediate_files/utterances_summary.csv'

if (!file.exists(utterances.path)){

  utterances <- foreach(v = meta$id, .combine = rbind) %do%
    {

      ## select conversation text
      transcript.v <- transcripts[v, on = 'id']

      ## loop over utterances in convo
      foreach(u = unique(transcript.v$utterance), .combine = rbind) %do%
        {

          ## select and process conversation audio features
          audio.v <- data.table(sec = attr(audio$data[[v]], 'timestamp') / 1000,
                                audio$data[[v]][, c('energy_dB', 'f0_mhs')]
                                )
          audio.v[, f0_mhs.d1 := c(0, diff(f0_mhs))]  # add 1st deriv of pitch
          ## for moments not considered to be speech, drop features
          audio.v[, keep := TRUE]
          audio.v[f0_mhs < pitch.floor, keep := FALSE]
          audio.v[f0_mhs > pitch.ceiling, keep := FALSE]
          audio.v[abs(f0_mhs.d1) > jitter.max, keep := FALSE]
          audio.v[energy_dB < energy.floor, keep := FALSE]
          audio.v <- na.omit(audio.v[keep == TRUE,])
          audio.v[, keep := NULL]

          ## select utterance text
          transcript.vu <- transcript.v[utterance == u,]

          ## subset to audio features that are between word endpoints
          audio.vu <- audio.v[inrange(sec,
                                      transcript.vu$start.sec,
                                      transcript.vu$end.sec
                                      ),
                              ]

          nwords <- nrow(transcript.vu)
          duration <-
            tail(transcript.vu$end.sec[1], 1) - head(transcript.vu$start.sec, 1)

          data.table(
            id = v,
            utterance = u,
            duration,
            nwords,
            text = paste(transcript.vu$word, collapse = ' '),
            energy.max = max(audio.vu$energy_dB),
            energy.mean = mean(audio.vu$energy_dB),
            energy.sd = sd(audio.vu$energy_dB),
            pitch.mean = mean(audio.vu$f0_mhs),
            pitch.sd = sd(audio.vu$f0_mhs),
            pitch.d1.mean = mean(audio.vu$f0_mhs.d1)
          )

        }

    }

  fwrite(utterances, utterances.path)

} else {

  utterances <- fread(utterances.path)

}

utterances[, speaker := gsub('_\\d+_speech\\d+', '', id)]
utterances <- na.omit(utterances)

features <- c('energy.mean',
              'energy.sd',
              'pitch.mean',
              'pitch.sd',
              'pitch.d1.mean'
              )
features.labels <- c(energy.mean = 'Loudness (average)',
                     energy.sd = 'Loudness (modulation)',
                     pitch.mean = 'Pitch (average)',
                     pitch.sd = 'Pitch (modulation)',
                     pitch.d1.mean = 'Pitch change (average)'
                     )

features.labels.units <- c(
  energy.mean = 'Loudness,\naverage\n(dB)',
  energy.sd = 'Loudness,\nmodulation\n(dB)',
  pitch.mean = 'Pitch,\naverage\n(Hz)',
  pitch.sd = 'Pitch,\nmodulation\n(Hz)',
  pitch.d1.mean = 'Pitch,\nchange\n(Hz / frame)'
)



########################################
## analyze speech patterns by speaker ##
########################################

mod <- lm(
  as.matrix(utterances[, features, with = FALSE]) ~ 0 + speaker,
  utterances
)
results <- as.matrix(coeftest(mod))
colnames(results) <- c('est', 'se', 't', 'p')
results <- data.table(coef = rownames(results), results[1:nrow(results),])
results[, outcome := gsub('(.*):(.*)', '\\1', coef)]
results[, speaker := gsub('(.*):speaker(.*)', '\\2', coef)]

## label for plotting
results[, outcome.label := factor(outcome,
                                  levels = features,
                                  labels = features.labels.units,
                                  ordered = TRUE
                                  )
        ]

obama_vs_romney <- ggplot(results,
       aes(x = speaker,
           y = est,
           ymin = est + se * qnorm(.025),
           ymax = est + se * qnorm(.975),
           color = speaker
           )
       ) +
  geom_point(size = 1.5) +
  geom_errorbar(width = .1, size = .75) +
  scale_color_manual(values = c(Romney = red,
                                Obama = blue
                                ),
                     guide = FALSE
                     ) +
  facet_wrap('outcome.label', nrow = 1, scales = 'free') +
  theme_light(element_text(family="Helvetica", size=12), base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Vocal style\n') +
  xlab('')
ggsave(filename = file.path(output.dir,'obama_vs_romney.png'),
       obama_vs_romney,
       width = 8,
       height = 6
       )



################################
## categorize utterance topic ##
################################

dict <- dictionary(file = '../data/policy_agendas_english.lcd')

utterances.dtm <- dfm(utterances$text,
                      dictionary = dict,
                      stem = TRUE
                      )
utterances.dtm <- utterances.dtm[, colSums(utterances.dtm) >= 50]
topics <- gsub('-', '_', colnames(utterances.dtm))
colnames(utterances.dtm) <- topics

topics.labels <- c(
  macroeconom = 'Economy',
  civil_right = 'Civil rights',
  healthcar = 'Healthcare',
  labour = 'Labor',
  educ = 'Education',
  energi = 'Energy',
  transport = 'Transportation',
  crime = 'Crime',
  social_welfar = 'Social welfare',
  financ = 'Finance',
  defenc = 'Defense',
  sstc = 'Technology',
  foreign_trad = 'Trade',
  land_water_manag = 'Environment',
  cultur = 'Culture',
  religion = 'Religion'
)

## merge binary topic indicator with other utterance statistics
utterances <- cbind(utterances, as.matrix(utterances.dtm > 0) + 0)

## counts by topic
topic.counts <- lapply(
  topics,
  function(topic){
    out <- utterances[get(topic) == 1,
                      .(utterances = .N,
                        speeches = uniqueN(id)
                        ),
                      by = 'speaker'
                      ]
    out <- cbind(topic = topic, out)
    out
  })
topic.counts <- rbindlist(topic.counts)



################################################
## analyze speech patterns by speaker & topic ##
################################################

## overall speaker summaries
speaker.summary <- utterances[,
                              c(lapply(.SD, mean, na.rm = TRUE)),
                              by = 'speaker',
                              .SDcols = features
                              ]

## standardize
for (feature in features){
  utterances[,
             (feature %.% '.std') := scale(get(feature)),
             by = 'speaker'
             ]
}

## coefs on topic dummies

mod.obama <- lm(
  as.matrix(
    utterances[speaker == 'Obama',
               features,
               with = FALSE
               ]
  ) ~
    0 +
    id +
    macroeconom +
    civil_right +
    healthcar +
    labour +
    educ +
    energi +
    transport +
    crime +
    social_welfar +
    financ +
    defenc +
    sstc +
    #foreign_trad +
    land_water_manag +
    cultur +
    religion,
  utterances[speaker == 'Obama',]
)
vcov.obama <- vcovCL(mod.obama, cl = utterances[speaker == 'Obama', id])
results.obama <- coeftest(mod.obama, vcov.obama)
results.obama <- as.matrix(
  results.obama[!rownames(results.obama) %like% '(^|:)id',]
  )
colnames(results.obama) <- c('est', 'se', 't', 'p')
results.obama <- data.table(coef = rownames(results.obama), results.obama)
results.obama[, outcome := gsub('(.*):(.*)', '\\1', coef)]
results.obama[, coef := gsub('(.*):(.*)', '\\2', coef)]
results.obama[, speaker := 'Obama']

mod.obama.std <- lm(
  as.matrix(
    utterances[speaker == 'Obama',
               features %.% '.std',
               with = FALSE
               ]
  ) ~
    0 +
    id +
    macroeconom +
    civil_right +
    healthcar +
    labour +
    educ +
    energi +
    transport +
    crime +
    social_welfar +
    financ +
    defenc +
    sstc +
    #foreign_trad +
    land_water_manag +
    cultur +
    religion,
  utterances[speaker == 'Obama',]
)
vcov.obama <- vcovCL(mod.obama.std, cl = utterances[speaker == 'Obama', id])
results.obama.std <- coeftest(mod.obama.std, vcov.obama)
results.obama.std <- as.matrix(
  results.obama.std[!rownames(results.obama.std) %like% '(^|:)id',]
  )
colnames(results.obama.std) <- c('est', 'se', 't', 'p')
results.obama.std <- data.table(coef = rownames(results.obama.std), results.obama.std)
results.obama.std[, outcome := gsub('(.*):(.*)', '\\1', coef)]
results.obama.std[, coef := gsub('(.*):(.*)', '\\2', coef)]
results.obama.std[, speaker := 'Obama']

mod.romney <- lm(
  as.matrix(
    utterances[speaker == 'Romney',
               features,
               with = FALSE
               ]
  ) ~
    0 +
    id +
    macroeconom +
    civil_right +
    healthcar +
    labour +
    educ +
    energi +
    transport +
    crime +
    social_welfar +
    financ +
    defenc +
    sstc +
    #foreign_trad +
    land_water_manag +
    cultur +
    religion,
  utterances[speaker == 'Romney',]
)
vcov.romney <- vcovCL(mod.romney, cl = utterances[speaker == 'Romney', id])
results.romney <- coeftest(mod.romney, vcov.romney)
results.romney <- as.matrix(
  results.romney[!rownames(results.romney) %like% '(^|:)id',]
  )
colnames(results.romney) <- c('est', 'se', 't', 'p')
results.romney <- data.table(coef = rownames(results.romney), results.romney)
results.romney[, outcome := gsub('(.*):(.*)', '\\1', coef)]
results.romney[, coef := gsub('(.*):(.*)', '\\2', coef)]
results.romney[, speaker := 'Romney']

mod.romney.std <- lm(
  as.matrix(
    utterances[speaker == 'Romney',
               features %.% '.std',
               with = FALSE
               ]
  ) ~
    0 +
    id +
    macroeconom +
    civil_right +
    healthcar +
    labour +
    educ +
    energi +
    transport +
    crime +
    social_welfar +
    financ +
    defenc +
    sstc +
    #foreign_trad +
    land_water_manag +
    cultur +
    religion,
  utterances[speaker == 'Romney',]
)
vcov.romney <- vcovCL(mod.romney.std, cl = utterances[speaker == 'Romney', id])
results.romney.std <- coeftest(mod.romney.std, vcov.romney)
results.romney.std <- as.matrix(
  results.romney.std[!rownames(results.romney.std) %like% '(^|:)id',]
  )
colnames(results.romney.std) <- c('est', 'se', 't', 'p')
results.romney.std <- data.table(coef = rownames(results.romney.std), results.romney.std)
results.romney.std[, outcome := gsub('(.*):(.*)', '\\1', coef)]
results.romney.std[, coef := gsub('(.*):(.*)', '\\2', coef)]
results.romney.std[, speaker := 'Romney']

## combine results
results <- rbind(results.obama, results.romney)
results[, p.adj := p.adjust(p, 'BY')]
results.std <- rbind(results.obama.std, results.romney.std)
results.std[, p.adj := p.adjust(p, 'BY')]

## recode/label for plots
results[, coef.label := topics.labels[coef]]
results.std[, coef.label := topics.labels[coef]]
results[, outcome.label := features.labels[outcome]]
results.std[, outcome.label := features.labels[gsub('\\.std', '', outcome)]]

obama_all_standardized <- ggplot(
  results.std[speaker == 'Obama' &
                outcome != 'pitch.d1.sd.std' &
                coef != 'foreign_trad',
              ],
  aes(x = outcome.label,
      y = est,
      ymin = est + se * qnorm(.025),
      ymax = est + se * qnorm(.975),
      size = p.adj < .05,
      color = p.adj < .05
      )
) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  geom_point(size = 1.5) +
  geom_errorbar(width = .1, size=.75) +
  facet_wrap('coef.label',
             ncol = 5
             ) +
  scale_color_manual(values = c('TRUE' = red,
                                'FALSE' = medgray
                                ),
                     guide = FALSE
                     ) +
  scale_size_manual(values = c('TRUE' = 1.5,
                               'FALSE' = .5
                               ),
                    guide = FALSE
                    ) +
  theme_light(element_text(family="Helvetica", size=15), base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Change in vocal style\n') +
  xlab('')
ggsave(filename = file.path(output.dir, 'obama_all_standardized.png'),
       obama_all_standardized,
       width = 8,
       height = 8
       )

romney_all_standardized <- ggplot(
  results.std[speaker == 'Romney' &
                outcome != 'pitch.d1.sd.std' &
                coef != 'foreign_trad',
              ],
  aes(x = outcome.label,
      y = est,
      ymin = est + se * qnorm(.025),
      ymax = est + se * qnorm(.975),
      size = p.adj < .05,
      color = p.adj < .05
      )
) +
  geom_hline(yintercept = 0, color = 'black', linetype = 'dashed') +
  geom_point(size = 1.5) +
  geom_errorbar(width = .1, size=.75) +
  facet_wrap('coef.label',
             ncol = 5
             ) +
  scale_color_manual(values = c('TRUE' = red,
                                'FALSE' = medgray
                                ),
                     guide = FALSE
                     ) +
  scale_size_manual(values = c('TRUE' = 1.5,
                               'FALSE' = .5
                               ),
                    guide = FALSE
                    ) +
  theme_light(element_text(family="Helvetica", size=15), base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab('Change in vocal style\n') +
  xlab('')
ggsave(filename = file.path(output.dir,'romney_all_standardized.png'),
       romney_all_standardized,
       width = 8,
       height = 8
       )

