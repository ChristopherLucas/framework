library(data.table)
library(ggplot2)
library(stringr)
library(sandwich)



###########################
# Experiment 2 Validation #
###########################
# Conjoint experiment is a fully-crossed experiment with 4 binary manipulations:
# Researcher: volume (loud/soft)
# Researcher: pitch (high/low)
# Actor: modulation (modulated/monotone)
# Actor: rate (fast/slow)

# 1:  Christopher
# 2:  Gale
# 3:  James
# 4:  Jamie
# 5:  Jermey
# 6:  Kia
# 7:  Patti
# 8:  Ruth
# 9:  Spencer
# 10: Tracy
speakers.female <- c(2, 6, 7, 8, 10)



###############
# Featurizing #
###############

fnames <- list.files('../data/actor_treatments',
                     pattern = '.wav$',
                     full.names = T)

audio.path <- '../intermediate_files/audio_features_actors.rds'
if (!file.exists(audio.path)){
  library(Rcpp)
  source('mass/R/extractAudioFeatures.R')
  actor.features <- extractAudioFeatures(wav.fnames = fnames,
                                         derivatives = 0,
                                         formants = TRUE,
                                         pitch = TRUE,
                                         energy = TRUE,
                                         zcr = TRUE,
                                         ac = TRUE,
                                         mfcc = FALSE
                                         )
  saveRDS(actor.features, file = audio.path)
} else {
  actor.features <- readRDS(file = audio.path)
}

audio <- actor.features$data
speakers <- as.numeric(gsub(".*?([0-9]+).*", "\\1", actor.features$files$filename))
female.audio <- as.numeric(speakers %in% speakers.female)

for(i in 1:length(audio)){
    # Average two f0 estimators
    audio[[i]][,'f0_mhs'][audio[[i]][,'f0_mhs'] == 0] <- NA
    audio[[i]][,'f0_ksv'][audio[[i]][,'f0_ksv'] == 0] <- NA
    audio[[i]] <- cbind(audio[[i]],
                        f0 = (audio[[i]][,'f0_mhs'] + audio[[i]][,'f0_ksv']) / 2
                        )

    # If either deviate by >10%, discard
    audio[[i]][audio[[i]][, 'f0_mhs'] > 1.1 * audio[[i]][, 'f0'], 'f0'] <- NA
    audio[[i]][audio[[i]][, 'f0_mhs'] < 0.9 * audio[[i]][, 'f0'], 'f0'] <- NA
    audio[[i]][audio[[i]][, 'f0_ksv'] > 1.1 * audio[[i]][, 'f0'], 'f0'] <- NA
    audio[[i]][audio[[i]][, 'f0_ksv'] < 0.9 * audio[[i]][, 'f0'], 'f0'] <- NA

    # ranges taken from: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3293852/
    # subset to vocal ranges
    if(female.audio[i] == 1){
        audio[[i]][audio[[i]][,'f0'] < 160, 'f0'] <- NA
        audio[[i]][audio[[i]][,'f0'] > 300, 'f0'] <- NA

    } else {
        audio[[i]][audio[[i]][,'f0'] < 60, 'f0'] <- NA
        audio[[i]][audio[[i]][,'f0'] > 180, 'f0'] <- NA
    }
}

audio.summaries <- data.frame(fname = actor.features$files$filename,
                              loudness = NA,
                              pitch = NA,
                              loudness.mod = NA,
                              pitch.mod = NA,
                              rate = NA)

script1 <- "Yes, we're gonna need to cut our deficit by 4 trillion dollars over the next 10 years. And I've already worked with Republicans and Democrats to cut a trillion dollars in spending. I'm ready to do more."
script2 <- "I am also proud to be the first president to include in my budget a plan for nationwide paid family leave, so that every new parent has the chance to bond with their newborn child."
script3 <- "No nation, however large or small, wealthy or poor, can escape the impact of climate change. The security and stability of each nation and all peoples, our prosperity, our health, our safety, are in jeopardy. And the time we have to reverse this tide is running out."
script4 <- "My fellow Americans, a short time ago, I ordered the United States Armed Forces to launch precision strikes on targets associated with the chemical weapons capabilities of Syrian dictator Bashar al-Assad. A combined operation with the armed forces of France and the United Kingdom is now underway. We thank them both."
script5 <- "Charter schools are here to stay. We’re now seeing the first generation of charter students raising children of their own. They know the difference educational choice made in their lives, and now as parents they want the same options for their children."
script6 <- "No act of terror will dim the light of the values that we proudly shine on the rest of the world, and no act of violence will shake the resolve of the United States of America."

script.wordcount <- c(str_count(script1, "\\S+"),
                      str_count(script2, "\\S+"),
                      str_count(script3, "\\S+"),
                      str_count(script4, "\\S+"),
                      str_count(script5, "\\S+"),
                      str_count(script6, "\\S+"))

for(i in 1:nrow(audio.summaries)){
    # first/second moments of loudness and pitch
    audio.summaries$loudness[i] = mean(audio[[i]][,'energy_dB'], na.rm = TRUE)
    audio.summaries$pitch[i] = mean(audio[[i]][,'f0'], na.rm = TRUE)
    audio.summaries$loudness.mod[i] = var(audio[[i]][,'energy_dB'], na.rm = TRUE)
    audio.summaries$pitch.mod[i] = var(audio[[i]][,'f0'], na.rm = TRUE)

    # rate of speech
    script.number <- as.numeric(sub(".*?\\.([0-9]+).*", '\\1', audio.summaries$fname[i]))
    wordcount <- script.wordcount[as.numeric(script.number)]
    words.per.second <- wordcount / # num words
         actor.features$files$duration[i] # num seconds
    audio.summaries$rate[i] <- words.per.second
}

audio.summaries$speaker <- as.numeric(sub(".*?([0-9]+).*", "\\1", actor.features$files$filename))
audio.summaries$script <- as.numeric(sub(".*?\\.([0-9]+).*", '\\1', audio.summaries$fname))


audio.summaries <- data.table(audio.summaries)

manipulations <- list(
  rate = c(hi = 'fast', lo = 'slow'),
  modulation = c(hi = 'mod', lo = 'mono'),
  volume = c(hi = 'loud', lo = 'soft'),
  pitch = c(hi = 'high', lo = 'low')
)

contrasts <- data.table()
for (i in 1:length(manipulations)){

  hi.args <- lo.args <- list(rate = '%s',
                             modulation = '%s',
                             volume = '%s',
                             pitch = '%s'
                             )

  hi.args[[i]] <- manipulations[[i]]['hi']
  lo.args[[i]] <- manipulations[[i]]['lo']
  hi.printf <- paste(hi.args, collapse = '')
  lo.printf <- paste(lo.args, collapse = '')

  fixed <- do.call(expand.grid, manipulations[-i])

  hi.regexes <- sprintf(hi.printf, fixed[, 1], fixed[, 2], fixed[, 3])
  lo.regexes <- sprintf(lo.printf, fixed[, 1], fixed[, 2], fixed[, 3])

  contrasts <- rbind(
    contrasts,
    audio.summaries[,
                    .(manipulation = names(manipulations)[i],
                      hi.inds = sapply(hi.regexes, function(x) grep(x, fname)),
                      lo.inds = sapply(lo.regexes, function(x) grep(x, fname))
                      ),
                    by = c('speaker', 'script')
                    ],
    fill = TRUE
  )

}

contrasts[,
          `:=`(hi.fname = basename(audio.summaries[hi.inds, fname]),
               lo.fname = basename(audio.summaries[lo.inds, fname]),
               hi.recording =
                 gsub('(fast|slow)(mod|mono)(loud|soft)(high|low)',
                      '\\1\\2',
                      gsub(' ', '', basename(audio.summaries[hi.inds, fname]))
                      ),
               lo.recording =
                 gsub('(fast|slow)(mod|mono)(loud|soft)(high|low)',
                      '\\1\\2',
                      gsub(' ', '', basename(audio.summaries[lo.inds, fname]))
                      ),
               hi.rate = audio.summaries[hi.inds, rate],
               lo.rate = audio.summaries[lo.inds, rate],
               hi.loudness = audio.summaries[hi.inds, loudness],
               lo.loudness = audio.summaries[lo.inds, loudness],
               hi.pitch = audio.summaries[hi.inds, pitch],
               lo.pitch = audio.summaries[lo.inds, pitch],
               hi.loudness.mod = audio.summaries[hi.inds, loudness.mod],
               lo.loudness.mod = audio.summaries[lo.inds, loudness.mod],
               hi.pitch.mod = audio.summaries[hi.inds, pitch.mod],
               lo.pitch.mod = audio.summaries[lo.inds, pitch.mod]
               )
          ]

mod.rate <- lm(
  I(hi.rate - lo.rate) ~
    0 + manipulation,
  contrasts
)
coef.rate <- coef(mod.rate)
se.rate <- sqrt(diag(
    vcovCL(mod.rate, cluster = contrasts[, .(hi.recording, lo.recording)])
  ))

mod.pitch <- lm(
  I(hi.pitch - lo.pitch) ~
    0 + manipulation,
  contrasts
)
coef.pitch <- coef(mod.pitch)
se.pitch <- sqrt(diag(
    vcovCL(mod.pitch, cluster = contrasts[, .(hi.recording, lo.recording)])
))

mod.loudness <- lm(
  I(hi.loudness - lo.loudness) ~
    0 + manipulation,
  contrasts
)
coef.loudness <- coef(mod.loudness)
se.loudness <- sqrt(diag(
    vcovCL(mod.loudness, cluster = contrasts[, .(hi.recording, lo.recording)])
  ))

mod.loudness.mod <- lm(
  I(hi.loudness.mod - lo.loudness.mod) ~
    0 + manipulation,
  contrasts
)
coef.loudness.mod <- coef(mod.loudness.mod)
se.loudness.mod <- sqrt(diag(
    vcovCL(mod.loudness.mod, cluster = contrasts[, .(hi.recording, lo.recording)])
  ))

mod.pitch.mod <- lm(
  I(hi.pitch.mod - lo.pitch.mod) ~
    0 + manipulation,
  contrasts
)
coef.pitch.mod <- coef(mod.pitch.mod)
se.pitch.mod <- sqrt(diag(
    vcovCL(mod.pitch.mod, cluster = contrasts[, .(hi.recording, lo.recording)])
))


balance <- data.table(
  variable = c(rep('rate', length(coef.rate)),
               rep('pitch', length(coef.pitch)),
               rep('loudness', length(coef.loudness)),
               rep('pitch.mod', length(coef.pitch.mod)),
               rep('loudness.mod', length(coef.loudness.mod))
               ),
  manipulation = gsub('manipulation',
                      '',
                      c(names(coef.rate),
                        names(coef.pitch),
                        names(coef.loudness),
                        names(coef.pitch.mod),
                        names(coef.loudness.mod)
                        )
                      ),
  diff = c(coef.rate,
           coef.pitch,
           coef.loudness,
           coef.pitch.mod,
           coef.loudness.mod
           ),
  se = c(se.rate,
         se.pitch,
         se.loudness,
         se.pitch.mod,
         se.loudness.mod
         )
)

balance[,
        variable.label := c(
          rate = 'Audio feature:\nRate (wps)',
          pitch = 'Audio feature:\nPitch (average)',
          loudness = 'Audio feature:\nLoudness (average)',
          pitch.mod = 'Audio feature:\nPitch (modulation)',
          loudness.mod = 'Audio feature:\nLoudness (modulation)'
        )[variable]
        ]

balance[,
        manipulation.label := c(
          modulation = 'Modulation',
          pitch = 'Pitch',
          rate = 'Rate',
          volume = 'Volume'
        )[manipulation]
        ]

balance.plot <- ggplot(balance,
                       aes(x = diff,
                           xmin = diff + se * qnorm(.025),
                           xmax = diff + se * qnorm(.975),
                           y = manipulation.label,
                           color = manipulation.label
                           )
                       ) +
  geom_point(size = 2.5) +
  geom_errorbarh(height = 0) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  theme_light(element_text(family="Helvetica", size=10), base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(x = "Difference Between High/Low Conditions For Each Manipulation",
       y = "Manipulation",
       color = "Manipulation") +
  facet_grid(. ~ variable.label,
             scales = 'free'
             )

ggsave('../figures/balance_plot.png',
       width = 11,
       height = 5,
       balance.plot)
