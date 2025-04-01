## data manipulation
library(data.table)
library(dplyr)

## analysis
library(sandwich)
library(lmtest)

## viz
library(ggplot2)

options(stringsAsFactors = FALSE)



###########
## style ##
###########

lightgray <- 'gray80'
medgray <- 'gray50'
darkgray <- 'gray20'
red <- '#A31F34'
blue <- '#4E84C4'



###############
## constants ##
###############

issues <- c('budget',
            'climate',
            'education',
            'military',
            'nationalism',
            'social'
            )

treatments <- c('mod',
                'pitch',
                'rate',
                'vol'
                )

treatment.labels <- c(mod = 'Increased\nmodulation',
                      pitch = 'Heightened\npitch',
                      rate = 'Faster\nspeech rate',
                      vol = 'Louder\nvolume'
                      )

outcomes <- c('competent',
              'enthusiastic',
              'inspiring',
              'passionate',
              'persuasive',
              'trustworthy',
              'vote'
              )

outcome.labels <- c(competent = 'Competent',
                    enthusiastic = 'Enthusiastic',
                    inspiring = 'Inspiring',
                    passionate = 'Passionate',
                    persuasive = 'Persuasive',
                    trustworthy = 'Trustworthy',
                    vote = 'Vote'
                    )

## 1:  Christopher
## 2:  Gale
## 3:  James
## 4:  Jamie
## 5:  Jermey
## 6:  Kia
## 7:  Patti
## 8:  Ruth
## 9:  Spencer
## 10: Tracy
speakers.female <- c(2, 6, 7, 8, 10)

output.dir <- '../figures'



###############
## functions ##
###############

`%.%` <- paste0



####################
## survey results ##
####################

d <- fread('../data/cleaned_survey.csv')
d <- na.omit(d)

## clean up row index
d[, V1 := NULL]
d[, id := 1:.N]

## fix malformed column
d$budget.audio <- gsub('Q98(0|1)', '', d$budget.audio)

## identify speaker using name of assigned clip
for (issue in issues){
  d[[issue %.% '.speaker']] <- as.integer(
    gsub('(\\d+)\\..*', '\\1', d[[issue %.% '.audio']])
  )
  d[[issue %.% '.audio']] <- NULL
}

d <- melt(d,
          id.vars = c('id',
                      'IPAddress',
                      'LocationLatitude',
                      'LocationLongitude',
                      'birth.year',
                      'education',
                      'race',
                      'hispanic',
                      'sex',
                      'income',
                      'zip',
                      'voted',
                      'ideology'
                      )
          )
d[, issue := gsub('(.*)\\.(.*)', '\\1', variable)]
d[, variable := gsub('(.*)\\.(.*)', '\\2', variable)]

## reshape
d <- dcast(d,
           id +
             IPAddress +
             LocationLatitude +
             LocationLongitude +
             birth.year +
             education +
             race +
             hispanic +
             sex +
             income +
             zip +
             voted +
             ideology +
             issue ~
               variable
           )

d[, speaker := factor(speaker)]
d[, speaker.gender := ifelse(speaker %in% speakers.female,
                             'female',
                             'male'
                             )
  ]



##################
## analyze data ##
##################

## men are seen as:
##   more competent
##   more enthusiastic
##   more inspiring
##   more passionate
##   more persuasive
##   EQUALLY trustworthy
##   more likely to get vote
## by ~2.5 percentage points (p<0.001 for all but trustworthy)

for (outcome in outcomes){
  cat('\n\n', rep('-', 20), '\n\n', sep = '')
  cat(outcome, '\n')
  mod <- lm(
    get(outcome) ~
      speaker.gender +
      mod +
      pitch +
      rate +
      vol +
      issue,
    d
  )
  vcov <- vcovCL(mod, d$id)
  print(
    round(coeftest(mod, vcov)['speaker.gendermale', ], 3)
  )
}

mods.base <- list()
for (outcome in outcomes){
  mod <- lm(
    get(outcome) ~
      0 +
      speaker +
      mod +
      pitch +
      rate +
      vol +
      issue,
    d
  )
  vcov <- vcovCL(mod, d$id)
  mods.base[[outcome]] <- list(mod = mod, vcov = vcov)
}

mods.gender <- list()
for (outcome in outcomes){
  mod <- lm(
    get(outcome) ~
      0 +
      speaker +
      ## mod +
      ## pitch +
      ## rate +
      ## vol +
      mod:speaker.gender +
      pitch:speaker.gender +
      rate:speaker.gender +
      vol:speaker.gender +
      issue,
    d
  )
  vcov <- vcovCL(mod, d$id)
  mods.gender[[outcome]] <- list(mod = mod, vcov = vcov)
}

mods.genderdiff <- list()
for (outcome in outcomes){
  mod <- lm(
    get(outcome) ~
      0 +
      speaker +
      mod +
      pitch +
      rate +
      vol +
      mod:speaker.gender +
      pitch:speaker.gender +
      rate:speaker.gender +
      vol:speaker.gender +
      issue,
    d
  )
  vcov <- vcovCL(mod, d$id)
  mods.genderdiff[[outcome]] <- list(mod = mod, vcov = vcov)
}

coefs.base <- data.table(rbindlist(
  lapply(outcomes,
         function(outcome){
           mod <- mods.base[[outcome]]$mod
           vcov <- mods.base[[outcome]]$vcov
           cbind(outcome,
                 coef = names(coef(mod)),
                 as.data.frame(coeftest(mod, vcov)[, 1:2])
                 )
         })
))
colnames(coefs.base) <- c('outcome', 'coef', 'est', 'se')
coefs.base[
  coef %like% 'speaker',
  speaker.gender := ifelse(coef %in% ('speaker' %.% speakers.female),
                           'Female',
                           'Male'
                           )
]

coefs.gender <- data.table(rbindlist(
  lapply(outcomes,
         function(outcome){
           mod <- mods.gender[[outcome]]$mod
           vcov <- mods.gender[[outcome]]$vcov
           cbind(outcome,
                 coef = names(coef(mod)),
                 as.data.frame(coeftest(mod, vcov)[, 1:2])
                 )
         })
))
colnames(coefs.gender) <- c('outcome', 'coef', 'est', 'se')
coefs.gender[
  coef %like% 'speaker\\.gender',
  speaker.gender := ifelse(coef %like% 'female',
                           'Female',
                           'Male'
                           )
]
coefs.gender[
  coef %like% 'speaker\\.gender',
  coef := gsub(':?speaker\\.gender(fe)?male:?', '', coef)
]

coefs.genderdiff <- data.table(rbindlist(
  lapply(outcomes,
         function(outcome){
           mod <- mods.genderdiff[[outcome]]$mod
           vcov <- mods.genderdiff[[outcome]]$vcov
           cbind(outcome,
                 coef = names(coef(mod)),
                 as.data.frame(coeftest(mod, vcov)[, 1:2])
                 )
         })
))
colnames(coefs.genderdiff) <- c('outcome', 'coef', 'est', 'se')



##########
## plot ##
##########

speakers.ordered <- coefs.base[
  outcome == 'vote' & coef %like% 'speaker',
  coef[order(speaker.gender, est)]
]

## separate plot for speaker fe
coefs.base.speakers <- coefs.base[coef %like% 'speaker',]
coefs.base.speakers[
 ,
   Speaker := factor(coef,
                     levels = speakers.ordered,
                     labels = LETTERS[seq_along(speakers.ordered)],
                     ordered = TRUE
                     )
]

coefs.base[, outcome.label := outcome.labels[outcome]]
coefs.base.speakers[, outcome.label := outcome.labels[outcome]]
coefs.gender[, outcome.label := outcome.labels[outcome]]

actors_results_speaker_vote <- ggplot(
  coefs.base.speakers[outcome == 'vote',],
  aes(x = Speaker, y = est,
      ymin = est + qnorm(.025) * se,
      ymax = est + qnorm(.975) * se,
      color = speaker.gender)) +
  geom_point(size = 2) +
  labs(x = NULL, y = 'Average evaulation\n', color = '') +
  geom_errorbar(width = .3, size = 1) +
  facet_wrap('outcome.label') +
  scale_color_manual(values = c(Female = red, Male = blue),
                     labels = c(Female = 'Women', Male = 'Men')
                     ) +
  theme_light(base_size = 24) +
  theme(legend.position = 'bottom')
ggsave(filename = file.path(output.dir, 'actors_results_speaker_vote.png'),
       actors_results_speaker_vote,
       width = 12, height = 6, dpi = 500, units = "in")

actors_results_speaker_evaluation <- ggplot(
  coefs.base.speakers[outcome != 'vote',],
  aes(x = Speaker,
      y = est,
      ymin = est + qnorm(.025) * se,
      ymax = est + qnorm(.975) * se,
      color = speaker.gender)) +
  geom_point(size = 1.5) +
  geom_errorbar(width = .3, size = 1) +
  facet_wrap('outcome.label') +
  scale_color_manual(name = NULL, values = c(Female = red,Male = blue),
                     labels = c(Female = 'Women', Male = 'Men')
                     ) +
  theme_light(element_text(family="Helvetica", size=10), base_size = 24) +
  theme(legend.position = 'bottom') +
  ylab('Average evaluation\n') +
  xlab(NULL)
ggsave(filename = file.path(output.dir,'actors_results_speaker_evaluation.png'),
       actors_results_speaker_evaluation,
       width = 12, height = 9, dpi = 500, units = "in")


actors_results_speech_evaluation <- ggplot(
  coefs.base[!coef %like% 'speaker|issue' & outcome != 'vote',],
  aes(x = coef,
      y = est,
      ymin = est + qnorm(.025) * se,
      ymax = est + qnorm(.975) * se)) +
  geom_hline(yintercept = 0,linetype = 'dashed') +
  geom_point(size = 2) +
  geom_errorbar(size = 1, width = .25) +
  facet_wrap('outcome.label') +
  labs(x='', y='Change in evaluation\n') +
  theme_light(element_text(family="Helvetica", size=10), base_size = 24) +
  theme(legend.position = 'bottom') +
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  scale_x_discrete(breaks=c("mod", "pitch", "rate", "vol"),
                   labels=c("Modulation", "Pitch", "Rate", "Volume")) +
  ylim(-5, 25)
ggsave(filename = file.path(output.dir,'actors_results_speech_evaluation.png'),
       actors_results_speech_evaluation,
       width = 12, height = 9, dpi = 500, units = "in")

actors_results_speech_vote_gender <- ggplot(
  coefs.gender[!coef %like% 'speaker|issue' & outcome == 'vote',],
  aes(x = coef,
      y = est,
      ymin = est + qnorm(.025) * se,
      ymax = est + qnorm(.975) * se,
      color = speaker.gender)) +
  geom_hline(yintercept = 0,linetype = 'dashed') +
  geom_point(size = 2, position = position_dodge(width = .6)) +
  geom_errorbar(size = 1, position = position_dodge(width = .6), width = .4) +
  facet_wrap('outcome.label') +
  scale_color_manual(name = NULL,
                     values = c(Female = red, Male = blue),
                     labels = c(Female = 'Women', Male = 'Men')
                     ) +
  theme_light(element_text(family="Helvetica", size=10), base_size = 24) +
  theme(legend.position = 'bottom') +
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  scale_x_discrete(breaks=c("mod","pitch","rate","vol"),
                   labels=c("Modulation", "Pitch", "Rate", "Volume")) +
  ylim(-5, 10) +
  labs(x = NULL, y = 'Change in evaluation\n', color = '')
ggsave(filename = file.path(output.dir,
                            'actors_results_speech_vote_gender.png'
                            ),
       actors_results_speech_vote_gender,
       width = 8, height = 6, dpi = 500, units = "in")

actors_results_speech_evaluation_gender <- ggplot(
  coefs.gender[!coef %like% 'speaker|issue' & outcome != 'vote',],
  aes(x = coef,
      y = est,
      ymin = est + qnorm(.025) * se,
      ymax = est + qnorm(.975) * se,
      color = speaker.gender)) +
  geom_hline(yintercept = 0,linetype = 'dashed') +
  geom_point(size = 2, position = position_dodge(width = .6)) +
  geom_errorbar(size = 1, position = position_dodge(width = .6), width = .4) +
  facet_wrap('outcome.label') +
  scale_color_manual(name = NULL,
                     values = c(Female = red, Male = blue),
                     labels = c(Female = 'Women', Male = 'Men')
                     ) +
  theme_light(element_text(family="Helvetica", size=10), base_size = 24) +
  theme(legend.position = 'bottom') +
  theme(axis.text.x = element_text(hjust = 1, angle = 45)) +
  scale_x_discrete(breaks=c("mod","pitch","rate","vol"),
                   labels=c("Modulation", "Pitch", "Rate", "Volume")) +
  ylim(-5, 25) +
  labs(x = NULL, y = 'Change in evaluation\n', color = '')
ggsave(filename = file.path(output.dir,
                            'actors_results_speech_evaluation_gender.png'
                            ),
       actors_results_speech_evaluation_gender,
       width = 12, height = 9, dpi = 500, units = "in")
