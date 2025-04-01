library(devtools)
install_version('mppa', version='1.0')

library(data.table)
library(ggplot2)
library(dplyr)
library(data.table)
library(mppa)  # simes.test

red <- '#A31F34'
blue <- '#4E84C4'

data.long <- fread('../intermediate_files/experiment_one.csv')
data.long$V1 <- NULL

proportions <- data.long %>%
  group_by(vignette, treatment, outcome) %>%
  summarise(x = sum(choice),
            n = n()
            )
proportions <- proportions %>%
  mutate(mean.choice = x / n)

phrase.shortnames <- c(
  'Tax cuts',
  'Make it',
  'Fair shot',
  'Medicare',
  'Energy',
  'Offshore',
  'Bailout',
  'Terror',
  'Military',
  'College',
  'Change',
  'Plurality',
  'Opportunity',
  'Students',
  'Can’t afford',
  'Top-down',
  'Deficit',
  'Economy',
  'Math',
  'Renewables',
  'Work',
  'Wealthy',
  'Apologize',
  'Rights',
  'Hymn',
  'Better days',
  'Same course',
  'Divide',
  'Promised',
  'Both sides'
)

proportions$vignette.text <- phrase.shortnames[proportions$vignette]
proportions$treatment.text <- c('text', 'audio', 'video')[proportions$treatment]

main_results <-
  ggplot(proportions,
         aes(x = outcome,
             y = mean.choice,
             color = treatment.text
             )
         ) +
  facet_wrap('vignette.text') +
  geom_point(position = position_dodge(width = .5), size = 3) +
  geom_hline(yintercept = .5, linetype = 'dashed') +
  scale_color_manual(values = c(text = 'black',
                                audio = red,
                                video = blue),
                     name = 'Mode of delivery') +
  theme_light(element_text(family="Helvetica", size=10), base_size = 20) +
  theme(legend.position = 'bottom',
        axis.text.x = element_text(hjust = 1, angle = 45)) +
  xlab(NULL) +
  ylab('Proportion selecting variant A\n')
ggsave('../figures/experiment_one_main.png',
       main_results, width = 16, height = 12, dpi = 500, units = "in")



########################################
## script- and outcome-specific tests ##
########################################

proportions <- data.table(proportions)

## when text is identical, compute & test choice prop deviation from 0.5
proportions[vignette.text == 'Work' & treatment.text == 'audio',
            `:=`(diff.audio = abs(mean.choice - .5),
                 p.audio = prop.test(x, n, p = .5)$p.value
                 ),
            by = c('vignette.text', 'outcome')
            ]
proportions[vignette.text == 'Work' & treatment.text == 'video',
            `:=`(diff.video = abs(mean.choice - .5),
                 p.video = prop.test(x, n, p = .5)$p.value
                 ),
            by = c('vignette.text', 'outcome')
            ]

## otherwise, compute & test if audio/video choice prop deviation from text prop
proportions[vignette.text != 'Work' & treatment.text %in% c('text', 'audio'),
            `:=`(diff.audio = diff(mean.choice),
                 p.audio = prop.test(x, n)$p.value
                 ),
            by = c('vignette.text', 'outcome')
            ]
proportions[vignette.text != 'Work' & treatment.text %in% c('text', 'video'),
            `:=`(diff.video = diff(mean.choice),
                 p.audio = prop.test(x, n)$p.value
                 ),
            by = c('vignette.text', 'outcome')
            ]

## audio effect (diff from .5 for work, diff from text prop for others)
## aggregating over the 29 catchphrases, average effect is 11.4pp
## evaluation-specific effect ranges from 9.7pp (knowledgeable) to 12.5pp (strong)
proportions.audio <- proportions[treatment.text == 'audio',]
proportions.audio[, .(diff.audio.mean = mean(abs(diff.audio)))]
proportions.audio[,
                  .(diff.audio.mean = mean(abs(diff.audio))),
                  by = 'outcome'
                  ]

## average magnitude of evaluation-specific video effects range from 10.7pp to 11.6pp
proportions.video <- proportions[treatment.text == 'video',]
proportions.video[, .(diff.video.mean = mean(abs(diff.video)))]
proportions.video[,
                  .(diff.video.mean = mean(abs(diff.video))),
                  by = 'outcome'
                  ]

## simes test aggregating outcome-specific audio p-values within each script
proportions.audio.aggregated <- proportions.audio[
 ,
   .(p.audio.intersect = round(simes.test(p.audio), 4)),
   by = 'vignette.text'
]
proportions.audio.aggregated[
 ,
   p.audio.intersect.adjust := p.adjust(p.audio.intersect, 'BH')
]
proportions.audio.aggregated[p.audio.intersect.adjust < .05,]



#####################################
## examples discussed in main text ##
#####################################

### work script, video and audio ###

## work script, video arm
test.work.video <- proportions %>%
  filter(vignette.text == 'Work',
         treatment.text == 'video'
         )
## proud and strong results
test.work.video %>% filter(outcome %in% c('strong', 'proud'))
## absolute deviation from .5 of over 20pp
min(abs(test.work.video$mean.choice - .5))
## max p value < .001
max(test.work.video$p.video)

## work script, audio arm
test.work.audio <- proportions %>%
  filter(vignette.text == 'Work',
         treatment.text == 'audio'
         )
## proud and strong results
test.work.audio %>% filter(outcome %in% c('strong', 'proud'))
## absolute deviation from .5 of over 17.5pp
min(abs(test.work.audio$mean.choice - .5))
## max p value < .005
max(test.work.audio$p.audio)



### military script, text vs audio ###

## military script, text arm
test.military.text <- proportions %>%
  filter(vignette.text == 'Military',
         treatment.text == 'text'
         ) %>%
  mutate(choice.spread = 2 * abs(50 - 100 * mean.choice))
## 4pp spread in inspiring
test.military.text %>%
  filter(outcome == 'inspiring') %>%
  select(outcome, mean.choice, choice.spread)

## military script, audio arm
test.military.audio <- proportions %>%
  filter(vignette.text == 'Military',
         treatment.text == 'audio'
         ) %>%
  mutate(choice.spread = 2 * abs(50 - 100 * mean.choice))
## military script, audio arm: 40pp spread in inspiring
test.military.audio %>%
  filter(outcome == 'inspiring') %>%
  select(outcome, mean.choice, choice.spread)
