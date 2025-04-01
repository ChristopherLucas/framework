library(reshape2)
library(stringr)

################################
# Load and Merge MTurk Batches #
################################

# Responses were gathered from three MTurk HITs (9, 10, and 10 
# phrases). First, clean and merge.

# Reuse this vector to index columns for different
# outcome-vignette pairs, which are spread across
# different columns in wide form
outcomes <- c('angry', 'afraid', 'hopeful', 'proud',
              'strong', 'knowledge', 'moral', 'inspiring')

## Load Batch 1 ##
# NOTE: Phrase 2 in batch 1 was excluded from the experiment because, 
# despite being good matched pair, the video was broken/frozen 
# in the low emotion condition. This was done before running the 
# experiment. IE, we didn't drop any observations. See column
# names in batch 1 to note this.
batch1 <- read.csv("../data/batch1.csv", stringsAsFactors = FALSE)
    
# Drop extra qualtrics headers
batch1 <- batch1[3:nrow(batch1),]

# Correct variable with inconsisent name
colnames(batch1)[colnames(batch1)=="anger1a"] <- "angry1a"
colnames(batch1)[colnames(batch1)=="anger1b"] <- "angry1b"

# Subjects who missed the audio check are recorded as complete
# responses. Drop them, and also drop any complete responses
# from an identical IP
batch1$audio_q <- as.factor(batch1$audio_q)
tt <- table(batch1$audio_q)
correct.audio.check <- names(tt[tt==max(tt)]) # Correct was most common

audio.fail.IPs <- unique(batch1$IPAddress[batch1$audio_q != correct.audio.check])
batch1 <- batch1[!batch1$IPAddress %in% audio.fail.IPs,]

## Rows are respondents. Convert to respodent-phrase-outcome.

# Subset to columns we'll keep in the melt
outcome.colnames <- grep(paste(outcomes, collapse="|"),
                         colnames(batch1),
                         value = T)
treatment.colnames <- grep('rand_',
                            colnames(batch1),
                            value = T)
keep.vars <- c(outcome.colnames, treatment.colnames, 'mturk_code')

batch1 <- batch1[,keep.vars]

## Melt all the outcomes into variable column
batch1.long <- melt(batch1,                               
                    id.vars = c("mturk_code", "rand_1_assignment",
                                "rand_2_assignment",  "rand_3_assignment",
                                "rand_4_assignment",  "rand_5_assignment",
                                "rand_6_assignment",  "rand_7_assignment",
                                "rand_8_assignment",  "rand_9_assignment",
                                "rand_10_assignment"),
                    variable.name = "outcome",
                    value.name = "choice"
                    )

# Drop columns that correspond to treatment conditions that the respondent
# was not assigned to
batch1.long <- batch1.long[nchar(batch1.long$choice) > 0, ]

# Convert subject choice to binary so we can invert the half that saw
# reverse order more cleanly
batch1.long$choice <- batch1.long$choice == 'Statement A'

# [outcome]a/b denotes whether the high/low emotion
# video was shown first/second. Flip those for b so 
# they're all the same
batch1.long$choice[grepl('*\\d+b', batch1.long$outcome)] <-
    !batch1.long$choice[grepl('*\\d+b', batch1.long$outcome)]    

# Extract the vignette (1-10) for the respective response
batch1.long$vignette <- as.numeric(gsub("[^\\d]+", "", batch1.long$outcome, perl=TRUE))

# Identify the treatment and drop redundant colums
treatment.assignments <- batch1.long[,treatment.colnames]
batch1.long$treatment <- treatment.assignments[cbind(1:nrow(treatment.assignments),
                                                     batch1.long$vignette)]
batch1.long$outcome <- gsub("\\d.*", "", batch1.long$outcome)
batch1.long <- batch1.long[,!(colnames(batch1.long) %in% treatment.colnames)]

# Check dimensions to make sure the merge did what we think it did
nrow(batch1.long) /
    length(unique(batch1.long$outcome)) /
    length(unique(batch1.long$vignette)) == nrow(batch1)


## Load Batch 2 ##
batch2 <- read.csv("../data/batch2.csv", stringsAsFactors = FALSE)

# Drop extra qualtrics headers
batch2 <- batch2[3:nrow(batch2),]

# Correct incorrect qualtrics question names
colnames(batch2) <- gsub('a\\.1', 'b', colnames(batch2))
colnames(batch2)[colnames(batch2)=="angry10"] <- "angry10a"

# Subjects who missed the audio check are recorded as complete
# responses. Drop them, and also drop any complete responses
# from an identical IP
audio.fail.IPs <- unique(batch2$IPAddress[batch2$audio_q != correct.audio.check])
batch2 <- batch2[!batch2$IPAddress %in% audio.fail.IPs,]

# Subset to columns we'll keep in the melt
outcome.colnames <- grep(paste(outcomes, collapse="|"),
                         colnames(batch2),
                         value = T)
treatment.colnames <- grep('rand_',
                            colnames(batch2),
                            value = T)
keep.vars <- c(outcome.colnames, treatment.colnames, 'mturk_code')

batch2 <- batch2[,keep.vars]

## Melt all the outcomes into variable column
batch2.long <- melt(batch2,                               
                    id.vars = c("mturk_code", "rand_1_assignment",
                                "rand_2_assignment",  "rand_3_assignment",
                                "rand_4_assignment",  "rand_5_assignment",
                                "rand_6_assignment",  "rand_7_assignment",
                                "rand_8_assignment",  "rand_9_assignment",
                                "rand_10_assignment"),
                    variable.name = "outcome",
                    value.name = "choice"
                    )

# Drop columns that correspond to treatment conditions that the respondent
# was not assigned to
batch2.long <- batch2.long[nchar(batch2.long$choice) > 0, ]

# Convert subject choice to binary so we can invert the half that saw
# reverse order more cleanly
batch2.long$choice <- batch2.long$choice == 'Statement A'

# [outcome]a/b denotes whether the high/low emotion
# video was shown first/second. Flip those for b so 
# they're all the same
batch2.long$choice[grepl('*\\d+b', batch2.long$outcome)] <-
    !batch2.long$choice[grepl('*\\d+b', batch2.long$outcome)]    

# Extract the vignette (1-10) for the respective response
batch2.long$vignette <- as.numeric(gsub("[^\\d]+", "", batch2.long$outcome, perl=TRUE))

# Identify the treatment and drop redundant colums
treatment.assignments <- batch2.long[,treatment.colnames]
batch2.long$treatment <- treatment.assignments[cbind(1:nrow(treatment.assignments),
                                                     batch2.long$vignette)]
batch2.long$outcome <- gsub("\\d.*", "", batch2.long$outcome)
batch2.long <- batch2.long[,!(colnames(batch2.long) %in% treatment.colnames)]

# Check dimensions to make sure the merge did what we think it did
nrow(batch2.long) /
    length(unique(batch2.long$outcome)) /
    length(unique(batch2.long$vignette)) == nrow(batch2)

## Load Batch 3 ##
batch3 <- read.csv("../data/batch3.csv", stringsAsFactors = FALSE)

# Drop extra qualtrics headers
batch3 <- batch3[3:nrow(batch3),]

# Correct incorrect qualtrics question names
colnames(batch3) <- gsub('a\\.1', 'b', colnames(batch3))
colnames(batch3)[colnames(batch3)=="anger1a"] <- "angry1a"
colnames(batch3)[colnames(batch3)=="anger1b"] <- "angry1b"
colnames(batch3)[colnames(batch3)=="inspiring"] <- "inspiring6b"
colnames(batch3)[colnames(batch3)=="angry10"] <- "angry10a"

# Subjects who missed the audio check are recorded as complete
# responses. Drop them, and also drop any complete responses
# from an identical IP
audio.fail.IPs <- unique(batch3$IPAddress[batch3$audio_q != correct.audio.check])
batch3 <- batch3[!batch3$IPAddress %in% audio.fail.IPs,]

# Subset to columns we'll keep in the melt
outcome.colnames <- grep(paste(outcomes, collapse="|"),
                         colnames(batch3),
                         value = T)
treatment.colnames <- grep('rand_',
                            colnames(batch3),
                            value = T)
keep.vars <- c(outcome.colnames, treatment.colnames, 'mturk_code')

batch3 <- batch3[,keep.vars]

## Melt all the outcomes into variable column
batch3.long <- melt(batch3,                               
                    id.vars = c("mturk_code", "rand_1_assignment",
                                "rand_2_assignment",  "rand_3_assignment",
                                "rand_4_assignment",  "rand_5_assignment",
                                "rand_6_assignment",  "rand_7_assignment",
                                "rand_8_assignment",  "rand_9_assignment",
                                "rand_10_assignment"),
                    variable.name = "outcome",
                    value.name = "choice"
                    )

# Drop columns that correspond to treatment conditions that the respondent
# was not assigned to
batch3.long <- batch3.long[nchar(batch3.long$choice) > 0, ]

# Convert subject choice to binary so we can invert the half that saw
# reverse order more cleanly
batch3.long$choice <- batch3.long$choice == 'Statement A'

# [outcome]a/b denotes whether the high/low emotion
# video was shown first/second. Flip those for b so 
# they're all the same
batch3.long$choice[grepl('*\\d+b', batch3.long$outcome)] <-
    !batch3.long$choice[grepl('*\\d+b', batch3.long$outcome)]    

# Extract the vignette (1-10) for the respective response
batch3.long$vignette <- as.numeric(gsub("[^\\d]+", "", batch3.long$outcome, perl=TRUE))

# Identify the treatment and drop redundant colums
treatment.assignments <- batch3.long[,treatment.colnames]
batch3.long$treatment <- treatment.assignments[cbind(1:nrow(treatment.assignments),
                                                     batch3.long$vignette)]
batch3.long$outcome <- gsub("\\d.*", "", batch3.long$outcome)
batch3.long <- batch3.long[,!(colnames(batch3.long) %in% treatment.colnames)]

# Check dimensions to make sure the merge did what we think it did
nrow(batch3.long) /
    length(unique(batch3.long$outcome)) /
    length(unique(batch3.long$vignette)) == nrow(batch3)


## Merge all three batches ##

# The batches contained different vignettes. Increment accordingly.
batch2.long$vignette <- batch2.long$vignette + 10
batch3.long$vignette <- batch3.long$vignette + 20

exp1.df <- rbind(batch1.long, batch2.long, batch3.long)

colnames(exp1.df)[colnames(exp1.df) == 'mturk_code'] <- 'ID'

# In this new long form, 'ID' is the subject ID, 'outcome' is the trait they rated everthing on, 
# 'choice' is the choice they made between A and B, vignette indexes the different pairs of statements (1-30),
# and 'treatment' notes whether they were assigned to text (1), audio (2), or video (3)

write.csv(exp1.df, '../data/experiment_one.csv')
