packages <- c('tm', 'stringr', 'syuzhet', 'xlsx')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}

library(tm)
library(stringr)
library(syuzhet)
library(xlsx)
rm(list=ls(all=TRUE))

# Transcripts of interviews are stored as separate text files (e.g. '1.txt, 2.txt...')
# in the below folder:
DataPath <- '//VARDA/vol1/files/research/DEB/Data Section/Active Projects/Faculty_Resident Projects/Kothari_Natural Language/DV Transcripts/dv raw text'

# We'll use the [tm] library to collect the texts and strip them down to lists of words.
# We need to make sure that contractions do not appear as NRC items once their
# punctuation has been stripped. (e.g. "I'll" oughn't be misinterpreted as "ill")
expandContractions <- function(foo){
  contractions <- c('aren\'t',
                    'can\'t',
                    'could\'ve',
                    'couldn\'t',
                    'didn\'t',
                    'doesn\'t',
                    'don\'t',
                    'gonna',
                    'gotta',
                    'hadn\'t',
                    'hasn\'t',
                    'haven\'t',
                    'he\'d',
                    'he\'ll',
                    'he\'s',
                    'how\'d',
                    'how\'ll',
                    'how\'s',
                    'I\'d',
                    'I\'ll',
                    'I\'m',
                    'I\'ve',
                    'isn\'t',
                    'it\'d',
                    'it\'ll',
                    'it\'s',
                    'mayn\'t',
                    'may\'ve',
                    'mightn\'t',
                    'might\'ve',
                    'mustn\'t',
                    'must\'ve',
                    'needn\'t',
                    'oughtn\'t',
                    'shan\'t',
                    'she\'d',
                    'she\'ll',
                    'she\'s',
                    'should\'ve',
                    'shouldn\'t',
                    'something\'s',
                    'that\'ll',
                    'that\'re',
                    'that\'s',
                    'that\'d',
                    'there\'d',
                    'there\'re',
                    'there\'s',
                    'these\'re',
                    'they\'d',
                    'they\'ll',
                    'they\'re',
                    'they\'ve',
                    'this\'s',
                    'those\'re',
                    'wasn\'t',
                    'we\'d',
                    'we\'d\'ve',
                    'we\'ll',
                    'we\'re',
                    'we\'ve',
                    'weren\'t',
                    'what\'d',
                    'what\'ll',
                    'what\'re',
                    'what\'s',
                    'what\'ve',
                    'when\'s',
                    'where\'d',
                    'where\'re',
                    'where\'s',
                    'where\'ve',
                    'which\'s',
                    'who\'d',
                    'who\'d\'ve',
                    'who\'ll',
                    'who\'re',
                    'who\'s',
                    'who\'ve',
                    'why\'d',
                    'why\'re',
                    'why\'s',
                    'won\'t',
                    'would\'ve',
                    'wouldn\'t',
                    'y\'all',
                    'you\'d',
                    'you\'ll',
                    'you\'re',
                    'you\'ve')
  expansions <-c ('are not',
                  'can not',
                  'could have',
                  'could not',
                  'did not',
                  'does not',
                  'do not',
                  'going to',
                  'got to',
                  'had not',
                  'has not',
                  'have not',
                  'he would',
                  'he will',
                  'he is',
                  'how did',
                  'how will',
                  'how is',
                  'I would',
                  'I will',
                  'I am',
                  'I have',
                  'is not',
                  'it would',
                  'it will',
                  'it is',
                  'may not',
                  'may have',
                  'might not',
                  'might have',
                  'must not',
                  'must have',
                  'need not',
                  'ought not',
                  'shall not',
                  'she would',
                  'she will',
                  'she is',
                  'should have',
                  'should not',
                  'something is',
                  'that will',
                  'that are',
                  'that is',
                  'that would',
                  'there would',
                  'there are',
                  'there has',
                  'these are',
                  'they would',
                  'they will',
                  'they are',
                  'they have',
                  'this has',
                  'those are',
                  'was not',
                  'we would',
                  'we would have',
                  'we will',
                  'we are',
                  'we have',
                  'were not',
                  'what did',
                  'what will',
                  'what are',
                  'what has',
                  'what have',
                  'when has',
                  'where did',
                  'where are',
                  'where has',
                  'where have',
                  'which has',
                  'who would',
                  'who would have',
                  'who will',
                  'who are',
                  'who is',
                  'who have',
                  'why did',
                  'why are',
                  'why is',
                  'will not',
                  'would have',
                  'would not',
                  'you all',
                  'you would',
                  'you will',
                  'you are',
                  'you have')
  names(expansions) <- contractions
  str_replace_all(foo, expansions)
}

# The [tm] library's 'removePunctuation' function fails to remove typographic
# quotes - we'll add the function 'leaveOnlyLetters' and use regex to solve that.
leaveOnlyLetters <- function(foo) gsub("[^a-zA-Z ]","",foo)

# Using 'tm' to create corpora:
RawCorpus <- Corpus(DirSource(DataPath))

# ...and clean:
CleanCorpus <- RawCorpus
CleanCorpus <- tm_map(CleanCorpus, content_transformer(expandContractions))
CleanCorpus <- tm_map(CleanCorpus, content_transformer(leaveOnlyLetters))
CleanCorpus <- tm_map(CleanCorpus, stripWhitespace)
CleanCorpus <- tm_map(CleanCorpus, content_transformer(tolower))

# Establish lists of words:
first_person_singular_pronouns <- c('i', 'im', 'ive', 'me', 'my', 'myself', 'mine')
first_person_plural_pronouns <- c('we', 'weve', 'us', 'our', 'ours', 'ourselves')
third_person_singular_pronouns <- c('he', 'him', 'himself', 'his', 'she', 'her', 'herself', 'hers', 'it', 'its', 'itself')
third_person_plural_pronouns <- c('they', 'theyd', 'them', 'themselves', 'their', 'theirs')

# Wrap word lists with regex characters to define beginning and end of words, rather than
# parts of other words (e.g. 'wedding' should not be counted as an instance of 'we')
first_person_singular_pronouns <- paste0('\\b', first_person_singular_pronouns, '\\b')
first_person_plural_pronouns <- paste0('\\b', first_person_plural_pronouns, '\\b')
third_person_singular_pronouns <- paste0('\\b', third_person_singular_pronouns, '\\b')
third_person_plural_pronouns <- paste0('\\b', third_person_plural_pronouns, '\\b')
# differentiation_liwc <- paste0('\\b', differentiation_liwc)

# Create a document term matrix and data frame for collecting data:
DTM <- DocumentTermMatrix(CleanCorpus)
DTMasDF <- as.data.frame(as.matrix(DTM))
CollectingDataframe <- data.frame(row.names=rownames(DTMasDF))

# Calculate and store the counts of first- and third-person pronouns in each text corpus,
# as well as a word count:
counter <- 1
for (i in 1:length(CleanCorpus)){
  CollectingDataframe[counter,1]<-length(unlist(strsplit(as.String(CleanCorpus[[counter]]$content), " ")))
  counter <- counter+1
}

# Determine the sentiment and valence values for each word in the combined text corpora,
# which words are the column names in DTMasDF
NRCMatrix <- as.data.frame(get_nrc_sentiment(colnames(DTMasDF)))
rownames(NRCMatrix) <- colnames(DTMasDF)

# Now let's add the additional columns in CollectingDataframe we'll use to log valence and sentiment,
# and add column names (the latter 10 corresponding to the NRC categories):
CollectingDataframe[, 2:11] <- 0
colnames(CollectingDataframe) <- c('word_count', colnames(NRCMatrix))

# And here, the robot runs through each row of DTMasDF, which represents the frequency of words found
# across the entire corpora, and totals the number of times each word appears in each text, per sentiment
# or valence.
counter_1 <- 1
for (i in 1:length(DTMasDF[,1])){
  counter_2 <- 1
  for (i in 1:length(DTMasDF[1,])){
    for (i in names(CollectingDataframe[,2:11])){
      CollectingDataframe[[paste(i)]][counter_1] <-
        CollectingDataframe[[paste(i)]][counter_1] +
        DTMasDF[counter_1,counter_2] * NRCMatrix[[paste(i)]][counter_2]
    }
    counter_2 <- counter_2+1
  }
  counter_1 <- counter_1+1
}

write.xlsx(CollectingDataframe, 'export.xlsx', row.names = TRUE, showNA = FALSE)
