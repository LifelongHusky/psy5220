##### LIBRARIES #####
library(dplyr)
library(tidyverse)
library(tm)
library(randomForest)
library(tokenizers)
library(mclust)
library(quanteda)

#save workspace
save.image(file='shinyAppML.RData')


# Custom stop words
ogsw <- stopwords()
nonos <- ogsw[!str_detect(ogsw, 'no')] # filter out 'no' etc
nonos <- nonos[!str_detect(nonos, "n\'t")] # filter out n't
apost.words <- ogsw[grepl("\'", ogsw)]                     # Get all words w/ '
apost.words2 <- apost.words[grepl("'[^t]+$", apost.words)] # filter out n't
apost.removed <- gsub("\'", '', apost.words)
apost.removed2 <- gsub("\'", '', apost.words2)
allsw <- c(ogsw, apost.removed) # for dtm
somesw <- c(nonos, apost.removed2) # for ngrams

rm(ogsw, nonos, apost.words, apost.words2, apost.removed, apost.removed2)


##### HELPER FUNCTIONS #####
clean_data <- function(data, sw=stopwords()){
  data$text <- gsub(' s ', ' ', data$text)
  data %>% 
    mutate(text = text |>
             # turn text into lowercase
             str_to_lower() |> 
             # remove puncuation
             removePunctuation() |>
             # remove stopwords
             tm::removeWords(words = sw) |>
             # reduce repeated whitespace from the text
             str_squish())
}

# Relabel Data (numbers to words to factor)
relabel_data_six <- function(data){
  data |> mutate(label = as.factor(label)) |>
    mutate(label = case_when(label == "0" ~ "sadness",
                             label == "1" ~ "joy",
                             label == "2" ~ "love",
                             label == "3" ~ "anger",
                             label == "4" ~ "fear",
                             label == "5" ~ "surprise")) |>
    mutate(label = as.factor(label))
}

relabel_data_two <- function(data){
  data |> mutate(label = as.factor(label)) |>
    mutate(label = case_when(label == "0" ~ "negative",
                             label == "1" ~ "positive",
                             label == "2" ~ "positive",
                             label == "3" ~ "negative",
                             label == "4" ~ "negative",
                             label == "5" ~ "positive")) |>
    mutate(label = as.factor(label))
}




##### GET DATA #####
train <- read.csv('emotion_train.csv',header=T)[,-1]
test <- read.csv('emotion_test.csv',header=T)[,-1]
val <- read.csv('emotion_val.csv',header=T)[,-1]


##### PREPROCESS TRAINING DATA #####
# Relabel & clean a little
train.prep1 <- train |> relabel_data_two() |> clean_data(sw=allsw)
train.prep2 <- train |> relabel_data_two() |> clean_data(sw=somesw)

test.prep1 <- test |> relabel_data_two() |> clean_data(sw=allsw)


##### TEXT REPRESENTATIONS - DOC TERM MATRIX #####
# Convert to DTM
train.dtm <- DocumentTermMatrix(train.prep1$text)
# Remove some sparse terms
train.dtm <- removeSparseTerms(train.dtm, .99)
# Connect the labels back
train.dtm <- data.frame(lbl=train.prep1$label, as.matrix(train.dtm))
# Keep terms here so it is easier to access later & less memory
train.dtm.terms <- colnames(train.dtm)[-1]


test.dtm <- DocumentTermMatrix(test.prep1$text)
test.dtm <- data.frame(lbl=test.prep1$label, as.matrix(test.dtm))


##### TEXT REPRESENTATIONS - NGRAMS 1:4 #####
# tokenize to have bi, tri, and quadgrams.. etc
train.ngram <- train.prep2$text |> tokens() |> 
  tokens_ngrams(n = 1:4, concatenator = ' ') |> 
  dfm() |> dfm_trim(sparsity = .99)
# Connect labels back
train.ngram <- data.frame(lbl=train.prep2$label, as.matrix(train.ngram))
# Keep terms here so it is easier to access later & less memory
train.ngram.terms <- colnames(train.ngram)[-1]


##### MODELS - Random Forest #####
rf.dtm <- randomForest(lbl~., data=train.dtm)
rf.ngram <- randomForest(lbl~., data=train.ngram)

##### MODELS - MclustDA #####
mcda.dtm <- MclustDA(train.dtm[,-1], train.dtm$lbl, verbose=F)
mcda.ngram <- MclustDA(train.ngram[,-1], train.ngram$lbl, verbose=F)
