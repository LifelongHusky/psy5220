##### LIBRARIES #####
library(dplyr)
library(tidyverse)
library(tm)
library(randomForest)
library(tokenizers)
library(mclust)


##### HELPER FUNCTIONS #####
clean_data <- function(data, sw=stopwords()){
  data %>% 
    mutate(text = text |>
             # turn text into lowercase
             str_to_lower() |> 
             # remove stopwords
             tm::removeWords(words = sw) |>
             # reduce repeated whitespace from the text
             str_squish())
}

# Relabel Data (numbers to words to factor)
relabel_data <- function(data){
  data |> mutate(label = as.factor(label)) |>
    mutate(label = case_when(label == "0" ~ "sadness",
                             label == "1" ~ "joy",
                             label == "2" ~ "love",
                             label == "3" ~ "anger",
                             label == "4" ~ "fear",
                             label == "5" ~ "surprise")) |>
    mutate(label = as.factor(label))
}



##### GET DATA #####
train <- read.csv('emotion_train.csv',header=T)[,-1]
test <- read.csv('emotion_test.csv',header=T)[,-1]
val <- read.csv('emotion_val.csv',header=T)[,-1]


##### PREPROCESS TRAINING DATA #####
# Relabel & clean a little
train.prep <- train |> relabel_data()
train.prep$text <- train.prep$text |> str_to_lower() |> removePunctuation()


##### TEXT REPRESENTATIONS - DOC TERM MATRIX #####
# Convert to DTM
train.dtm <- DocumentTermMatrix(train.prep$text)
# Remove some sparse terms
train.dtm <- removeSparseTerms(train.dtm, .99)
# Connect the labels back
train.dtm <- data.frame(emotion = train.prep$label, as.matrix(train.dtm))
# Keep terms here so it is easier to access later & less memory
train.dtm.terms <- colnames(train.dtm)[-1]


##### TEXT REPRESENTATIONS - NGRAMS 2:4 #####
# tokenize to have bi, tri, and quadgrams.. etc
train.ngram <- train.prep$text |> tokens() |> 
  tokens_ngrams(n = 2:4, concatenator = ' ') |> 
  dfm() |> dfm_trim(sparsity = .99)
# Connect labels back
train.ngram <- data.frame(emotion = train.prep$label, as.matrix(train.ngram))
# Keep terms here so it is easier to access later & less memory
train.ngram.terms <- colnames(train.ngram)[-1]


##### MODELS - Random Forest #####
rf.dtm <- randomForest(emotion~., data=train.dtm)
rf.ngram <- randomForest(emotion~., data=train.ngram)

##### MODELS - MclustDA #####
mcda.dtm <- MclustDA(train.dtm[,-1], train.dtm$emotion, verbose=F)
mcda.ngram <- MclustDA(train.ngram[,-1], train.ngram$emotion, verbose=F)