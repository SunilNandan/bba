
library(quanteda)
library(stm)
library(tm)
library(NLP)
library(openNLP)
library(ggplot2)
library(ggdendro)
library(cluster)
library(fpc)  

require(quanteda)
#using Corpus from quanteda - US President inaugural speeches
setwd("C:/Users/sunil/Google Drive/Sem 3/Business analytics/Assignments/Assignment 5")

speechdf<- read.csv("trumpVICTORYspeech.csv", 
                  header=TRUE, stringsAsFactors=FALSE)

speechcorpus<- corpus(speechdf$Speech,
                    docnames=NULL,
                    docvar=NULL)
summary(speechcorpus)
#Generate DFM
speechcorpus<- toLower(speechcorpus, keepAcronyms = FALSE) 
cleancorpus <- tokenize(speechcorpus, 
                        removeNumbers=TRUE,  
                        removePunct = TRUE,
                        removeSeparators=TRUE,
                        removeTwitter=FALSE,
                        verbose=TRUE)

#removing stopwords from the corpus
dfm<- dfm(cleancorpus,
          toLower = TRUE, 
          ignoredFeatures =stopwords("english"), 
          verbose=TRUE, 
          stem=TRUE)
# Reviewing top features
topfeatures(dfm, 50)     # displays 50 features


wordlist<- read.csv("wordlist.csv", header = TRUE,stringsAsFactors=FALSE )
names(wordlist)
mydict <- dictionary(list(negative = wordlist$Negative.Words,
                          postive = wordlist$Positive.words))
dfm.sentiment <- dfm(cleancorpus$text3, dictionary = mydict)
topfeatures(dfm.sentiment) 
View(dfm.sentiment) 



#######################################
### Advanced method for Topic Modeling
#######################################


library(dplyr)
require(magrittr)
library(tm)
library(ggplot2)
library(stringr)
library(NLP)
library(openNLP)


#passing Full Text to variable news_2015
news_2015<-speechdf$Speech

#the same script can be run to analyse. pass the required text to news_2015. the script can be used as a package
#Cleaning corpus
stop_words <- stopwords("SMART")
## additional junk words showing up in the data
stop_words <- c(stop_words, "said", "the", "also", "say", "just", "like","for", 
                "us", "can", "may", "now", "year", "according", "mr")
stop_words <- tolower(stop_words)


news_2015 <- gsub("'", "", news_2015) # remove apostrophes
news_2015 <- gsub("[[:punct:]]", " ", news_2015)  # replace punctuation with space
news_2015 <- gsub("[[:cntrl:]]", " ", news_2015)  # replace control characters with space
news_2015 <- gsub("^[[:space:]]+", "", news_2015) # remove whitespace at beginning of documents
news_2015 <- gsub("[[:space:]]+$", "", news_2015) # remove whitespace at end of documents
news_2015 <- gsub("[^a-zA-Z -]", " ", news_2015) # allows only letters
news_2015 <- tolower(news_2015)  # force to lowercase

## get rid of blank docs
news_2015 <- news_2015[news_2015 != ""]

# tokenize on space and output as a list:
doc.list <- strsplit(news_2015, "[[:space:]]+")

# compute the table of terms:
term.table <- table(unlist(doc.list))
term.table <- sort(term.table, decreasing = TRUE)


# remove terms that are stop words or occur fewer than 5 times:
del <- names(term.table) %in% stop_words | term.table < 5
term.table <- term.table[!del]
term.table <- term.table[names(term.table) != ""]
vocab <- names(term.table)

# now put the documents into the format required by the lda package:
get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents <- lapply(doc.list, get.terms)

#############
# Compute some statistics related to the data set:
D <- length(documents)  # number of documents (1)
W <- length(vocab)  # number of terms in the vocab (1741)
doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document [312, 288, 170, 436, 291, ...]
N <- sum(doc.length)  # total number of tokens in the data (56196)
term.frequency <- as.integer(term.table) 

# MCMC and model tuning parameters:
K <- 10
G <- 3000
alpha <- 0.02
eta <- 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 <- Sys.time()
fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
## display runtime
t2 - t1  

theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

news_for_LDA <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

library(LDAvis)
library(servr)

# create the JSON object to feed the visualization:
json <- createJSON(phi = news_for_LDA$phi, 
                   theta = news_for_LDA$theta, 
                   doc.length = news_for_LDA$doc.length, 
                   vocab = news_for_LDA$vocab, 
                   term.frequency = news_for_LDA$term.frequency)

serVis(json, out.dir = 'vis', open.browser = TRUE)

