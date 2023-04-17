# load necessary packages
library(tm)
library(topicmodels)
library(ldatuning)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(LDAvis)


# read in your corpus according to the location its in
# read format is such that each document is a txt file in a folder
#docs <- Corpus(DirSource("/Users/marykatekoch/R/WalkthroughTexts"))
docs <- Corpus(DirSource("~/Desktop/WalkthroughTexts"))
# how many documents are in your corpus?
docs


# preprocess corpus
## remove punctuation
processedDocs <- tm_map(docs, removePunctuation, preserve_intra_word_dashes = TRUE)

## remove numbers
processedDocs <- tm_map(processedDocs, removeNumbers)

## lower case the text
processedDocs <- tm_map(processedDocs, content_transformer(tolower))

## remove stopwords
processedDocs <- tm_map(processedDocs, removeWords, stopwords("english"))

## stem words
processedDocs <- tm_map(processedDocs, stemDocument, language = "en")

## remove any white space (formatting issue)
processedDocs <- tm_map(processedDocs, stripWhitespace)


# create document-term matrix from processed corpus
dtm <- DocumentTermMatrix(processedDocs, control = list(bounds = list(wordLengths=c(3, 20))))
# look at the number of documents and terms in the matrix
dim(dtm) 

# sparsity refers to the percentage of docs a term appears in
# what happens if you adjust sparsity to 50%? 99% 5%?
dtm = removeSparseTerms(dtm, 0.95)
dtm
dim(dtm) 

# inspect most frequent terms
freqr <- colSums(as.matrix())
length()
#create sort order (asc)
ordr <- order(,decreasing=TRUE)
#inspect most frequently occurring terms
[head(ordr)]
#inspect least frequently occurring terms
[tail(ordr)]
#return all terms that occur more than K times, returned alphabetically 
findFreqTerms(,lowfreq=100)

# what do the most frequent terms tell us about the data?



# selcect number of topics
# what does a model with 2 topics look like? 15? 35?
k <- 

# set random number generator seed - independent intialization
# run the model with three different seeds - do the topics change or stay the same?
set.seed() 
#1
#2
#3 

# compute the LDA model, inference via 500 iterations of Gibbs sampling
topicModel <- LDA(dtm, k, method="Gibbs", control=list(iter = 500))

#view the top 10 terms for each of the topics
terms(topicModel,10)

# examine the topic assignment for each document
topics(topicModel)

## visualize the LDA model using LDAvis
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(topicModel))

# what topcis seems to cluster together? any words that seem especially relevant?


# run the topic model again but this time eliminate some of the names that dominant topic output
#define and eliminate all custom stopwords
myStopwords <- c("", "") 
processedDocs <- tm_map(processedDocs, removeWords, myStopwords)
# create document-term matrix from processed corpus
dtm <- DocumentTermMatrix(processedDocs, control = list(bounds = list(wordLengths=c(3, 20))))
# sparsity refers to the percentage of docs a term appears in
# what happens if you adjust sparsity to 50%? 99% 5%?
dtm = removeSparseTerms(dtm, 0.0)
dtm
dim(dtm) 
# selcect number of topics
k <- 
# set random number generator seed - independent intialization
set.seed() 
# compute the LDA model, inference via 500 iterations of Gibbs sampling
topicModel <- LDA(dtm, k, method="Gibbs", control=list(iter = 500))
#view the top 10 terms for each of the topics
terms(topicModel,10)
# examine the topic assignment for each document
topics(topicModel)



#write out results to csv files
#docs to topics
topicModel.topics <- as.matrix(topics(topicModel))
write.csv(ltopicModel.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

# top K terms in each topic
topicModel.terms <- as.matrix(terms(topicModel,10))
topicModel.terms
write.csv(topicModel.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(topicModel@gamma)
write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))


# take a look at the resultant csv files # do any trends or patterns pop out?


# investigate topic probabilities data.frame
summary(topicProbabilities)

# further look at the posterior distributions
tmResult <- posterior(topicModel)

beta <- tmResult$terms
theta <- tmResult$topics

# concatenate 5 most likely terms of each topic to a string that represents pseudo-name for each topic
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
topicNames

# visualize topics as word cloud
topicToViz <-  # change for your own topic of interest
topicToViz <- grep('', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

# look at distribution of topics within individual documents
# play around with different IDs to see which topcis are represented across books
exampleIds <- c(, , )

#this will print the whole book so maybe don't do this
#lapply(docs[exampleIds], as.character)


N <- length(exampleIds)
# get topic proportions from example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)




# tidyverse analsyses of our text
library(tidytext)
library(tidyr)
library(dplyr)

## Visualize topic results from LDA by beta
# create tidy version of our topic model results


# arrange topics by top 5 terms and their beta weight
top_terms <-  %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# plot topics by terms
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# do any trends or patterns pop out?

## Visualize sentiment analysis of the document term matrix
# create tidy version of our document term matrix


# load bing sentiment dictionary values for each word
sentiments <-  %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
# check output
sentiments

# arrange words by sentiment
sentiments %>%
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)

# plot terms by positive/negative sentiment contribution
sentiments %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 50) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("") +
  ggtitle("")


