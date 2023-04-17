# load necessary packages
library(tm)
library(topicmodels)
library(ldatuning)
library(ggplot2)
library(wordcloud)
library(reshape2)
library(LDAvis)

# read in your corpus
# read format is such that each document is a txt file in a folder
docs <- Corpus(DirSource("/Users/marykatekoch/R/WalkthroughTexts"))
#check your corpus object
docs #35 documents in corpus


# preprocess corpus
toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
processedDocs <- tm_map(docs, toSpace, "\n")

## remove punctation
processedDocs <- tm_map(processedDocs, removePunctuation, preserve_intra_word_dashes = TRUE)

## remove numbers
processedDocs <- tm_map(processedDocs, removeNumbers)

## lower case the text
processedDocs <- tm_map(processedDocs, content_transformer(tolower))

## remove stopwords
processedDocs <- tm_map(processedDocs, removeWords, stopwords("english"))

## stem words
#processedDocs <- tm_map(processedDocs, stemDocument, language = "en")

## remove any white space (formatting issue)
#processedDocs <- tm_map(processedDocs, stripWhitespace)


# create document-term matrix from processed corpus
dtm <- DocumentTermMatrix(processedDocs, control = list(bounds = list(wordLengths=c(3, 20))))
# look at the number of documents and terms in the matrix
dim(dtm) 

# sparsity refers to the percentage of docs a term appears in
dtm = removeSparseTerms(dtm, 0.98)
dtm
dim(dtm) 

# determine number of topics using density-based statistic - this will take a long time to run
# CaoJuan2009 = adaptive density-based statistic for LDA (Cao, Xia, Li, Zhang, & Tang, 2009)
optimal.topics <- FindTopicsNumber(dtm, topics = 2:20, metrics = "CaoJuan2009", 
                                   method = "Gibbs")
# plot values to analyze optimal topic number
FindTopicsNumber_plot(optimal.topics)

# selcect number of topics
k <- 15 

# set random number generator seed - independent intialization
set.seed(6547)

# compute the LDA model, inference via 1000 iterations of Gibbs sampling
topicModel <- LDA(dtm, k, method="Gibbs", control=list(iter = 1000))

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

# investigate topic probabilities data.frame
summary(topicProbabilities)

# further look at the posterior distributions
tmResult <- posterior(topicModel)

# format of the resulting object
attributes(tmResult)

beta <- tmResult$terms
theta <- tmResult$topics

# concatenate 5 most likely terms of each topic to a string that represents pseudo-name for each topic
top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
topicNames

# visualize topics as word cloud
topicToViz <- 2 # change for your own topic of interest
topicToViz <- grep('party', topicNames)[1] # Or select a topic by a term contained in its name
# select to 40 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:40]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

# look at distribution of topics within individual documents
exampleIds <- c(2, 15, 22)
lapply(docs[exampleIds], as.character)

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
bm_topics <- tidy(topicModel, matrix = "beta")
bm_topics

# arrange topics by top 5 terms and their beta weight
bm_top_terms <- bm_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# plot topics by terms
bm_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

## Visualize sentiment analysis of the document term matrix
# create tidy version of our document term matrix
bm_dtm <- tidy(dtm)

# load bing sentiment dictionary values for each word
bm_sentiments <- bm_dtm %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))
# check output
bm_sentiments

# arrange words by sentiment
bm_sentiments %>%
  count(document, sentiment, wt = count) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  arrange(sentiment)

# plot terms by positive/negative sentiment contribution
bm_sentiments %>%
  count(sentiment, term, wt = count) %>%
  filter(n >= 50) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ylab("Contribution to sentiment") +
  ggtitle("Sentiment across all episodes")
