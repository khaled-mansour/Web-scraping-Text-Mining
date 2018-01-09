####################### Part 4:  topic modelling ########################
library(tm)
# We will work with the term-document matrix that we have
# created for the 2000 Yelp reviews
reviews <- readRDS("./yelp_dtm.rds")

# Have a quick look at reviews to see if everything worked
inspect(reviews[1:5, 1:10])

# Number of empty documents in the matrix
rowTotals <- apply(reviews, 1, sum) #Find the sum of words in each Document
sum(rowTotals == 0) # There is one review in the corpus that doesn't contain any freq. term
reviews   <- reviews[rowTotals > 0, ]  

install.packages("topicmodels")
library(topicmodels)
# fitting the model
k <- 20
fit <- LDA(reviews, k, method = "Gibbs", control = list(seed = 123, verbose = 250, burnin = 500, iter = 500))

# results
terms(fit, 10)
topics(fit)[1:10]
ldaOut.topics <- as.matrix(topics(fit))


## Additional functionality
There are two additional packages that provide convenient helper functions. **ldatuning** helps us to find a good number of topics to describe the data. **LDAvis** provides a dynamic interface to explore the topics.

```{r}
#### Topic models
install.packages("ldatuning")
library(ldatuning)
# ldatuning to find optimal topics number
kTuning <- FindTopicsNumber(
  reviews,
  topics = seq(from = 16, to = 24, by = 4),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 123),
  mc.cores = 3L,
  verbose = TRUE
)
FindTopicsNumber_plot(kTuning)

#### Visualization of topics
install.packages("LDAvis")
library(LDAvis)

# Find required quantities
phi <- as.matrix(posterior(fit)$terms) # The topic-word mixture
theta <- as.matrix(posterior(fit)$topics) # The document-topic mixture

# Convert to json to display dynamically with shiny
fit_json <- LDAvis::createJSON(phi = phi, theta = theta,
                               vocab = colnames(phi), # The unique terms
                               doc.length = rowSums(as.matrix(reviews)), # The number of words in each document
                               term.frequency = colSums(as.matrix(reviews)) # The frequency of each term overall
)

# Load json with shiny
serVis(fit_json)
