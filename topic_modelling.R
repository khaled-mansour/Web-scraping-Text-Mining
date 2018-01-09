####################### Part 4:  topic modelling ########################
library(tm)
reviews <- readRDS("./yelp_dtm.rds")
inspect(reviews[1:5, 1:10])

# Nombre de documents vides dans la matrice
rowTotals <- apply(reviews, 1, sum) #Trouver la somme des mots dans chaque document
sum(rowTotals == 0) # Il y a une revue dans le corpus qui ne contient aucune freq. terme
reviews   <- reviews[rowTotals > 0, ]  

install.packages("topicmodels")
library(topicmodels)
# adapter le modèle
k <- 20
fit <- LDA(reviews, k, method = "Gibbs", control = list(seed = 123, verbose = 250, burnin = 500, iter = 500))

# resultats
terms(fit, 10)
topics(fit)[1:10]
ldaOut.topics <- as.matrix(topics(fit))


## Fonctionnalité supplémentaire
# Il y a deux paquets supplémentaires qui fournissent des fonctions auxiliaires pratiques. ** Ldatuning ** nous aide à trouver un bon nombre de sujets pour décrire les données. ** LDAvis ** fournit une interface dynamique pour explorer les sujets.

#### Topic models
install.packages("ldatuning")
library(ldatuning)
# ldatuning pour trouver le nombre optimal des sujets "topics"
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

#### Visualisation des "topics"
install.packages("LDAvis")
library(LDAvis)

# Trouver les quantités requises
phi <- as.matrix(posterior(fit)$terms) # Le mélange sujet-mot
theta <- as.matrix(posterior(fit)$topics) # Le mélange document-sujet

# Convertir en json pour afficher dynamiquement avec Shiny
fit_json <- LDAvis::createJSON(phi = phi, theta = theta,
                               vocab = colnames(phi), # les terms uniques
                               doc.length = rowSums(as.matrix(reviews)),# Le nombre de mots dans chaque document
                               term.frequency = colSums(as.matrix(reviews)) # La fréquence global de chaque terme
)

# Lancer json avec shiny
serVis(fit_json)
