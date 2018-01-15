# Sentiment analysis

library(jsonlite)
connection_reviews <- file("./reviews.txt")
reviews <- stream_in(connection_reviews)
# Extraire les étoiles de notations
ratings <- reviews$stars
reviews <- reviews$text

# Les évaluations sont déséquilibrées dans un modèle typique
# avec quelques mauvaises notes et de nombreux commentaires 4 étoiles
table(ratings)
# Pour égaliser cela et nous assurer que notre modèle ne se concentre pas sur
# seulement la classe la plus commune, nous utilisons une heuristique et le cluster N ° 1, 2 et 3 étoiles ensemble

ratings2 <- cut(ratings, breaks = c(0, 3, 4, 5), labels = c("<3Stars", "4Stars", "5Stars"))
table(ratings2)

## Machine learning avec matrice document-term 

# Chargez la matrice tf-idf que nous avons calculée auparavant
library(tm)
tfIdf <- readRDS("./yelp_tfidf.rds")
tfIdf <- as.matrix(tfIdf)
tfIdf <- data.frame(tfIdf, check.names = TRUE)
str(tfIdf, 0)
tfIdf[1:5, 1:10]

# Nous divisons nos données en deux parties, où nous utilisons un
# partie pour former le modèle et l'autre pour vérifier
# comment il fonctionne pour s'assurer qu'il est robuste et général
library(caret)
set.seed(123)
trainIdx <- createDataPartition(ratings2, p = 0.6, list = FALSE)
head(trainIdx)

### Modèle de prédiction
#Regardons l'arbre de décision pour avoir une intuition
library(rpart)
library(rpart.plot)
dt_fit <- rpart(ratings ~., data = data.frame(ratings = ratings2[trainIdx], tfIdf[trainIdx, ]), control = list(minsplit = 10, cp = 0.01))
rpart.plot(dt_fit)

dt_pred_ts <- predict(dt_fit , newdata = tfIdf[-trainIdx, ], type = "class")
confusionMatrix(dt_pred_ts, reference = ratings2[-trainIdx])$table

#Random Forest
library(randomForest)
rf_fit <- randomForest(y = ratings2[trainIdx], x = tfIdf[trainIdx, ],
                       ntree = 400, nodesize = 5, 
                       mtry = 25, sampsize = c(100, 100, 100)) 

# Le modèle de forêt aléatoire est moins interprétable qu'un arbre de décision unique
# mais nous pouvons obtenir un score d'importance pour chaque variable / mot
varImpPlot(rf_fit, n.var = 20)

# Maintenant, nous pouvons utiliser le modèle pour faire une prédiction pour
# toutes nos observations
pred <- predict(rf_fit, newdata = tfIdf)
pred.prob <- predict(rf_fit, newdata = tfIdf, type = "prob")

# Pour vérifier la performance de ce modèle, nous utilisons uniquement les observations
# nous n'avons pas utilisé pour former la forêt aléatoire

# nous pourrions atteindre une précision d'environ 0,3 par
# assigner les groupes au hasard, donc 0,3 pourrait être un
# benchmark simple pour comparer le modèle 
confusionMatrix(pred[-trainIdx], ratings2[-trainIdx])

# Le modèle semble assez bon pour prédire les très bons restaurants
# Ce sont les endroits que nous voulons recommander! Regardons les cas
# où il met des mauvais endroits dans la catégorie génial.
bad2goodIdx <- which(pred == "5Stars" & ratings2 == "<3Stars")[-trainIdx]
bad2goodIdx
# Regardons les 5 premières notes des restaurants
pred.prob[bad2goodIdx[9:13],]
ratings[bad2goodIdx[9:13]]
# Voir les 5 premières critiques de restaurants
reviews[bad2goodIdx[9:13]]
# Un gros problème sont les phrases sans beaucoup de mots d'indication clairs pour les modèles "bag of words"
# et fautes de frappe pour l'exploration de texte en général

## Vérification des mots dans un lexique d'opinion

install.packages("tidytext")
library(tidytext)
# Les dictionnaires de sentiment sont sauvegardés dans un objet appelé sentiments
head(sentiments, 5)
#Nous voulons le dictionnaire AFINN car il marque un sentiment positif vs négatif sur une échelle non-binaire
head(sentiments[sentiments$lexicon == "AFINN",], 10)
# Recueillir les mots et les scores du lexique AFINN
lexicon <- sentiments[sentiments$lexicon == "AFINN", c("word", "score")]

#Les mots du dictionnaire ne sont pas tronqués, donc on doit ne pas tronquer nos critiques.
#Nous faisons le même corpus que précédemment, mais cette fois, nous le transformons en un bloc de données contenant le mot, le numéro de document et le sentiment.
library(tm)
# Transformez les "reviews" en un corpus
reviews_source <- VectorSource(reviews)
corpus <- SimpleCorpus(reviews_source)
str(corpus[[1]])

# Meme Transformation comme auparavant
corpus <- tm_map(corpus, content_transformer(tolower))
replaceCharacter <- content_transformer(function(x, pattern, replacement)
  gsub(pattern = pattern,replacement = replacement, x))
corpus <- tm_map(corpus, replaceCharacter, "'", "")
corpus <- tm_map(corpus, replaceCharacter, "[[:punct:]]", " ")
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Extrait les mots du corpus
str(corpus[[1]])
# Extraire de chaque élément de liste le premier élément, c'est-à-dire le contenu
documents <- sapply(corpus, function(x) x)
head(documents, 2)
documents <- strsplit(documents, split = " ")
head(documents, 2)

# Le moyen le plus simple de placer la structure de liste dans un cadre de données
# est la fonction melt() du package "reshape"
install.packages("reshape2")
library(reshape2)
documents <- melt(documents)
documents[c(1:3, 47:51, 62:64),]
colnames(documents) <- c("word", "document")

# Nous pouvons utiliser la fusion pour rechercher le sentiment pour chaque mot
documents[["word"]] <- as.character(documents[["word"]]) 
documents <- documents[documents$word != "", ]
documents <- merge(documents, lexicon, by = "word", all.x = TRUE)

# Vérifiez les mots pour lesquels aucun sentiment n'est disponible
sum(is.na(documents$score)) / nrow(documents)
sample(documents$word[is.na(documents$score)], 25)
# Les mots manquants sont difficiles à juger sur leur valeur de sentiment,
# donc nous allons simplement accepter cette restriction

# Calcule du sentiment global par revue
# Nous calculons le sentiment moyen pour les mots disponibles
# pour chaque document séparément
sentiment_scores <- tapply(documents$score, factor(documents$document), function(x) sum(x, na.rm = TRUE) / max(1, sum(!is.na(x))) )
sentiment_scores[7:8]
reviews[7:8]

# la distribution de des scores de sentiment en boxplots pour chaque classement d'étoiles
boxplot(sentiment_scores ~ ratings, data = data.frame(ratings, sentiment_scores), xlab = "Star rating", ylab = "Sentiment score")

# Nous pouvons comparer cela au modèle de forêt aléatoire basé sur tous les mots (communs)
sentiment_categories <- cut(sentiment_scores, breaks = quantile(sentiment_scores, seq(0, 1, 1/3)), labels = c("<3Stars", "4Stars", "5Stars"))
confusionMatrix(sentiment_categories[-trainIdx], ratings2[-trainIdx])
# La forêt aléatoire a une précision globale de 0,54.

# Pour voir pourquoi nous échouons sur certaines critiques, regardons les commentaires avec un
# mauvais sentiment mais une note élevée
head(reviews[sentiment_scores < -1.5 & ratings == 5], 10)
## Pour le dictionnaire anglais, l'allemand se lit comme expression en colère et donne alors un mauvais sentiment !
