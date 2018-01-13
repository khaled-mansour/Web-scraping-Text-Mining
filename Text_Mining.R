################ Part 3 : Text Mining #################
#on continue à travailler avec les données de Yelp, en particulier avec les données que Yelp a partagées dans le cadre d'un concours de recherche. 
#Les données ont été téléchargées depuis https://www.yelp.de/dataset_challenge au format json.
#on va traiter seulement un sous-ensemble, car l'ensemble des données est énorme.
# Puisque les données sont sauvegardées sous JSON format , on va utiliser le package "jsonlite" encore une fois.
library(jsonlite)
# On va utlisier la commande stream_in() au lieu de fromJSON() pour lire les données.
connection_bus <- file("businesses.txt")
businesses <- stream_in(connection_bus)
str(businesses,1)
connection_reviews <- file("reviews.txt")
reviews <- stream_in(connection_reviews)
str(reviews, 1)
head(reviews$text, 2)
head(reviews$stars, 2)

### Afin de procéder avec le text mining , on va utiliser le package tm (text mining) dans R
install.packages("tm")
library(tm)
reviews_source <- VectorSource(reviews$text)
# Après avoir identifier notre vecteur comme source, on peut lire les "reviews" dans un corpus qui contient le text pour chaque document
corpus <- VCorpus(reviews_source)

# Notre objectif maintenant c'est de diviser les texts en petits morceaus sur ce qu'on appelle "tokens" ou jetons.
# mots ou termes, et comptez combien de fois ceux-ci apparaissent dans chaque document.
#Puisque des mots très similaires se présentent sous différentes formes, par ex. Pizza / Pizza / Pizzas / Pizza !
#on va standardiser les éléments de texte avant de compter.
install.packages("SnowballC")
library(SnowballC)
#Transformer toutes les lettres en minuscules
#La fonction tm_map applique l'argument de la fonction à tous les documents du corpus
corpus <- tm_map(corpus, content_transformer(tolower))
# Supprimer tous les caractères de ponctuation
replaceCharacter <- content_transformer(function(x, pattern, replacement)
    gsub(pattern = pattern,replacement = replacement, x))
corpus <- tm_map(corpus, replaceCharacter, "-", "")
corpus <- tm_map(corpus, replaceCharacter, "[[:punct:]]", " ")
# Ici on a pris la décision de séparer "pizza-place" à "pizza" et "place" mais en par contre "wouldn't" devient "wouldnt"

# Réduisez tous les espaces à un et supprimez les sauts de ligne, etc.
corpus <- tm_map(corpus, stripWhitespace)
# Supprimer les mots sans contenu sémantique, comme «and» ou «it»
# les "stopwords" sont spécifiques à la langue
head(stopwords("english"), 10)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
# Réduire tous les mots à leur " word stem"
corpus <- tm_map(corpus, stemDocument, "english")
# Puisque on a décidé de choisir un dictionnaire anglais , on va négliger le faite qu'il y a des reviews ou commentaires en français ou en allemand

# on peut vérifier maintenant le contenu de chaque document de la façon suivante: 
corpus[[1]]$content

## Donner une structure à des données non structurées

#Dans un format de table, les colonnes sont toutes des termes qui apparaissent dans n'importe quel document, les lignes sont remplies par la fréquence à laquelle chaque terme apparaît dans chacun des documents (et inversement pour la matrice document-terme).

```{r}
# Nous pouvons maintenant calculer la matrice du terme de document
# Pendant le processus, nous ignorons les termes qui se produisent dans moins de cinq avis
# parce que nous nous attendons à ce qu'ils ne soient pertinents que pour un petit nombre d'observations
corpus <- tm_map(corpus,content_transformer(removePunctuation))
corpus <- tm_map(corpus,content_transformer(stripWhitespace))
corpus <- tm_map(corpus,content_transformer(removePunctuation))
dtm <- DocumentTermMatrix(corpus)
## on peut visualiser les mots les plus fréquents qui ont été utilisés au moins 100 fois
findFreqTerms(dtm, lowfreq = 200, highfreq = Inf)

# on peut additionner la fréquence des termes sur tous les documents
# et jetez un oeil aux termes les plus fréquents
head(sort(colSums(as.matrix(dtm)), decreasing = TRUE), 20)
freq <- as.data.frame(head(sort(colSums(as.matrix(dtm)), decreasing = TRUE), 20))
library(xlsx)
write.xlsx(freq, "frequency.xlsx")
saveRDS(dtm, "yelp_dtm.rds")

# Nous n'avons pas besoin de restreindre l'analyse à des 'mots' uniques
# Calculer des n-grammes, c'est-à-dire des termes avec plus d'un jeton
# Le coeur de cette fonction est la fonction ngrams, qui déplace un n-sized
# filtre sur le document et sort les mots qui apparaissent ensemble
# Cette définition de la fonction provient de la documentation de tm
# elle combine la liste des ngrams de ngrams () et les mots simples

BigramTokenizer <-  function(x){
  wordVec <- words(x)
  bigramVec <- unlist(lapply(ngrams(wordVec, 2), paste, collapse = " "), use.names = FALSE)
  return(c(wordVec, bigramVec))
}

# on va l'appliquer lors de la construction de la matrice de termes de document via l'option 'tokenize'.
# Cette fois, nous appliquerons également une pondération de «fréquence de document inverse» au
# matrice de fréquence des mots. En multipliant la fréquence dans le document par son total inverse
# ratio de fréquence, nous mettons plus de poids sur des termes qui ne sont pas très communs. Intuitivement,
# les termes courants ne nous donnent pas beaucoup d'informations sur un document

tfIdf <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(8, Inf)),
                                                tokenize = BigramTokenizer,
                                                weighting = function(x) weightTfIdf(x, normalize = TRUE)))

# on reçoit un message d'avertissement indiquant qu'un document semble vide
corpus[[1537]]$content
# En effet, la critique n'est pas en anglais et ne contient pas de mots qui apparaissent dans d'autres commentaires

# Regardons le début de la liste complète des termes
head(tfIdf$dimnames$Terms, 50)

# Il est courant de réduire le nombre de jetons rares dans la matrice
# Suppression des termes les plus rares 1. augmente la vitesse de calcul et
# 2. met l'accent sur les mots qui apparaîtront probablement dans les futurs "reviews"
# La densité d'une matrice ou d'un vecteur mesure le pourcentage de
# vide (0) entrées. Si nous voulons utiliser seulement les termes qui apparaissent dans
# au moins 2% des commentaires (ici 40 commentaires sur 2000), nous devrions
# définir le taux de "sparsity" maximal à 98%
dim(tfIdf)
tfIdf<- removeSparseTerms(tfIdf, sparse = 0.98)
dim(tfIdf)

# To see if some 2-grams have made it through the selection
# process, since they are by definition rarer than their single word
# counterparts, we use grep to search the terms for a space " "
# Pour voir si quelques 2-grams ont réussi le processus de sélection
# puisqu'ils sont par définition plus rares que leur seul mot
# homologues, on utilise grep pour rechercher les termes d'un espace ""

grep(tfIdf$dimnames$Terms, " ", value = TRUE)

# Nous sauvegardons également la matrice tfIdf et l'utiliserons plus tard pour l'analyse des sentiments.
saveRDS(tfIdf, "yelp_tfidf.rds")

## Créer un word cloud
#Wordclouds sont une belle technique de visualisation pour montrer la fréquence des mots et leur classe définie ou connotation d'une belle manière.
#Le package ** wordcloud ** fournit des heuristiques utiles sur quels mots placer où et dans quelle taille et tracons très vite des nuages décents.

#Pour faire des sommes, etc., nous avons besoin d'une matrice R standard
dtm <- as.matrix(dtm)

# Afin de créer des visualisations, on va travailler avec une table qui contient les mots et les informations.
wordTable <- data.frame(words = colnames(dtm))
# Les nuages de mots sont basés sur la fréquence des termes, donc on va les calculer
wordTable$frequency <- colSums(dtm)
summary(wordTable$frequency)

# Plot un simple wordcloud
install.packages("wordcloud")
library(wordcloud)
wordcloud(wordTable$words, wordTable$frequency, max.words = 50) 

# La fréquence brute des mots favorise les mots comme «place» ou «great» ou "like"
# Un wordcloud plus intéressant pourrait être basé sur l'information de notation qu'on dans les données
ratings <- reviews$stars
summary(ratings)

# on calcule à nouveau la fréquence, mais cette fois uniquement sur la base sur le sous-ensemble des avis positifs ou négatifs
# on fait cela en sélectionnant uniquement les lignes dans lesquelles il y a des critiques avec la note d'intérêt
head(ratings >= 5)

# Résumer la fréquence globale des sous-ensembles

wordTable$frequencyBad <- colSums(dtm[ratings <= 2,])
wordTable$frequencyNeutral <- colSums(dtm[ratings == 3 | ratings == 4,])
wordTable$frequencyGood<- colSums(dtm[ratings >= 5,])
head(wordTable, 10)

# Maintenant, on fait un autre nuage pour les commentaires positifs seulement
wordcloud(wordTable$words, wordTable$frequencyGood, max.words = 50)
wordcloud(wordTable$words, wordTable$frequencyBad, max.words = 50)

# Certains mots sont intéressants, d'autres apparaissent dans les deux
# Ce serait encore plus utile si on peut voir les mots qui sont fréquents dans les bonnes critiques mais rares dans les mauvaises critiques
# on peut le faire en divisant les chiffres par la fréquence globale des mots

head(wordTable)
wordTable[, c("frequencyBad", "frequencyNeutral", "frequencyGood")] <- lapply(wordTable[, c("frequencyBad", "frequencyNeutral", "frequencyGood")], 
                                                                              function(x) x/wordTable$frequency)
head(wordTable)

# Pour rendre les nuages agréables, nous pouvons ajouter des couleurs dégradées en utilisant le package RColorBrewer
install.packages("RColorBrewer")
library(RColorBrewer)
# on veut une gamme de couleurs, une palette, dans une teinte qui signifie l'aspect positif ou négatif de la critique
greenPalette <- brewer.pal(n = 9, "Greens") # For ces palettes, le max est 9
redPalette <- brewer.pal(n = 9, "Reds")

# et on fait le plot comme avant.
wordcloud(wordTable$words, wordTable$frequencyGood, max.words = 60, colors = greenPalette, scale = c(1.5,0.5))
wordcloud(wordTable$words, wordTable$frequencyBad, max.words = 60, colors = redPalette, scale = c(1.5,0.5))

# Pour condenser l'information complète dans un nuage, on peut utiliser la fonction comparison.cloud ()
# pour tracer les mots autour de leurs étiquettes de classe
# Les tracés de fonction basés sur un bloc de données où les mots sont row.names et les classes column.names

comparisonTable <- data.frame(wordTable[, c("frequencyGood", "frequencyNeutral","frequencyBad")], row.names = wordTable$words)
comparison.cloud(comparisonTable, max.words = 300, colors=c("darkgreen", "blue","red"), title.size = 1, random.order = FALSE, scale = c(1.5, 0.4))
