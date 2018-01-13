############ Part 1 : Web scraping à travers YELP API ################

library("httr")
yelp_login <- read.csv("yelp_token.csv", stringsAsFactors = FALSE) 
# D'abord, nous spécifions notre ID client / clé de consommateur et secret
yelpAPI <- oauth_app("YELP", key = yelp_login$consumer_key, secret = yelp_login$consumer_secret)
str(yelpAPI)
# Ensuite, nous spécifions le jeton d'accès qui nous donne accès aux données limitées par la spécification de l'utilisateur et l'heure
signature <- sign_oauth1.0(yelpAPI, token = yelp_login$token, token_secret = yelp_login$token_secret)
## Indiquer l'agent utilisateur au cas où le propriétaire du serveur veut contacter
userAgent <- user_agent("Humboldt-Unversity student")
searchURL <- "http://api.yelp.com/v2/search/?"
location <- "Paris"
term <- "food"
limit <- 40 # 40 est le  nombre maximum de résultats (selon documentation)
searchRequest <- paste0(searchURL,"limit=",limit,"&term=",term,"&location=", location)
# Ensuite, nous envoyons la requête GET construite, y compris notre authentification et notre agent utilisateur, au serveur Yelp.
response <- GET(searchRequest, signature, userAgent)
str(response, 1)
str(response$content)
## Gérer le contenu renvoyé
response_content <- content(response, "text")
str(response_content, 1)
# information est là mais au format JSON que nous allons transformer en un format avec lequel R peut travailler, en utilisant la librairie "jsonlite"
library("jsonlite")
yelp <- fromJSON(response_content, simplifyVector = TRUE, flatten = TRUE)
str(yelp, 2)
# Les données JSON sont maintenant enregistrées sous forme de liste avec les éléments supérieurs: région, total et * businesses *
business <- yelp$businesses
str(business, 1)
# Malheureusement, nous n'avons reçu aucune critique.
# Une revue par restaurant peut être extraite via une autre API.

# Test pour un seul restaurant

id <- business$id[[1]]
reviewURL <- "https://api.yelp.com/v2/business/"
reviewRequest <- paste0(reviewURL,id)
response <- GET(reviewRequest, signature, userAgent)
response_content <- content(response, "text")
reviews <- fromJSON(response_content, simplifyVector = TRUE, flatten = TRUE)

str(reviews,1)
reviews$reviews

# Cela nous donne seulement un extrait d'une critique, pas grand-chose à faire pour l'exploration de texte ...

############ Part 2 : Web scraping avec R crawler ################
library(rvest)

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/7.0.3 Safari/7046A194A"

### Collecte des liens de la page web du restaurant ####
Parisiens <- list()
steps <- seq(0, 100, by = 10)
# Randomiser l'ordre dans lequel nous passons par la liste pour apparaître moins «robotique»
steps <- sample(steps)

for(i in seq_along(steps)){
  
  url <- paste0("https://www.yelp.com/search?find_loc=Paris,+France&start=", steps[i])
  
  # Nous utilisons GET et read_html pour télécharger et traiter le site web
  webpage <- GET(url[1], 
                 config = add_headers(user_header = ua, 
                                      Accept_Language = 'en-US', 
                                      Accept_Encoding = "gzip, deflate, sdch, br",
                                      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"))
  content <- read_html(webpage)
  xml_node_href <- html_nodes(content, xpath = "//*[@data-analytics-label='biz-name']/@href")
  restaurant_links <- html_text(xml_node_href)
  Parisiens[[i]] <- restaurant_links
  
  # Pause d'une seconde pour ne pas abuser du serveur Yelp (et être bloqué)
  Sys.sleep(1)
}

#Traiter nos résultats
Parisiens <- unlist(Parisiens)
Parisiens

### Extraction de données spécifiques au restaurant
ua <- "Mozilla/5.0 (Windows; Windows NT 6.1; rv:2.0b2) Gecko/20100720 Firefox/4.0b2"
ParisiensReviews <- list()
ParisiensRatings <- list()
for(i in seq_along(Parisiens)){
  
  url <- paste0("https://www.yelp.com/", Parisiens[i])
  
# Nous utilisons GET et read_html pour télécharger et traiter le site web
  webpage <- GET(url[1], 
                 config = add_headers(user_header = ua, 
                                      Accept_Language = 'en-US', 
                                      Accept_Encoding = "gzip, deflate, sdch, br",
                                      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"))
  content <- read_html(webpage)
  
  xml_node_review <- html_nodes(content, xpath = "//*[@class = 'review-content']/p")
  restaurant_reviews <- html_text(xml_node_review)
  ParisiensReviews[[i]] <- restaurant_reviews
 ## Obtenir la note
   # Ceci est un peu délicat, car la note est affichée en tant qu'image
   # Nous pouvons obtenir la note dans le cadre du texte alt de l'image
  xml_node_rating <- html_nodes(content, xpath = "//*[@class = 'biz-rating biz-rating-large clearfix']//*[contains(./@alt,'rating')]/@alt")
  restaurant_rating <- html_text(xml_node_rating)
  ParisiensRatings[[i]] <- restaurant_rating
  
  Sys.sleep(1)
}

# Regardons les notes et les critiques de premier lien de restaurant:
ParisiensRatings[[1]]
ParisiensReviews[[1]]

