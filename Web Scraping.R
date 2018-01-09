############ Part 1 : Web scraping through API ################

library("httr")
yelp_login <- read.csv("yelp_token.csv", stringsAsFactors = FALSE) 
# First we specify our client ID/consumer key and secret
yelpAPI <- oauth_app("YELP", key = yelp_login$consumer_key, secret = yelp_login$consumer_secret)
str(yelpAPI)
# Then we specify the access token that provides us with access to data limited by the user specification and time
signature <- sign_oauth1.0(yelpAPI, token = yelp_login$token, token_secret = yelp_login$token_secret)
##specify user agent in case server owner wants to contact
userAgent <- user_agent("Humboldt-Unversity student")
searchURL <- "http://api.yelp.com/v2/search/?"
location <- "Paris"
term <- "food"
limit <- 40 # 40 is the maximum number of results (see documentation)
searchRequest <- paste0(searchURL,"limit=",limit,"&term=",term,"&location=", location)
# Next we send the constructed GET request including our authentication and user agent to the Yelp server.
response <- GET(searchRequest, signature, userAgent)
str(response, 1)
str(response$content)
## Handling the returned content 
response_content <- content(response, "text")
str(response_content, 1)
# information is there but in JSON format which we will transform to a format that R can work with, using library "jsonlite"
library("jsonlite")
yelp <- fromJSON(response_content, simplifyVector = TRUE, flatten = TRUE)
str(yelp, 2)
# The JSON data is now saved as a list with the top elements : region, total, and *businesses*
business <- yelp$businesses
str(business, 1)
# Unfortunately, we didn't get any reviews. Another look at the API documentation
# at https://www.yelp.de/developers/documentation/v2/search_api reveals
# that up only one review per restaurant can be extracted via yet another API,
# the busines API if we know the business ID!

# Test it for one business

id <- business$id[[1]]
reviewURL <- "https://api.yelp.com/v2/business/"
reviewRequest <- paste0(reviewURL,id)
response <- GET(reviewRequest, signature, userAgent)
response_content <- content(response, "text")
reviews <- fromJSON(response_content, simplifyVector = TRUE, flatten = TRUE)

str(reviews,1)
reviews$reviews

# This gives us only a snippet of one review, not much to go on for text mining...

############ Part 2 : Web scraping with crawler ################
library(rvest)

ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_9_3) AppleWebKit/537.75.14 (KHTML, like Gecko) Version/7.0.3 Safari/7046A194A"

### Collecting the restaurant web page links ####
Parisiens <- list()
steps <- seq(0, 100, by = 10)
# Randomize the order in which we go through the list to appear less 'robotic'
steps <- sample(steps)

for(i in seq_along(steps)){
  
  url <- paste0("https://www.yelp.com/search?find_loc=Paris,+France&start=", steps[i])
  
  # We use GET and read_html to download and process the website
  webpage <- GET(url[1], 
                 config = add_headers(user_header = ua, 
                                      Accept_Language = 'en-US', 
                                      Accept_Encoding = "gzip, deflate, sdch, br",
                                      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"))
  content <- read_html(webpage)
  # html_nodes extracts more than a single node (unlike html_node) as a vector
  # Here, we look for all elements with the custom 'data-analytics-label' of value
  # 'biz-name' and extract from these nodes the value for the tag 'href', i.e. the link
  # We get this information from using 'Inspect element' in the browser to look at the html code
  # behind the web page
  xml_node_href <- html_nodes(content, xpath = "//*[@data-analytics-label='biz-name']/@href")
  # Note here that, like many functions in R, html_text is vectorized
  # This means we can pass a vector of nodes without any loop and will 
  # get a vector of html text in return
  restaurant_links <- html_text(xml_node_href)
  Parisiens[[i]] <- restaurant_links
  
  # Pause for a second to not overuse the Yelp server (and get blocked)
  Sys.sleep(1)
}

# Process our results
Parisiens <- unlist(Parisiens)
Parisiens

### Extracting restaurant-specific data
ua <- "Mozilla/5.0 (Windows; Windows NT 6.1; rv:2.0b2) Gecko/20100720 Firefox/4.0b2"
### Collecting the restaurant web page links ####
ParisiensReviews <- list()
ParisiensRatings <- list()
"https://www.yelp.com/", 
# Randomize the order in which we go through the list to appear less 'robotic'
ParisiensLinks <- sample(Parisiens)[1:10]
for(i in seq_along(business$url)){
  
  url <- paste0(business$url[i])
  
  # We use GET and read_html to download and process the website
  webpage <- GET(url[1], 
                 config = add_headers(user_header = ua, 
                                      Accept_Language = 'en-US', 
                                      Accept_Encoding = "gzip, deflate, sdch, br",
                                      Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"))
  content <- read_html(webpage)
  
  ## Get the reviews
  # We get this information from using 'Inspect element' in the browser to look at the html code
  # behind the web page
  xml_node_review <- html_nodes(content, xpath = "//*[@class = 'review-content']/p")
  restaurant_reviews <- html_text(xml_node_review)
  ParisiensReviews[[i]] <- restaurant_reviews
  ## Get the rating
  # This is a bit tricky, since the rating is displayed as an image
  # We can get the rating as part of the alt text of the image
  xml_node_rating <- html_nodes(content, xpath = "//*[@class = 'biz-rating biz-rating-large clearfix']//*[contains(./@alt,'rating')]/@alt")
  restaurant_rating <- html_text(xml_node_rating)
  ParisiensRatings[[i]] <- restaurant_rating
  
  # Pause for a second to not overuse the Yelp server (and get blocked)
  Sys.sleep(1)
}

# Let's look at the fruits of our labor! 
ParisiensRatings[[1]]
ParisiensReviews[[1]]

