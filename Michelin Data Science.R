# Required packages
require(httr)
require(httpuv)
require(jsonlite)
require(base64enc)
library(dplyr)
library(stringr)
library(rvest)
require(tm)

yelp_query <- function(path, query_args) {
  # Use OAuth to authorize your request.
  myapp <- oauth_app("YELP", key=consumerKey, secret=consumerSecret)
  sig <- sign_oauth1.0(myapp, token=token, token_secret=token_secret)
  
  # Build Yelp API URL.
  scheme <- "https"
  host <- "api.yelp.com"
  yelpurl <- paste0(scheme, "://", host, path)
  
  # Make request.
  results <- GET(yelpurl, sig, query=query_args)
  
  # If status is not success, print some debugging output.
  HTTP_SUCCESS <- 200
  if (results$status != HTTP_SUCCESS) {
    print(results)
  }
  return(results)
}

yelp_search <- function(term, location, limit=10) {
  # Search term and location go in the query string.
  path <- "/v2/search/"
  query_args <- list(term=term, location=location, limit=limit)
  
  # Make request.
  results <- yelp_query(path, query_args)
  return(results)
}

yelp_business <- function(business_id) {
  # Business ID goes in the path.
  path <- paste0("/v2/business/", business_id)
  query_args <- list()
  
  # Make request.
  results <- yelp_query(path, query_args)
  return(results)
}

print_search_results <- function(yelp_search_result) {
  print("=== Search Results ===")
  # Load data.  Flip it around to get an easy-to-handle list.
  locationdataContent = content(yelp_search_result)
  locationdataList=jsonlite::fromJSON(toJSON(locationdataContent))
  
  # Print output.
  print(head(data.frame(locationdataList)))
}

print_business_results <- function(yelp_business_result) {
  print("=== Business ===")
  print(content(yelp_business_result))
}

mround <- function(x,base){ 
  base*round(x/base) 
} 



get_all_yelp <- function(restaurant, location) {


  yelp_search_result <- yelp_search(term=restaurant, location=location, limit=1)  
 yelp_data<-content(yelp_search_result)

 nreviews<-mround(yelp_data$businesses[[1]]$review_count, 20)-20
 
  url_end<-yelp_data$businesses[[1]]$id
   initial_url<-paste0("https://www.yelp.com/biz/", url_end, "?start=")  
urls<-paste0(initial_url, seq(0, nreviews, by=20))

   
review_text<-list()
for (i  in 1:length(urls)){
  url<-urls[i]
  h <- read_html(url)
  tmp <- h %>%
    html_nodes(".review-content") %>%
    html_text()
  review_text[[i]]<-tmp
}
  restaurant_data<-list()
  
  restaurant_data$id<-yelp_data$businesses[[1]]$id

  restaurant_data$rating<-yelp_data$businesses[[1]]$rating
  
  price_url<-paste0(initial_url, 0)
  
  h <- read_html(price_url)
  price <- h %>%
    html_nodes(".price-range") %>%
    html_text()
  
  restaurant_data$price<-nchar(price[[1]])
  
  restaurant_data$reviews<-unlist(review_text)
  
  return(restaurant_data)
}





  
