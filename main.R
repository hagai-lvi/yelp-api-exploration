# Based on https://github.com/Yelp/yelp-api/blob/master/v2/r/sample.R

# Yelp API v2.0 code sample.
# 
# This program demonstrates the capability of the Yelp API version 2.0
# by using the Search API to query for businesses by a search term and location.
# 
# Please refer to http://www.yelp.com/developers/documentation for the API documentation.
# 
# This program requires some R libraries including "httr", which you can install via:
# `packages.install("httr")`
# `packages.install("httpuv")`
# etc.
# 
# Sample usage of the program:
# `> source("sample.R")`
# (output to the screen)
# or
# `R CMD BATCH sample.R`
# (output to sample.Rout)

# Required packages
require(httr)
require(httpuv)
require(jsonlite)
require(base64enc)
library(geosphere)
library(plyr)
library(ggplot2)
library(ggmap)
# Number of pages to retrieve from YELP.
# Each page contains 20 rows
NUM_OF_PAGES <- 50

# When you wish to sort the results by distance, use this argument
YELP_SORT_BY_DISTANCE <- 1

# TODO
setwd('/Users/hagai_lvi/tmp/data_scientist/assignment_2')
source('credentials.R')

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

yelp_search <- function(term, location, category_filter, sort=0, offset=0, limit=20) {
  # Search term and location go in the query string.
  path <- "/v2/search/"
  query_args <- list(term=term,
                     location=location,
                     category_filter=category_filter,
                     sort=sort,
                     offset=offset,
                     limit=limit)
  
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

demo <- function() {
  # Query Yelp API, print results.
  yelp_search_result <- yelp_search(term="dinner", location="Boston, MA", limit=3)
  print_search_results(yelp_search_result)
  
  # Pick the top search result, get more info about it.
  # Find Yelp business ID, such as "giacomos-ristorante-boston".
  business_id = content(yelp_search_result)$businesses[[1]]$id
  yelp_business_result <- yelp_business(business_id)
  print_business_results(yelp_business_result)
}

#demo()
if(! file.exists('data')){
  dir.create('data')
}


if(! file.exists('./data/business.csv')){
businesses = NULL
  for (offset in 0:(NUM_OF_PAGES-1)) {
    yelp_search_result <- yelp_search(term="food", category_filter="food", location="San Francisco, CA", sort=0, offset = offset*20)
    locationdataContent = content(yelp_search_result)
    locationdataList=jsonlite::fromJSON(toJSON(locationdataContent, auto_unbox = TRUE))
    tmp <- locationdataList$businesses
    tmp <- data.frame(tmp$name, tmp$rating, tmp$review_count, tmp$location$coordinate$latitude, tmp$location$coordinate$longitude)
    tmp <- rename(tmp, c("tmp.name"="name", "tmp.rating"="rating", "tmp.review_count"="review_count",
    "tmp.location.coordinate.latitude"="latitude", "tmp.location.coordinate.longitude"="longitude"))
    print(nrow(tmp))
    businesses <- rbind(businesses, tmp)
    write.table(businesses, file='./data/business.csv')
  }
  # Add a distance column that is calculated according to the Pythagorean theorem
  businesses$dist <- sqrt( (abs(businesses$longitude - (-122.4227)))^2 + (abs(businesses$latitude - (37.7770)))^2 )
} else{
  businesses <- read.csv('./data/business.csv', header = TRUE)
}

print('Features: ')
print(names(businesses))
with(businesses, plot(rating, review_count))
with(businesses, plot(dist, rating))

with(businesses, plot(dist, review_count))
with(businesses, abline(lm(review_count ~ dist)) )

hist(businesses$dist*1000, main = 'Histogram of amount of restaurants as\na function of distance from the center', xlab = 'Distance(miles)')

# Show all the restaurants on a map:
map <- get_map(location = c(lon = -122.4250, lat = 37.7550), zoom = 12, maptype = "hybrid", scale = 2)
ggmap(map) + geom_point(aes(x=longitude, y=latitude), data=businesses)

# A Heat-map that shows the mose "dense" areas
ggmap(map) + geom_density2d(aes(x=longitude, y=latitude), data = businesses) + stat_density2d(data=businesses,aes(x=longitude, y=latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)


