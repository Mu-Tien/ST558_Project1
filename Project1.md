ST558 Project 1
================
Mu-Tien, Lee
2020,09.04

-   [Require Package](#require-package)
-   [Data cleaning](#data-cleaning)
    -   [Building functions to reach statsAPI](#building-functions-to-reach-statsapi)
    -   [Building functions to reach recordAPI](#building-functions-to-reach-recordapi)
    -   [accessory for API endpoints](#accessory-for-api-endpoints)
-   [Data analysis](#data-analysis)

Require Package
---------------

``` r
library(rmarkdown)
library(dplyr)
library(tidyverse)
library(RSQLite)
library(bigrquery)
library(httr)
library(jsonlite)
```

Data cleaning
-------------

### Building functions to reach statsAPI

``` r
statsAPI <- function(x,...){
#setting up base url for stats API
base_url <-"https://statsapi.web.nhl.com/api/v1/teams"
modifiers <- x

#construct the full path
full_url <- paste0(base_url, "?", modifiers)

# retrieve information in raw form
GET(full_url)

#transfer into JSON text form
text <- content(GET(full_url),"text")

#convert it to a list
mydata <- fromJSON(text, flatten = TRUE) 
mydata<- as.data.frame(mydata)
return(mydata)
}
m<- statsAPI("expand=team.roster")
n<- statsAPI("expand=person.names")
o<- statsAPI("expand=team.schedule.next")
p<- statsAPI("expand=team.schedule.previous")
q<- statsAPI("expand=team.stats")
r<- statsAPI("expand=team.roster&season=20142015")
s<- statsAPI("teamId=4,5,29")
t<- statsAPI("stats=statsSingleSeasonPlayoffs")
```

### Building functions to reach recordAPI

``` r
recordAPI <- function(x,id=NULL,...){
#setting up base url for stats API
base_url <-"https://records.nhl.com/site/api"
modifiers <- x
if (is.null(id)){
  full_url <- paste0(base_url, "/", modifiers)
  }
else {
  full_url <- paste0(base_url, "/", modifiers,"?cayenneExp=franchiseId=", id)
  } 


# retrieve information in raw form
GET(full_url)

#transfer into JSON text form
text <- content(GET(full_url),"text")

#convert it to a list
mydata <- fromJSON(text, flatten = TRUE) 
mydata<- as.data.frame(mydata)
return(mydata)
}
a<-recordAPI("franchise")
b<-recordAPI("franchise-team-totals")
c<-recordAPI("franchise-season-records",1)
d<-recordAPI("franchise-goalie-records",1)
e<-recordAPI("franchise-skater-records",1)
```

### accessory for API endpoints

sapply()

Data analysis
-------------
