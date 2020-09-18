ST558 Project 1
================
Mu-Tien, Lee
2020,09.04

-   [Require Package](#require-package)
-   [Discription](#discription)
-   [Data cleaning](#data-cleaning)
    -   [Building functions to reach statsAPI](#building-functions-to-reach-statsapi)
    -   [Building functions to reach recordAPI](#building-functions-to-reach-recordapi)
    -   [Accessory for API endpoints](#accessory-for-api-endpoints)
-   [Data analysis](#data-analysis)
    -   [Building data for later use](#building-data-for-later-use)
    -   [Numeric summarize](#numeric-summarize)
        -   [A contingency table (Division\*Fist year of play)](#a-contingency-table-divisionfist-year-of-play)
        -   [Numeric summary table](#numeric-summary-table)
    -   [Plots](#plots)
        -   [A bar plot tells conference and division](#a-bar-plot-tells-conference-and-division)
        -   [Histogram of penalty times](#histogram-of-penalty-times)
-   [Data analysis between 4 selected team](#data-analysis-between-4-selected-team)
    -   [Arrange data for later use](#arrange-data-for-later-use)
    -   [Numerical summary](#numerical-summary)
        -   [Some data to compare between these team](#some-data-to-compare-between-these-team)
        -   [Distribution of skater position for each team](#distribution-of-skater-position-for-each-team)
    -   [Plots](#plots-1)
        -   [Scatter and boxplot](#scatter-and-boxplot)
-   [The End](#the-end)

Require Package
===============

``` r
#install.packages("qwraps2")
library(rmarkdown)
library(dplyr)
library(tidyverse)
library(knitr)
library(RSQLite)
library(bigrquery)
library(httr)
library(jsonlite)
library(ggplot2)
library(qwraps2)
```

Discription
===========

This project are going to talk about some data in NHL the franchise are loaded from <https://gitlab.com/dword4/nhlapi/-/blob/master/records-api.md> and <https://gitlab.com/dword4/nhlapi/-/blob/master/stats-api.md>. You may use `endpoint` function and key in with correct endpoint's name to get whatever table you would like to get. At some point you can assign a specific team by both name and team ID/franchise ID. After that I will show you some data analysis among all active team and some selected team. wish you can have some fun in this project!

Data cleaning
=============

Building functions to reach statsAPI
------------------------------------

``` r
#build up base dataset for switch team.names and team.ID
text <- content(GET("https://statsapi.web.nhl.com/api/v1/teams?expand=team.roster"),"text")
base <- fromJSON(text, flatten = TRUE) 
base <- as.data.frame(base) %>% select(teams.franchise.teamName,teams.franchiseId,teams.teamName,teams.id)

#make a function to let user reach statsAPI
statsAPI <- function(x,teamID=NULL,season=NULL, ...){
  
#setting up base url for stats API
base_url <-"https://statsapi.web.nhl.com/api/v1/teams"
modifiers <- x

#construct the full path
if (x %in% "teamId"){
  full_url <- paste0(base_url, "?", modifiers, "=", teamID)
}
#convert team name into teamID
else if(is.character(teamID)){
  base <- base %>% filter(teams.teamName==teamID)
  teamID <- base[4]
}

if (x %in% c("expand=team.roster","expand=person.names","expand=team.schedule.next","expand=team.schedule.previous","expand=team.stats","stats=statsSingleSeasonPlayoffs")){
  
  if (is.null(teamID)){
    full_url <- paste0(base_url, "?", modifiers)}
  
  else {
    full_url <- paste0(base_url, "/", teamID, "?", modifiers)}
}  
  
else if (x %in% "expand=team.roster&season" ){
  if (is.null(season)){
    stop("seaon is missing")}
  
  else if(length(teamID)>1){
    stop("sorry, we can only show one team each time")}
  
  else if(is.null(teamID)){
    full_url <- paste0(base_url,"?", modifiers,"=", 20142015)}

  else {
    full_url <- paste0(base_url, "/", teamID, "?", modifiers,"=", season)}
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
```

Building functions to reach recordAPI
-------------------------------------

``` r
#make a function to let user reach record API
recordAPI <- function(x,franchiseID=NULL,...){
#setting up base url for stats API
base_url <-"https://records.nhl.com/site/api"
modifiers <- x

#convert team name into teamID
if(is.character(franchiseID)){
  base <- base %>% filter(teams.franchise.teamName==franchiseID)
  franchiseID <- base[2]
  franchiseID
}

#construct the full url path
if (is.null(franchiseID)){
    full_url <- paste0(base_url, "/", modifiers)
  }
else if (x %in% c("franchise","franchise-team-totals")) {
    stop("Sorry,table franchise and franchise-team-totals cannot indicate team")
  }
else {
  full_url <- paste0(base_url, "/", modifiers,"?cayenneExp=franchiseId=", franchiseID)
  } 

#retrieve information in raw form
GET(full_url)
#transfer into JSON text form
text <- content(GET(full_url),"text")
#convert it to a list
mydata <- fromJSON(text, flatten = TRUE) 
mydata<- as.data.frame(mydata)
return(mydata)
}
```

Below is a function that can help you read whatever the endpoint you would like to get, please key in the following option in the function. NOTE: If you want to reach out to the **teamId** please put your teamID as character, ex:`"2,12,22"` to reach the table.

``` r
endpoints <- c("franchise","franchise-team-totals","franchise-season-records","franchise-goalie-records","franchise-skater-records","expand=team.roster","expand=person.names","expand=team.schedule.next","expand=team.schedule.previous","expand=team.stats","expand=team.roster&season","teamId","stats=statsSingleSeasonPlayoffs")

idOption <- c(rep("None",2),rep("franchiseID or teamName without location",3),rep("teamID or teamName without location",8))

seasonOption <- c(rep("No",10),"Yes", "No","No")

kable(cbind(endpoints,idOption,seasonOption), format = "html")
```

<table>
<thead>
<tr>
<th style="text-align:left;">
endpoints
</th>
<th style="text-align:left;">
idOption
</th>
<th style="text-align:left;">
seasonOption
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
franchise
</td>
<td style="text-align:left;">
None
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
franchise-team-totals
</td>
<td style="text-align:left;">
None
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
franchise-season-records
</td>
<td style="text-align:left;">
franchiseID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
franchise-goalie-records
</td>
<td style="text-align:left;">
franchiseID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
franchise-skater-records
</td>
<td style="text-align:left;">
franchiseID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
expand=team.roster
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
expand=person.names
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
expand=team.schedule.next
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
expand=team.schedule.previous
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
expand=team.stats
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
expand=team.roster&season
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
Yes
</td>
</tr>
<tr>
<td style="text-align:left;">
teamId
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
<tr>
<td style="text-align:left;">
stats=statsSingleSeasonPlayoffs
</td>
<td style="text-align:left;">
teamID or teamName without location
</td>
<td style="text-align:left;">
No
</td>
</tr>
</tbody>
</table>
``` r
kable(base, caption = "ID table for teams", format = "html")
```

<table>
<caption>
ID table for teams
</caption>
<thead>
<tr>
<th style="text-align:left;">
teams.franchise.teamName
</th>
<th style="text-align:right;">
teams.franchiseId
</th>
<th style="text-align:left;">
teams.teamName
</th>
<th style="text-align:right;">
teams.id
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Devils
</td>
<td style="text-align:right;">
23
</td>
<td style="text-align:left;">
Devils
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
Islanders
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:left;">
Islanders
</td>
<td style="text-align:right;">
2
</td>
</tr>
<tr>
<td style="text-align:left;">
Rangers
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
Rangers
</td>
<td style="text-align:right;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
Flyers
</td>
<td style="text-align:right;">
16
</td>
<td style="text-align:left;">
Flyers
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
Penguins
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:left;">
Penguins
</td>
<td style="text-align:right;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
Bruins
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Bruins
</td>
<td style="text-align:right;">
6
</td>
</tr>
<tr>
<td style="text-align:left;">
Sabres
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
Sabres
</td>
<td style="text-align:right;">
7
</td>
</tr>
<tr>
<td style="text-align:left;">
Canadiens
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Canadiens
</td>
<td style="text-align:right;">
8
</td>
</tr>
<tr>
<td style="text-align:left;">
Senators
</td>
<td style="text-align:right;">
30
</td>
<td style="text-align:left;">
Senators
</td>
<td style="text-align:right;">
9
</td>
</tr>
<tr>
<td style="text-align:left;">
Maple Leafs
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Maple Leafs
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
Hurricanes
</td>
<td style="text-align:right;">
26
</td>
<td style="text-align:left;">
Hurricanes
</td>
<td style="text-align:right;">
12
</td>
</tr>
<tr>
<td style="text-align:left;">
Panthers
</td>
<td style="text-align:right;">
33
</td>
<td style="text-align:left;">
Panthers
</td>
<td style="text-align:right;">
13
</td>
</tr>
<tr>
<td style="text-align:left;">
Lightning
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:left;">
Lightning
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
Capitals
</td>
<td style="text-align:right;">
24
</td>
<td style="text-align:left;">
Capitals
</td>
<td style="text-align:right;">
15
</td>
</tr>
<tr>
<td style="text-align:left;">
Blackhawks
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
Blackhawks
</td>
<td style="text-align:right;">
16
</td>
</tr>
<tr>
<td style="text-align:left;">
Red Wings
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
Red Wings
</td>
<td style="text-align:right;">
17
</td>
</tr>
<tr>
<td style="text-align:left;">
Predators
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:left;">
Predators
</td>
<td style="text-align:right;">
18
</td>
</tr>
<tr>
<td style="text-align:left;">
Blues
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
Blues
</td>
<td style="text-align:right;">
19
</td>
</tr>
<tr>
<td style="text-align:left;">
Flames
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:left;">
Flames
</td>
<td style="text-align:right;">
20
</td>
</tr>
<tr>
<td style="text-align:left;">
Avalanche
</td>
<td style="text-align:right;">
27
</td>
<td style="text-align:left;">
Avalanche
</td>
<td style="text-align:right;">
21
</td>
</tr>
<tr>
<td style="text-align:left;">
Oilers
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:left;">
Oilers
</td>
<td style="text-align:right;">
22
</td>
</tr>
<tr>
<td style="text-align:left;">
Canucks
</td>
<td style="text-align:right;">
20
</td>
<td style="text-align:left;">
Canucks
</td>
<td style="text-align:right;">
23
</td>
</tr>
<tr>
<td style="text-align:left;">
Ducks
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:left;">
Ducks
</td>
<td style="text-align:right;">
24
</td>
</tr>
<tr>
<td style="text-align:left;">
Stars
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:left;">
Stars
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
Kings
</td>
<td style="text-align:right;">
14
</td>
<td style="text-align:left;">
Kings
</td>
<td style="text-align:right;">
26
</td>
</tr>
<tr>
<td style="text-align:left;">
Sharks
</td>
<td style="text-align:right;">
29
</td>
<td style="text-align:left;">
Sharks
</td>
<td style="text-align:right;">
28
</td>
</tr>
<tr>
<td style="text-align:left;">
Blue Jackets
</td>
<td style="text-align:right;">
36
</td>
<td style="text-align:left;">
Blue Jackets
</td>
<td style="text-align:right;">
29
</td>
</tr>
<tr>
<td style="text-align:left;">
Wild
</td>
<td style="text-align:right;">
37
</td>
<td style="text-align:left;">
Wild
</td>
<td style="text-align:right;">
30
</td>
</tr>
<tr>
<td style="text-align:left;">
Jets
</td>
<td style="text-align:right;">
35
</td>
<td style="text-align:left;">
Jets
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
Coyotes
</td>
<td style="text-align:right;">
28
</td>
<td style="text-align:left;">
Coyotes
</td>
<td style="text-align:right;">
53
</td>
</tr>
<tr>
<td style="text-align:left;">
Golden Knights
</td>
<td style="text-align:right;">
38
</td>
<td style="text-align:left;">
Golden Knights
</td>
<td style="text-align:right;">
54
</td>
</tr>
</tbody>
</table>
Accessory for API endpoints
---------------------------

``` r
#build a wrapper function to let user reach endpoints easily
endpoints <- function(x,...){
  record <- c("franchise","franchise-team-totals","franchise-season-records", "franchise-goalie-records", "franchise-skater-records")
  
  stats <- c("expand=team.roster","expand=person.names","expand=team.schedule.next","expand=team.schedule.previous","expand=team.stats","expand=team.roster&season","teamId","stats=statsSingleSeasonPlayoffs")
  
  if (x %in% record) recordAPI(x,...)
  else if (x %in% stats) statsAPI(x,...)
  else stop("Please enter the correct name of your enpoints")
}
```

Data analysis
=============

Building data for later use
---------------------------

``` r
#get some wins/losses data for active teams
teamtotal<-endpoints("franchise-team-totals")%>% filter(is.na(data.lastSeasonId))

#get teams' division and conference from another table
division <- endpoints("expand=team.roster")%>% select(teams.id, teams.division.name, teams.division.nameShort, teams.conference.name) %>%rename(data.teamId=teams.id)

#combine them as new dataset
new <- left_join(teamtotal,division, by="data.teamId") 

#creat new variables
#calculating some rate
new <- new %>% mutate(winrate=data.wins/data.gamesPlayed, winlossrate=data.wins/data.losses, overtimelossrate= data.overtimeLosses/data.losses, goalpergame=data.goalsFor/data.gamesPlayed, averagePenaltytime= data.penaltyMinutes/data.gamesPlayed)%>% mutate(lossathomerate=data.homeLosses/(data.homeLosses+data.homeWins),winonraodrate=data.roadWins/(data.roadWins+data.roadLosses))

#put the level on the age of teams
for (i in 1: nrow(new)){
  
  if (new$data.firstSeasonId[i]/10000<1943){
    new$age[i]<- "Senior"
  }
  else if (new$data.firstSeasonId[i]/10000<1968){
    new$age[i]<- "Junior"
  }
  else if (new$data.firstSeasonId[i]/10000<1993){
    new$age[i]<- "Sophomore"
  }
  else {
    new$age[i]<- "Freshman"
  }
}
```

Numeric summarize
-----------------

### A contingency table (Division\*Fist year of play)

``` r
#level the age
new$age <- as.factor(new$age)
levels(new$age)<-list("1917-1942"="Senior","1942-1967"="Junior","1967-1992"="Sophomore","1992-2017"="Freshman")

#show the table
datafortable <- new %>% filter(data.gameTypeId==2)
table1 <- table(datafortable$age, datafortable$teams.division.name) 
kable(table1,caption = " Division and Team established year information", format = "html") 
```

<table>
<caption>
Division and Team established year information
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Atlantic
</th>
<th style="text-align:right;">
Central
</th>
<th style="text-align:right;">
Metropolitan
</th>
<th style="text-align:right;">
Pacific
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
1917-1942
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
1942-1967
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
1967-1992
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
1992-2017
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
3
</td>
</tr>
</tbody>
</table>
In this table, we can notice that Atlantic has older team distribution when Centrl and Pacific have more younger team

### Numeric summary table

``` r
#make a summary table among all teams

#build a function for calculation
calculate <- function(x,...){
  data <- new %>% filter(data.gameTypeId == x)%>% select(winrate,winlossrate,overtimelossrate,goalpergame)
  if (x==2) type <- "regular season" else type <- "play off season"

kable(apply(data,2, summary), digit = 4,caption = paste0("Summary among all active teams during ", type), format = "html")
}
calculate(2)
```

<table>
<caption>
Summary among all active teams during regular season
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
winrate
</th>
<th style="text-align:right;">
winlossrate
</th>
<th style="text-align:right;">
overtimelossrate
</th>
<th style="text-align:right;">
goalpergame
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:right;">
0.3958
</td>
<td style="text-align:right;">
0.8051
</td>
<td style="text-align:right;">
0.0546
</td>
<td style="text-align:right;">
2.4833
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:right;">
0.4413
</td>
<td style="text-align:right;">
1.0471
</td>
<td style="text-align:right;">
0.0890
</td>
<td style="text-align:right;">
2.8356
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
0.4605
</td>
<td style="text-align:right;">
1.1643
</td>
<td style="text-align:right;">
0.1252
</td>
<td style="text-align:right;">
3.0541
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.4639
</td>
<td style="text-align:right;">
1.1710
</td>
<td style="text-align:right;">
0.1508
</td>
<td style="text-align:right;">
2.9862
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:right;">
0.4837
</td>
<td style="text-align:right;">
1.2694
</td>
<td style="text-align:right;">
0.2157
</td>
<td style="text-align:right;">
3.1771
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:right;">
0.5660
</td>
<td style="text-align:right;">
1.6625
</td>
<td style="text-align:right;">
0.2750
</td>
<td style="text-align:right;">
3.3322
</td>
</tr>
</tbody>
</table>
``` r
calculate(3)
```

<table>
<caption>
Summary among all active teams during play off season
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
winrate
</th>
<th style="text-align:right;">
winlossrate
</th>
<th style="text-align:right;">
overtimelossrate
</th>
<th style="text-align:right;">
goalpergame
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:right;">
0.3506
</td>
<td style="text-align:right;">
0.5400
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
2.2597
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:right;">
0.4536
</td>
<td style="text-align:right;">
0.8321
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
2.5917
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
0.4891
</td>
<td style="text-align:right;">
0.9745
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
2.7400
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
0.4906
</td>
<td style="text-align:right;">
0.9964
</td>
<td style="text-align:right;">
0.0007
</td>
<td style="text-align:right;">
2.7758
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:right;">
0.5354
</td>
<td style="text-align:right;">
1.1523
</td>
<td style="text-align:right;">
0.0000
</td>
<td style="text-align:right;">
2.9395
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:right;">
0.5970
</td>
<td style="text-align:right;">
1.4815
</td>
<td style="text-align:right;">
0.0111
</td>
<td style="text-align:right;">
3.6791
</td>
</tr>
</tbody>
</table>
In this two table we can see that the win rate and goal per game have no huge difference between regular season and play off season. However, the over time loss rate is very differet. Also, we can discover that the mean is higher that 755 percentile which meas that only a small group of team have over time loss in play off season.

``` r
#make a summary with grouping data by division
catesummary <- function(x,...){
  datacatesumm <- new %>% filter(data.gameTypeId == 2)%>% filter(teams.division.name == x) %>% select(data.goalsAgainst,data.goalsFor, data.points, data.pointPctg, data.wins)
    kable(apply(datacatesumm,2, summary), format = "html", caption = paste("Summary of division", x), digit = 4, col.names = c("Goal Against", "Goals For", "Points", "Point Percentage", " Wins"))
}
catesummary("Metropolitan")
```

<table>
<caption>
Summary of division Metropolitan
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Goal Against
</th>
<th style="text-align:right;">
Goals For
</th>
<th style="text-align:right;">
Points
</th>
<th style="text-align:right;">
Point Percentage
</th>
<th style="text-align:right;">
Wins
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:right;">
4425.00
</td>
<td style="text-align:right;">
3955.00
</td>
<td style="text-align:right;">
1500.00
</td>
<td style="text-align:right;">
0.4960
</td>
<td style="text-align:right;">
660.00
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:right;">
7782.00
</td>
<td style="text-align:right;">
7669.00
</td>
<td style="text-align:right;">
2806.75
</td>
<td style="text-align:right;">
0.5122
</td>
<td style="text-align:right;">
1229.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
11584.50
</td>
<td style="text-align:right;">
11607.00
</td>
<td style="text-align:right;">
3803.50
</td>
<td style="text-align:right;">
0.5201
</td>
<td style="text-align:right;">
1660.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
10889.50
</td>
<td style="text-align:right;">
10952.50
</td>
<td style="text-align:right;">
3717.75
</td>
<td style="text-align:right;">
0.5248
</td>
<td style="text-align:right;">
1615.25
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:right;">
12513.75
</td>
<td style="text-align:right;">
13564.75
</td>
<td style="text-align:right;">
4382.25
</td>
<td style="text-align:right;">
0.5304
</td>
<td style="text-align:right;">
1913.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:right;">
19863.00
</td>
<td style="text-align:right;">
19864.00
</td>
<td style="text-align:right;">
6667.00
</td>
<td style="text-align:right;">
0.5759
</td>
<td style="text-align:right;">
2856.00
</td>
</tr>
</tbody>
</table>
``` r
catesummary("Atlantic")
```

<table>
<caption>
Summary of division Atlantic
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Goal Against
</th>
<th style="text-align:right;">
Goals For
</th>
<th style="text-align:right;">
Points
</th>
<th style="text-align:right;">
Point Percentage
</th>
<th style="text-align:right;">
Wins
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:right;">
5969.00
</td>
<td style="text-align:right;">
5476.00
</td>
<td style="text-align:right;">
2049.00
</td>
<td style="text-align:right;">
0.4990
</td>
<td style="text-align:right;">
852.00
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:right;">
6471.75
</td>
<td style="text-align:right;">
6078.50
</td>
<td style="text-align:right;">
2170.50
</td>
<td style="text-align:right;">
0.5074
</td>
<td style="text-align:right;">
948.75
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
14929.50
</td>
<td style="text-align:right;">
15878.00
</td>
<td style="text-align:right;">
5382.50
</td>
<td style="text-align:right;">
0.5228
</td>
<td style="text-align:right;">
2314.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
13279.12
</td>
<td style="text-align:right;">
13966.12
</td>
<td style="text-align:right;">
4890.75
</td>
<td style="text-align:right;">
0.5304
</td>
<td style="text-align:right;">
2113.25
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:right;">
18782.75
</td>
<td style="text-align:right;">
20080.75
</td>
<td style="text-align:right;">
6865.25
</td>
<td style="text-align:right;">
0.5429
</td>
<td style="text-align:right;">
2956.00
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:right;">
19805.00
</td>
<td style="text-align:right;">
21632.00
</td>
<td style="text-align:right;">
7899.00
</td>
<td style="text-align:right;">
0.5868
</td>
<td style="text-align:right;">
3449.00
</td>
</tr>
</tbody>
</table>
``` r
catesummary("Central")
```

<table>
<caption>
Summary of division Central
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Goal Against
</th>
<th style="text-align:right;">
Goals For
</th>
<th style="text-align:right;">
Points
</th>
<th style="text-align:right;">
Point Percentage
</th>
<th style="text-align:right;">
Wins
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:right;">
1997.000
</td>
<td style="text-align:right;">
2039.000
</td>
<td style="text-align:right;">
776.000
</td>
<td style="text-align:right;">
0.5040
</td>
<td style="text-align:right;">
352.000
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:right;">
4264.500
</td>
<td style="text-align:right;">
4279.500
</td>
<td style="text-align:right;">
1757.500
</td>
<td style="text-align:right;">
0.5401
</td>
<td style="text-align:right;">
772.500
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
5325.000
</td>
<td style="text-align:right;">
5660.000
</td>
<td style="text-align:right;">
2175.000
</td>
<td style="text-align:right;">
0.5561
</td>
<td style="text-align:right;">
968.000
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
7617.857
</td>
<td style="text-align:right;">
7736.571
</td>
<td style="text-align:right;">
2830.143
</td>
<td style="text-align:right;">
0.5499
</td>
<td style="text-align:right;">
1230.857
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:right;">
8986.500
</td>
<td style="text-align:right;">
9261.000
</td>
<td style="text-align:right;">
3394.500
</td>
<td style="text-align:right;">
0.5628
</td>
<td style="text-align:right;">
1481.500
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:right;">
19501.000
</td>
<td style="text-align:right;">
19376.000
</td>
<td style="text-align:right;">
6556.000
</td>
<td style="text-align:right;">
0.5833
</td>
<td style="text-align:right;">
2788.000
</td>
</tr>
</tbody>
</table>
``` r
catesummary("Pacific")
```

<table>
<caption>
Summary of division Pacific
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Goal Against
</th>
<th style="text-align:right;">
Goals For
</th>
<th style="text-align:right;">
Points
</th>
<th style="text-align:right;">
Point Percentage
</th>
<th style="text-align:right;">
Wins
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Min.
</td>
<td style="text-align:right;">
669.000
</td>
<td style="text-align:right;">
748.000
</td>
<td style="text-align:right;">
288.00
</td>
<td style="text-align:right;">
0.4521
</td>
<td style="text-align:right;">
133.000
</td>
</tr>
<tr>
<td style="text-align:left;">
1st Qu.
</td>
<td style="text-align:right;">
4605.000
</td>
<td style="text-align:right;">
4473.250
</td>
<td style="text-align:right;">
1776.50
</td>
<td style="text-align:right;">
0.4908
</td>
<td style="text-align:right;">
777.250
</td>
</tr>
<tr>
<td style="text-align:left;">
Median
</td>
<td style="text-align:right;">
8039.500
</td>
<td style="text-align:right;">
8220.000
</td>
<td style="text-align:right;">
2835.00
</td>
<td style="text-align:right;">
0.5268
</td>
<td style="text-align:right;">
1241.500
</td>
</tr>
<tr>
<td style="text-align:left;">
Mean
</td>
<td style="text-align:right;">
7591.375
</td>
<td style="text-align:right;">
7429.625
</td>
<td style="text-align:right;">
2477.75
</td>
<td style="text-align:right;">
0.5227
</td>
<td style="text-align:right;">
1076.125
</td>
</tr>
<tr>
<td style="text-align:left;">
3rd Qu.
</td>
<td style="text-align:right;">
11062.000
</td>
<td style="text-align:right;">
10941.500
</td>
<td style="text-align:right;">
3469.50
</td>
<td style="text-align:right;">
0.5414
</td>
<td style="text-align:right;">
1509.750
</td>
</tr>
<tr>
<td style="text-align:left;">
Max.
</td>
<td style="text-align:right;">
13591.000
</td>
<td style="text-align:right;">
12910.000
</td>
<td style="text-align:right;">
4048.00
</td>
<td style="text-align:right;">
0.6128
</td>
<td style="text-align:right;">
1733.000
</td>
</tr>
</tbody>
</table>
In these table we can notice that Atlantic is tha division that has the most win among all division, this is very reasonable for me because in the previous table, we know that Atlantic is the one that has more older teams. Other data seems reasonable, in goals Against and Goals for the summary are similar among all the division, only Pacific and Central has lower minimum. Maybe some younger teams just played less games than others.

Plots
-----

### A bar plot tells conference and division

``` r
#draw a bar plot to show the count in each division and conference
type2 <- new %>% filter(data.gameTypeId==2)
bar1 <- ggplot(data=type2,aes(x=teams.conference.name))
bar1+ geom_bar(aes(fill=teams.division.name), position = "dodge")+labs(title = "Barplot for teams' Conference and Division")+xlab("Conference")
```

![](README_files/figure-markdown_github/bar%20plot-1.png) This Bar plot show you the count of each Dicision and Conference. So we know that there are two conference, each has two division. In each division there are 7~8 teams.

### Histogram of penalty times

``` r
#seperate data by regular season and play off season since the differnce are obvious
type3 <- new %>% filter(data.gameTypeId==3)

#make two histogram to show the distribution of penalty time
histo1 <- ggplot(data=type2,aes(x=averagePenaltytime))
histo1+geom_histogram(binwidth = 1, aes(fill=age))+labs(title = "Histogram for penalty time during relugar season")+xlab("Average penalty time (in minutes)")
```

![](README_files/figure-markdown_github/histogram-1.png)

``` r
histo2 <- ggplot(data=type3,aes(x=averagePenaltytime))
histo2+geom_histogram(binwidth = 1, aes(fill=age))+labs(title = "Histogram for penalty time during play off season")+xlab("Average penalty time (in minutes)")
```

![](README_files/figure-markdown_github/histogram-2.png)

In these 2 plots we can say that the oldest teams have lower average penalty time both in the regular seasons and in the play off seasons if we don't take recent established teams into consider.

Data analysis between 4 selected team
=====================================

I choose NY islanders, Tampa Bay lightening, Vegas Golden Knights, Dallas stars, which are in the Conference Finals in 2019-20 NHL season.

Arrange data for later use
--------------------------

``` r
#filtering old dataset to only selected groups
fourteams <- filter(new, data.teamId %in% c(2, 14, 25, 54)) 
fourteams <- fourteams 

#making some new dataset on selected
#dataset of season record
NY <-endpoints("franchise-season-records",22)
Tampa_bay <-endpoints("franchise-season-records",31)
Vegas <-endpoints("franchise-season-records",38)
Dallas <-endpoints("franchise-season-records",15)
seasonrecord <- rbind.data.frame(NY, Tampa_bay, Vegas, Dallas)

#detail about the goalier of selected team
NY <-endpoints("franchise-goalie-records",22)
Tampa_bay <-endpoints("franchise-goalie-records",31)
Vegas <-endpoints("franchise-goalie-records",38)
Dallas <-endpoints("franchise-goalie-records",15)
goalierecord <- rbind.data.frame(NY, Tampa_bay, Vegas, Dallas)

#detail about the skater of selected team
NY <-endpoints("franchise-skater-records",22)
Tampa_bay <-endpoints("franchise-skater-records",31)
Vegas <-endpoints("franchise-skater-records",38)
Dallas <-endpoints("franchise-skater-records",15)
skaterrecord <- rbind.data.frame(NY, Tampa_bay, Vegas, Dallas)

#make one new variables for goalier
goalierecord <- goalierecord %>% mutate(winRate=data.wins/data.gamesPlayed)
```

Numerical summary
-----------------

### Some data to compare between these team

``` r
#rearrange the dataset
fourteams <- fourteams %>% select(data.teamName,data.gamesPlayed,winrate,winonraodrate,lossathomerate,data.gameTypeId)%>% arrange( data.gameTypeId)

rs <- filter(fourteams, data.gameTypeId==2) %>% select(-data.gameTypeId)
pos <- filter(fourteams, data.gameTypeId==3) %>% select(-data.gameTypeId)

#print it out
kable(rs, format = "html", caption = "Data summary of selected team during regular season", col.names = c("Team", "Game Played", "Win Rate", "Win on Road Rate", "Loss at Home Rate"))
```

<table>
<caption>
Data summary of selected team during regular season
</caption>
<thead>
<tr>
<th style="text-align:left;">
Team
</th>
<th style="text-align:right;">
Game Played
</th>
<th style="text-align:right;">
Win Rate
</th>
<th style="text-align:right;">
Win on Road Rate
</th>
<th style="text-align:right;">
Loss at Home Rate
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
3732
</td>
<td style="text-align:right;">
0.4437299
</td>
<td style="text-align:right;">
0.4434783
</td>
<td style="text-align:right;">
0.4170792
</td>
</tr>
<tr>
<td style="text-align:left;">
Tampa Bay Lightning
</td>
<td style="text-align:right;">
2138
</td>
<td style="text-align:right;">
0.4438728
</td>
<td style="text-align:right;">
0.4400428
</td>
<td style="text-align:right;">
0.4306878
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
2053
</td>
<td style="text-align:right;">
0.5168047
</td>
<td style="text-align:right;">
0.5381166
</td>
<td style="text-align:right;">
0.3457207
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
235
</td>
<td style="text-align:right;">
0.5659574
</td>
<td style="text-align:right;">
0.5523810
</td>
<td style="text-align:right;">
0.3055556
</td>
</tr>
</tbody>
</table>
``` r
kable(pos, format = "html", caption = "Data summary of selected team during play off season", col.names = c("Team", "Game Played", "Win Rate", "Win on Road Rate", "Loss at Home Rate"))
```

<table>
<caption>
Data summary of selected team during play off season
</caption>
<thead>
<tr>
<th style="text-align:left;">
Team
</th>
<th style="text-align:right;">
Game Played
</th>
<th style="text-align:right;">
Win Rate
</th>
<th style="text-align:right;">
Win on Road Rate
</th>
<th style="text-align:right;">
Loss at Home Rate
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
New York Islanders
</td>
<td style="text-align:right;">
294
</td>
<td style="text-align:right;">
0.5476190
</td>
<td style="text-align:right;">
0.4610390
</td>
<td style="text-align:right;">
0.3571429
</td>
</tr>
<tr>
<td style="text-align:left;">
Tampa Bay Lightning
</td>
<td style="text-align:right;">
156
</td>
<td style="text-align:right;">
0.5576923
</td>
<td style="text-align:right;">
0.5866667
</td>
<td style="text-align:right;">
0.4691358
</td>
</tr>
<tr>
<td style="text-align:left;">
Dallas Stars
</td>
<td style="text-align:right;">
194
</td>
<td style="text-align:right;">
0.5309278
</td>
<td style="text-align:right;">
0.5051546
</td>
<td style="text-align:right;">
0.4432990
</td>
</tr>
<tr>
<td style="text-align:left;">
Vegas Golden Knights
</td>
<td style="text-align:right;">
47
</td>
<td style="text-align:right;">
0.5957447
</td>
<td style="text-align:right;">
0.5217391
</td>
<td style="text-align:right;">
0.3333333
</td>
</tr>
</tbody>
</table>
These table show the data directly, we can notice that **Vegas Golden Knights** has not played many games. Because this team started to play in NHL in 2017. Therefore, the data seems to be not so compareable to other three groups. But we can also say that, this young group has less likely to loss at home than any others.

### Distribution of skater position for each team

``` r
#showing skater position distribution of selected between non-active player and avctive player
table2 <- table(skaterrecord$data.positionCode, skaterrecord$data.franchiseName, skaterrecord$data.activePlayer) 
kable(table2[ , ,1], format = "html",caption = " Position distribution of skater for non-active player")
```

<table>
<caption>
Position distribution of skater for non-active player
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dallas Stars
</th>
<th style="text-align:right;">
New York Islanders
</th>
<th style="text-align:right;">
Tampa Bay Lightning
</th>
<th style="text-align:right;">
Vegas Golden Knights
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
C
</td>
<td style="text-align:right;">
135
</td>
<td style="text-align:right;">
120
</td>
<td style="text-align:right;">
59
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
D
</td>
<td style="text-align:right;">
179
</td>
<td style="text-align:right;">
159
</td>
<td style="text-align:right;">
126
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
L
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:right;">
98
</td>
<td style="text-align:right;">
63
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
114
</td>
<td style="text-align:right;">
78
</td>
<td style="text-align:right;">
67
</td>
<td style="text-align:right;">
1
</td>
</tr>
</tbody>
</table>
``` r
kable(table2[ , ,2], format = "html",caption = " Position distribution of skater for active player")
```

<table>
<caption>
Position distribution of skater for active player
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Dallas Stars
</th>
<th style="text-align:right;">
New York Islanders
</th>
<th style="text-align:right;">
Tampa Bay Lightning
</th>
<th style="text-align:right;">
Vegas Golden Knights
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
C
</td>
<td style="text-align:right;">
17
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
22
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
D
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
14
</td>
</tr>
<tr>
<td style="text-align:left;">
L
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
R
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
8
</td>
<td style="text-align:right;">
5
</td>
</tr>
</tbody>
</table>
The distribution of the position is similar among each team about 15 for position c and D and about 10 for L and R. **Vegaas golden Knights** has less amount in each position maybe because they are a 3-year-old team.

Plots
-----

### Scatter and boxplot

``` r
#choosing player that have already played over 10 games
actskaterrecord <- skaterrecord %>%  filter(data.gamesPlayed > 10)

#make a boxplot for skater to see the summary of most point in one season
boxplot1 <- ggplot(data=actskaterrecord,aes(x=data.franchiseName, y=data.mostPointsOneSeason))
boxplot1+geom_boxplot()+labs(title="Boxplot of most point in one season for skater who palyed more than 15 games", x="Team", y="Skater's Most Point in One Season")
```

![](README_files/figure-markdown_github/boxplot%20for%20activeskater-1.png) We can say that **Vegas Golden Knights** has higher average points in one season and **Tampa Bay Lightning** has a liitle bit lower. Also, there are some outliers for **New York Islanders** and **Tampa Bay Lightning**. I personnally think that this is Rare in a century. Many fter 50 years \*\* Vegas Golen Knights\*\* will also have some skaters who will get more than 100 points in one season.

``` r
#showing the win rate boxplot of the goalier of selected team
boxplot2 <- ggplot(data=goalierecord,aes(x=data.franchiseName, y=winRate))
boxplot2+geom_boxplot()+geom_jitter(aes(color=data.franchiseName))+labs(title="Boxplot of win rate for goalier", x="Team", y=" Goalier's win rate")
```

![](README_files/figure-markdown_github/boxplot%20for%20all%20goalier-1.png) In this plot we can notice that **Tampa Bay Lightning's** goalier havehiest average win rate, Even one of them has a 100% winning rate. I bet that is because they play less games.

``` r
scatter1 <- ggplot(data=goalierecord, aes(x=data.mostGoalsAgainstOneGame, y=data.mostSavesOneGame))

scatter1+ geom_point(aes(color=data.activePlayer))+geom_smooth(aes(group=data.franchiseName), method=lm, color= "orange")+ labs(title = "Most saves vs Most goal against of goalier", x="Most Goal Against (in one game)", y="Most Saves (in one game)")+facet_grid(.~data.franchiseName)
```

![](README_files/figure-markdown_github/scatter%20plot%20for%20goalier-1.png)

In this plot we can notice that **Tampa Bay Lightning's** goalier are worse savers but better goal maker than other teams' goalier.

The End
=======

Thank you for reading my Prooject.

render("Project1.Rmd", output\_format ="github\_document", output\_file=I("README.md"))
