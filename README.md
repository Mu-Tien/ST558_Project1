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
    -   [building data for later use](#building-data-for-later-use)
    -   [Numeric summarize](#numeric-summarize)
        -   [A contingency table (Division\*Fist year of play)](#a-contingency-table-divisionfist-year-of-play)
        -   [Numeric summary table](#numeric-summary-table)
    -   [Plots](#plots)
        -   [a bar plot tells conference and division](#a-bar-plot-tells-conference-and-division)
        -   [a histogram of penalty times](#a-histogram-of-penalty-times)
-   [data analysis between 4 selected team](#data-analysis-between-4-selected-team)
    -   [Numerical summary](#numerical-summary)
    -   [Plots](#plots-1)
        -   [scatter and boxplot](#scatter-and-boxplot)
-   [render("Project1.Rmd", output\_format ="github\_document", output\_file=I("README.md"))](#renderproject1.rmd-output_format-github_document-output_fileireadme.md)

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

Data cleaning
=============

Building functions to reach statsAPI
------------------------------------

``` r
statsAPI <- function(x,teamID=NULL,season=NULL, ...){
#setting up base url for stats API
base_url <-"https://statsapi.web.nhl.com/api/v1/teams"
modifiers <- x

#construct the full path
if (x %in% c("expand=team.roster","expand=person.names","expand=team.schedule.next","expand=team.schedule.previous","expand=team.stats","stats=statsSingleSeasonPlayoffs")){
  
  if (is.null(teamID)){
    full_url <- paste0(base_url, "?", modifiers)}
  
  else if(length(teamID)>1){
    stop("sorry, we can only show one team each time")}

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

#m<- statsAPI("expand=team.roster")
#n<- statsAPI("expand=person.names")
#o<- statsAPI("expand=team.schedule.next")
#p<- statsAPI("expand=team.schedule.previous")
#q<- statsAPI("expand=team.stats",12)
r<- statsAPI("expand=team.roster&season",teamid=54, season = 19971998)
#s<- statsAPI("teamId=4,5,29",12)
#t<- statsAPI("stats=statsSingleSeasonPlayoffs")
```

Building functions to reach recordAPI
-------------------------------------

``` r
recordAPI <- function(x,franchiseID=NULL,...){
#setting up base url for stats API
base_url <-"https://records.nhl.com/site/api"
modifiers <- x

if (is.null(franchiseID)){
    full_url <- paste0(base_url, "/", modifiers)
  }
else if (x %in% c("franchise","franchise-team-totals")) {
    stop("Sorry,table franchise and franchise-team-totals cannot indicate team")
  }
else {
  full_url <- paste0(base_url, "/", modifiers,"?cayenneExp=franchiseId=", franchiseID)
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
c<-recordAPI("franchise-season-records",11)
d<-recordAPI("franchise-goalie-records",1)
e<-recordAPI("franchise-skater-records",1)
```

Below is a function that can help you read whatever the endpoint you would like to get, please key in the following option in the function.

``` r
endpoints <- c("franchise","franchise-team-totals","franchise-season-records","franchise-goalie-records","franchise-skater-records","expand=team.roster","expand=person.names","expand=team.schedule.next","expand=team.schedule.previous","expand=team.stats","expand=team.roster&season","teamId","stats=statsSingleSeasonPlayoffs")
idOption <- c(rep("None",2),rep("franchiseID",3),rep("teamID",8))
seasonOption <- c(rep("No",10),"Yes", "No","No")
kable(cbind(endpoints,idOption,seasonOption))
```

| endpoints                       | idOption    | seasonOption |
|:--------------------------------|:------------|:-------------|
| franchise                       | None        | No           |
| franchise-team-totals           | None        | No           |
| franchise-season-records        | franchiseID | No           |
| franchise-goalie-records        | franchiseID | No           |
| franchise-skater-records        | franchiseID | No           |
| expand=team.roster              | teamID      | No           |
| expand=person.names             | teamID      | No           |
| expand=team.schedule.next       | teamID      | No           |
| expand=team.schedule.previous   | teamID      | No           |
| expand=team.stats               | teamID      | No           |
| expand=team.roster&season       | teamID      | Yes          |
| teamId                          | teamID      | No           |
| stats=statsSingleSeasonPlayoffs | teamID      | No           |

``` r
IDtable <- recordAPI("franchise-team-totals") %>%filter(data.gameTypeId==2) %>% select(data.teamName, data.franchiseId, data.teamId)
kable(IDtable,caption = "Id table for teams")
```

| data.teamName           |  data.franchiseId|  data.teamId|
|:------------------------|-----------------:|------------:|
| New Jersey Devils       |                23|            1|
| New York Islanders      |                22|            2|
| New York Rangers        |                10|            3|
| Philadelphia Flyers     |                16|            4|
| Pittsburgh Penguins     |                17|            5|
| Boston Bruins           |                 6|            6|
| Buffalo Sabres          |                19|            7|
| Montréal Canadiens      |                 1|            8|
| Ottawa Senators         |                30|            9|
| Toronto Maple Leafs     |                 5|           10|
| Atlanta Thrashers       |                35|           11|
| Carolina Hurricanes     |                26|           12|
| Florida Panthers        |                33|           13|
| Tampa Bay Lightning     |                31|           14|
| Washington Capitals     |                24|           15|
| Chicago Blackhawks      |                11|           16|
| Detroit Red Wings       |                12|           17|
| Nashville Predators     |                34|           18|
| St. Louis Blues         |                18|           19|
| Calgary Flames          |                21|           20|
| Colorado Avalanche      |                27|           21|
| Edmonton Oilers         |                25|           22|
| Vancouver Canucks       |                20|           23|
| Anaheim Ducks           |                32|           24|
| Dallas Stars            |                15|           25|
| Los Angeles Kings       |                14|           26|
| Phoenix Coyotes         |                28|           27|
| San Jose Sharks         |                29|           28|
| Columbus Blue Jackets   |                36|           29|
| Minnesota Wild          |                37|           30|
| Minnesota North Stars   |                15|           31|
| Quebec Nordiques        |                27|           32|
| Winnipeg Jets (1979)    |                28|           33|
| Hartford Whalers        |                26|           34|
| Colorado Rockies        |                23|           35|
| Ottawa Senators (1917)  |                 3|           36|
| Hamilton Tigers         |                 4|           37|
| Pittsburgh Pirates      |                 9|           38|
| Philadelphia Quakers    |                 9|           39|
| Detroit Cougars         |                12|           40|
| Montreal Wanderers      |                 2|           41|
| Quebec Bulldogs         |                 4|           42|
| Montreal Maroons        |                 7|           43|
| New York Americans      |                 8|           44|
| St. Louis Eagles        |                 3|           45|
| Oakland Seals           |                13|           46|
| Atlanta Flames          |                21|           47|
| Kansas City Scouts      |                23|           48|
| Cleveland Barons        |                13|           49|
| Detroit Falcons         |                12|           50|
| Brooklyn Americans      |                 8|           51|
| Winnipeg Jets           |                35|           52|
| Arizona Coyotes         |                28|           53|
| Vegas Golden Knights    |                38|           54|
| California Golden Seals |                13|           56|
| Toronto Arenas          |                 5|           57|
| Toronto St. Patricks    |                 5|           58|

accessory for API endpoints
---------------------------

``` r
endpoints <- function(x,...){
  record <- c("franchise","franchise-team-totals","franchise-season-records", "franchise-goalie-records", "franchise-skater-records")
  
  stats <- c("expand=team.roster","expand=person.names ","expand=team.schedule.next","expand=team.schedule.previous","expand=team.stats","expand=team.roster&season","teamId","stats=statsSingleSeasonPlayoffs")
  
  if (x %in% record) recordAPI(x,...)
  else if (x %in% stats) statsAPI(x,...)
  else stop("Please enter the correct name of your enpoints")
}
try<-endpoints("franchise-season-records")
```

Data analysis
=============

building data for later use
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
new <- new %>% mutate(winrate=data.wins/data.gamesPlayed, winlossrate=data.wins/data.losses, overtimelossrate= data.overtimeLosses/data.losses, goalpergame=data.goalsFor/data.gamesPlayed, averagePenaltytime= data.penaltyMinutes/data.gamesPlayed)

#put the level on the age of te team
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
levels(new$age)<-list("1917-1942"="Freshman","1942-1967"="Sophomore","1967-1992"="Junior","1992-2017"="Senior")
#show the table
datafortable <- new %>% filter(data.gameTypeId==2)
table1 <- table(datafortable$age, datafortable$teams.division.name) 
kable(table1,caption = " Division and Team established year information") 
```

|           |  Atlantic|  Central|  Metropolitan|  Pacific|
|:----------|---------:|--------:|-------------:|--------:|
| 1917-1942 |         1|        5|             2|        3|
| 1942-1967 |         3|        0|             3|        4|
| 1967-1992 |         0|        1|             2|        1|
| 1992-2017 |         4|        1|             1|        0|

In this table, we can notice that Atlantic has younger team distribution when Centrl and Pacific have more older team

### Numeric summary table

``` r
calculate <- function(x,...){
  data <- new %>% filter(data.gameTypeId == x)%>% select(winrate,winlossrate,overtimelossrate,goalpergame)
  if (x==2) type <- "regular season" else type <- "play off season"
  
    kable(apply(data,2, summary), digit = 4,caption = paste0("Summary among all active teams during ", type))
}
calculate(2)
```

|         |  winrate|  winlossrate|  overtimelossrate|  goalpergame|
|:--------|--------:|------------:|-----------------:|------------:|
| Min.    |   0.3958|       0.8051|            0.0546|       2.4833|
| 1st Qu. |   0.4413|       1.0471|            0.0890|       2.8356|
| Median  |   0.4605|       1.1643|            0.1252|       3.0541|
| Mean    |   0.4639|       1.1710|            0.1508|       2.9862|
| 3rd Qu. |   0.4837|       1.2694|            0.2157|       3.1771|
| Max.    |   0.5660|       1.6625|            0.2750|       3.3322|

``` r
calculate(3)
```

|         |  winrate|  winlossrate|  overtimelossrate|  goalpergame|
|:--------|--------:|------------:|-----------------:|------------:|
| Min.    |   0.3506|       0.5400|            0.0000|       2.2597|
| 1st Qu. |   0.4536|       0.8321|            0.0000|       2.5917|
| Median  |   0.4891|       0.9745|            0.0000|       2.7400|
| Mean    |   0.4906|       0.9962|            0.0007|       2.7762|
| 3rd Qu. |   0.5354|       1.1523|            0.0000|       2.9395|
| Max.    |   0.5970|       1.4815|            0.0111|       3.6791|

Plots
-----

### a bar plot tells conference and division

``` r
type2 <- new %>% filter(data.gameTypeId==2)
bar1 <- ggplot(data=type2,aes(x=teams.conference.name))
bar1+ geom_bar(aes(fill=teams.division.name), position = "dodge")+labs(title = "Barplot for teams' conference and division")+xlab("conference")
```

![](README_files/figure-markdown_github/bar%20plot-1.png)

### a histogram of penalty times

``` r
type3 <- new %>% filter(data.gameTypeId==3)
histo1 <- ggplot(data=type2,aes(x=averagePenaltytime))
histo1+geom_histogram(binwidth = 1, aes(fill=teams.division.name))+labs(title = "Histogram for penalty time during relugar season")+xlab("penalty time in minutes")
```

![](README_files/figure-markdown_github/histogram-1.png)

``` r
histo2 <- ggplot(data=type3,aes(x=averagePenaltytime))
histo2+geom_histogram(binwidth = 1, aes(fill=age))+labs(title = "Histogram for penalty time during play off season")+xlab("penalty time in minutes")
```

![](README_files/figure-markdown_github/histogram-2.png)

data analysis between 4 selected team
=====================================

I choose NY islanders, Tampa Bay lightening, Vegas Golden Knights, Dallas stars, which are in the Conference Finals in 2019-20 NHL season

``` r
fourteams <- filter(new, data.teamId %in% c(2, 14, 25, 54)) 

NY <-endpoints("franchise-season-records",22)
Tampa_bay <-endpoints("franchise-season-records",31)
Vegas <-endpoints("franchise-season-records",38)
Dallas <-endpoints("franchise-season-records",15)
seasonrecord <- rbind.data.frame(NY, Tampa_bay, Vegas, Dallas)

NY <-endpoints("franchise-goalie-records",22)
Tampa_bay <-endpoints("franchise-goalie-records",31)
Vegas <-endpoints("franchise-goalie-records",38)
Dallas <-endpoints("franchise-goalie-records",15)
goalierecord <- rbind.data.frame(NY, Tampa_bay, Vegas, Dallas)

NY <-endpoints("franchise-skater-records",22)
Tampa_bay <-endpoints("franchise-skater-records",31)
Vegas <-endpoints("franchise-skater-records",38)
Dallas <-endpoints("franchise-skater-records",15)
skaterrecord <- rbind.data.frame(NY, Tampa_bay, Vegas, Dallas)

names(goalierecord)
```

    ##  [1] "data.id"                      "data.activePlayer"           
    ##  [3] "data.firstName"               "data.franchiseId"            
    ##  [5] "data.franchiseName"           "data.gameTypeId"             
    ##  [7] "data.gamesPlayed"             "data.lastName"               
    ##  [9] "data.losses"                  "data.mostGoalsAgainstDates"  
    ## [11] "data.mostGoalsAgainstOneGame" "data.mostSavesDates"         
    ## [13] "data.mostSavesOneGame"        "data.mostShotsAgainstDates"  
    ## [15] "data.mostShotsAgainstOneGame" "data.mostShutoutsOneSeason"  
    ## [17] "data.mostShutoutsSeasonIds"   "data.mostWinsOneSeason"      
    ## [19] "data.mostWinsSeasonIds"       "data.overtimeLosses"         
    ## [21] "data.playerId"                "data.positionCode"           
    ## [23] "data.rookieGamesPlayed"       "data.rookieShutouts"         
    ## [25] "data.rookieWins"              "data.seasons"                
    ## [27] "data.shutouts"                "data.ties"                   
    ## [29] "data.wins"                    "total"

``` r
goalierecord <- goalierecord %>% mutate(winRate=data.wins/data.gamesPlayed)
```

Numerical summary
-----------------

``` r
#datafortable2 <- skaterrecord %>% filter(data.gameTypeId==2)
table2 <- table(skaterrecord$data.positionCode, skaterrecord$data.franchiseName, skaterrecord$data.activePlayer) 
kable(table2[ , ,1],caption = " teams and skatter position for non-active player")
```

|     |  Dallas Stars|  New York Islanders|  Tampa Bay Lightning|  Vegas Golden Knights|
|:----|-------------:|-------------------:|--------------------:|---------------------:|
| C   |           135|                 120|                   59|                     1|
| D   |           179|                 159|                  126|                     1|
| L   |           124|                  98|                   63|                     1|
| R   |           114|                  78|                   67|                     1|

``` r
kable(table2[ , ,2],caption = " teams and skatter position for active player")
```

|     |  Dallas Stars|  New York Islanders|  Tampa Bay Lightning|  Vegas Golden Knights|
|:----|-------------:|-------------------:|--------------------:|---------------------:|
| C   |            17|                  15|                   22|                    14|
| D   |            25|                  19|                   15|                    14|
| L   |            10|                  10|                    6|                    10|
| R   |            11|                  10|                    8|                     5|

Plots
-----

### scatter and boxplot

``` r
actskaterrecord <- skaterrecord %>%  filter(data.gamesPlayed > 15)
boxplot1 <- ggplot(data=actskaterrecord,aes(x=data.franchiseName, y=data.mostPointsOneSeason))
boxplot1+geom_boxplot()+labs(title="Boxplot of most point in one season for skater who palyed more than 15 games")
```

![](README_files/figure-markdown_github/boxplot%20for%20activeskater-1.png)

``` r
#+geom_jitter(aes(color=data.franchiseName))
```

``` r
boxplot2 <- ggplot(data=goalierecord,aes(x=data.franchiseName, y=winRate))
boxplot2+geom_boxplot()+geom_jitter(aes(color=data.franchiseName))+labs(title="Boxplot of win rate for goalier")
```

![](README_files/figure-markdown_github/boxplot%20for%20all%20goalier-1.png)

render("Project1.Rmd", output\_format ="github\_document", output\_file=I("README.md"))
=======================================================================================
