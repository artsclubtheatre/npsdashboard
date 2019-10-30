library(tidyverse)
library(lubridate)
library(odbc)
library(httr)
library(jsonlite)
library(tidytext)

# Initialize DB connection
con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "aws-tessil",
  Database = "impresario",
  UID = Sys.getenv("userId"),
  PWD = Sys.getenv("pwd"),
  Port = 1433
)

# Data Load In ----

# These are custom tables that hold the history of NPS scores for each patron

# Returned columns: id, customer_no, prod_season_no, prod_title, nps_prod_score, 
# created_by, create_dt, create_loc, last_updated_by, last_update_dt
nps_prod_con <- dbSendQuery(con,
                            "SELECT id, customer_no, prod_season_no,  b.description as prod_title, nps_prod_score, 
                            created_by, create_dt, create_loc, last_updated_by, last_update_dt
                            FROM lt_nps_prod a With (NOLOCK)
                            JOIN T_INVENTORY b With (NOLOCK) on a.prod_season_no = b.inv_no;")
nps_prod <- dbFetch(nps_prod_con)

# get just the unique title and prod_season_no combo

prodSeasonTitles <- nps_prod %>%
  group_by(prod_season_no) %>%
  summarize(title = unique(prod_title))

# Returned columns: id, customer_no, prod_season_no, nps_company_score, 
# created_by, create_dt, create_loc, last_updated_by, last_update_dt
nps_company_con <- dbSendQuery(con,
                               "SELECT * 
                               FROM lt_nps_company a with (NOLOCK)")
nps_company <- dbFetch(nps_company_con)

# Get Typeform Data through the API

typeformData <- GET(
  'https://api.typeform.com/forms/NTuvnn/responses?page_size=1000',
  add_headers(
    Authorization = paste0(
      "Bearer ", 
      Sys.getenv("typeformToken")
      )
    )
  ) %>%
  content("text") %>%
  fromJSON(flatten = TRUE)

surveyResponses <- tibble(
  answers = typeformData$items$answers,
  patronId = typeformData$items$hidden.patronid,
  prodSeasonNo = typeformData$items$hidden.prod_season_no,
  prodTitle = typeformData$items$hidden.show_title,
  segment = typeformData$items$hidden.segment
) %>%
  mutate(id = as.character(row_number()))

segments <- tibble(
  patronId = as.numeric(typeformData$items$hidden.patronid),
  segment = typeformData$items$hidden.segment
)

surveyAnswers <- bind_rows(surveyResponses$answers, .id="id") %>%
  filter(!is.na(text)) %>%
  left_join(surveyResponses, by="id") %>%
  select(-answers)
  

# Data Prep ----

companyScore <- calculateNPS(nps_company,"nps_company_score")

# Get the company scores by segment

companyScoreBySegment <- nps_company %>%
  left_join(segments, by=c("customer_no" = "patronId")) %>%
  mutate(nps = categorizeNPSRating(nps_company_score)) %>%
  group_by(segment, nps) %>%
  summarize(n = n()) %>%
  group_by(segment) %>%
  mutate(total = sum(n),
         per = round(n/total * 100, 1)) %>%
  filter(nps != "Passive") %>%
  spread(nps, per) %>%
  summarize(total = sum(total),
            detractor = sum(Detractor, na.rm=TRUE),
            promoter = sum(Promoter, na.rm=TRUE),
            score = promoter - detractor) %>%
  select(segment, score) %>%
  filter(!segment %in% c("STX", NA))

# Get the cumulative NPS as of each rating
cumulativeScores <- c()
cumulativePromoters <- c()
cumulativePassives <- c()
cumulativeDetractors <- c()
for(score in 1:nrow(nps_company)){
  ratings <- nps_company[1:score,]
  newScore <- calculateNPS(ratings, "nps_company_score")
  
  cumulativeScores <- append(cumulativeScores, newScore$npsScore)
  cumulativePromoters <- append(cumulativePromoters, newScore$totalPromoters)
  cumulativePassives <- append(cumulativePassives, newScore$totalPassives)
  cumulativeDetractors <- append(cumulativeDetractors, newScore$totalDetractors)
}
nps_company$cumulativeScore <- cumulativeScores
nps_company$totalPromoters <- cumulativePromoters
nps_company$totalPassives <- cumulativePassives
nps_company$totalDetractors <- cumulativeDetractors

# Get the NPS for each show
prodResults <- list()
for(prodSeason in factor(nps_prod$prod_season_no)){
  prodSeasonScores <- nps_prod %>%
    filter(prod_season_no == prodSeason)
  
  prodResults[[prodSeason]] <- calculateNPS(prodSeasonScores, "nps_prod_score")
}
productionScores <- bind_rows(prodResults, .id="prodSeason") %>%
  mutate(prodSeason = as.numeric(prodSeason)) %>%
  left_join(prodSeasonTitles, by=c("prodSeason" = "prod_season_no"))

# Combined data for comparison analysis

allScores <- nps_company %>%
  left_join(nps_prod, by=c("customer_no","prod_season_no")) %>%
  select(customer_no, prod_season_no, nps_company_score, nps_prod_score)

# Get the text for word clouds

artsClubStopWords <- tribble(
  ~ word,
  "play",
  "plays",
  "theatre",
  "arts",
  "club",
  "production",
  "productions",
  "performance",
  "performances"
)

companyText <- surveyAnswers %>%
  filter(field.ref == "actc-company-text") %>%
  select(text) %>%
  gather(key, text) %>%
  unnest_tokens(word, text) %>%
  count(word, sort=TRUE) %>%
  ungroup() %>%
  anti_join(stop_words) %>%
  anti_join(artsClubStopWords)

productionText <- surveyAnswers %>%
  filter(field.ref == "actc-show-text") %>%
  select(text, prodSeasonNo) %>%
  gather(key, text, -prodSeasonNo) %>%
  unnest_tokens(word, text) %>%
  group_by(prodSeasonNo) %>%
  count(word, sort=TRUE) %>%
  ungroup() %>%
  anti_join(stop_words) %>%
  anti_join(artsClubStopWords)

# Save data ----

save(
  nps_company,
  nps_prod,
  productionScores,
  companyScore,
  companyScoreBySegment,
  allScores,
  companyText,
  productionText,
  calculateNPS,
  file='NPSDashboard/npsData.RData'
)

# Functions ----

#' Calculates the NPS score 
#' 
#' @param patronScores    - the dataframe of patron NPS scores
#' @param scoreColumn     - the column in the dataframe that contains the scores
#' 
#' @return a tibble with:
#' {num}  npsScore        - the calculated NPS score, which is Promoter % - Detractor %
#' {int}  totalPromoters  - the number of promoters
#' {int}  totalPassives   - the number of passives
#' {int}  totalDetractors - the number of detractors
#' {int}  totalPatrons    - the number of patrons 

calculateNPS <- function (patronScores, scoreColumn) {
  require("dplyr")
  require("lazyeval")
  
  promoterScores <- c(9, 10)
  passiveScores <- c(7,8)
  detractorScores <- c(0:6)
  
  filterPromoters <- interp(~y %in% x, .values=list(y = as.name(scoreColumn), x=promoterScores))
  filterPassives <- interp(~y %in% x, .values=list(y = as.name(scoreColumn), x=passiveScores))
  filterDetractors <- interp(~y %in% x, .values=list(y = as.name(scoreColumn), x=detractorScores))
  
  promoters <- patronScores %>%
    filter_(filterPromoters)
  
  passives <- patronScores %>%
    filter_(filterPassives)
  
  detractors <- patronScores %>%
    filter_(filterDetractors)
  
  totalPromoters <- length(promoters[,scoreColumn])
  totalPassives <- length(passives[,scoreColumn])
  totalDetractors <- length(detractors[,scoreColumn])
  totalPatrons <- length(patronScores[,scoreColumn])
  
  print(
    paste0(
      "Promoters: ", totalPromoters, 
      " Passives: ", totalPassives,
      " Detractors: ", totalDetractors, 
      " Total Patrons: ", totalPatrons
      )
    )
  
  promotersPercent <- totalPromoters / totalPatrons
  detractorsPercent <- totalDetractors / totalPatrons
  
  npsScore <- (promotersPercent - detractorsPercent) * 100
  
  nps <- tibble(npsScore, totalPromoters, totalPassives, totalDetractors, totalPatrons)
  
  return(nps)
  
}

categorizeNPSRating <- function (score) {
  require("dplyr")
  
  promoterScores <- c(9, 10)
  passiveScores <- c(7,8)
  detractorScores <- c(0:6)
  
  category <- ifelse(
    score %in% promoterScores, "Promoter", 
    ifelse(
      score %in% passiveScores, "Passive", "Detractor"
      )
    )
  
  return(category)
}
