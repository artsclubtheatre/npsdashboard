library(shiny)
library(flexdashboard)
library(scales)
library(plotly)
library(wordcloud2)

load("npsData.RData")

shinyUI(
  fluidPage(
    div(class="col-xs-12 text-center bg-primary",
        h1("Net Promoter Score Dashboard")
        ),
    div(class="col-xs-12 text-center",
          h2("Overall Arts Club Score"),
          p(
              strong(companyScore$totalPromoters), " Promoters, ",
              strong(companyScore$totalPassives), " Passives, and ",
              strong(companyScore$totalDetractors), " Detractors "
            ),
        p(
          strong(percent(companyScore$totalPromoters / companyScore$totalPatrons)),
          " (Promoters %) - ",
          strong(percent(companyScore$totalDetractors / companyScore$totalPatrons)),
          " (Detractors %) = "
          ),
          gaugeOutput("companyScoreGauge"),
        h2("How has the overall Arts Club net promoter score changed over time?"),
        plotlyOutput("companyScoreOverTime"),
        h2("How has the number of patrons in each NPS category changed over time?"),
        plotOutput("companyRatingsOverTime"),
        h2("How does the Arts Club score break down by patron segment?"),
        plotOutput("companyScoreBySegment"),
        h2("What are the most common words used in company ratings?"),
        plotOutput("companyWordCloud"),
        h2("Are the Arts Club and production scores correlated?"),
        p("This shows the combination of ratings. The size of each dot counts how many times 
          that combination of scores was given.", br(), 
          "The line is a line of best fit through the points, which represents the correlation between the two scores.", br(),
          "If Arts Club and production ratings were perfectly correlated,
          we would expect to see a straight line following the dashed line."),
        h4(textOutput("companyProductionGreater")),
        plotOutput("companyProductionCorrelation"),
        hr(),
        h1("Production Net Promoter Scores"),
        uiOutput("prodPlots")
       ),
  title = "NPS Dashboard",
  theme = "style.css"
  )
)
