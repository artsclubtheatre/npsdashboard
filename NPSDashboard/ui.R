library(shiny)
library(flexdashboard)
library(scales)
library(plotly)

load("npsData.Rdata")

shinyUI(
  fluidPage(
    div(class="col-xs-12 text-center bg-primary",
        h1("Net Promoter Score Dashboard")
        ),
    div(class="col-xs-12 text-center",
          h2("Overall Company Score"),
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
        h2("How has the overall company NPS score changed over time?"),
        plotlyOutput("companyScoreOverTime"),
        plotOutput("companyRatingsOverTime"),
        h2("How many times has each score has been given?"),
        plotOutput("companyScoreDistribution"),
        h2("Are the company and production scores related?"),
        p("This shows the combination of ratings. The size of each dot counts how many times 
          that combination of scores was given."),
        plotOutput("companyProductionCorrelation"),
        hr(),
        h1("Production NPS Scores"),
        uiOutput("prodPlots")
       ),
  title = "NPS Dashboard",
  theme = "style.css"
  )
)
