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
        h2("How has the overall NPS score changed over time?"),
        plotlyOutput("companyScoreOverTime"),
        h2("How many times has each score has been given?"),
        plotOutput("companyScoreDistribution"),
        h2("Does a particular production affect the company score?"),
        p("Or, in other words, are company scores correlated to production scores?"),
        p("The size of each dot counts how many times
          that combination of scores was given. The line shows the correlation between the scores. The stronger the slope,
          the more we can say the two scores are correlated."),
        p("In this case, ",
          strong(percent(cor(allScores$nps_company_score, allScores$nps_prod_score))),
          " of the company score can be explained by the production score"
          ),
        plotOutput("companyProductionCorrelation"),
        hr(),
        uiOutput("prodPlots")
       ),
  title = "NPS Dashboard",
  theme = "style.css"
  )
)
