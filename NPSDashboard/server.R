library(shiny)
library(tidyverse)
library(flexdashboard)
library(plotly)

load("npsData.RData")

shinyServer(function(input, output, session) {
   
  output$companyScoreGauge <- renderGauge({
    
    score <- round(companyScore$npsScore)
    
    gauge(value = score, min = -100, max = 100, symbol = '', gaugeSectors(
      success = c(50, 100),
      warning = c(0, 50),
      danger = c(-100, 0)
    ))
    
  })
  
  output$companyScoreOverTime <- renderPlotly({
    plot <- ggplot(
      nps_company, 
      aes(
        x=create_dt, 
        y=cumulativeScore, 
        group=created_by, 
        text=paste("Score: ", round(cumulativeScore, 1)
                   )
        )
      )+
      geom_line(size=1.5, col="steelblue")+
      ylab("Net Promoter Score")+
      xlab("Date")+
      theme_minimal()+
      theme(axis.text = element_text(size=14))
    
    ggplotly(plot, tooltip=c("text"))
  })
  
  output$companyScoreDistribution <- renderPlot({
    ggplot(nps_company, aes(nps_company_score))+
      geom_histogram(binwidth = .5, color="steelblue", fill="white")+
      scale_x_continuous(breaks = c(0:10))+
      ylab("Number of Scores")+
      xlab("Score")+
      theme_minimal()+
      theme(axis.text = element_text(size=14))
  })
  
  output$companyProductionCorrelation <- renderPlot({
    ggplot(allScores, aes(nps_company_score, nps_prod_score))+
      geom_count(col="steelblue")+
      scale_size_area(max_size = 20)+
      scale_x_continuous(breaks = c(0:10))+
      scale_y_continuous(breaks = c(0:10))+
      ylab("Production Score")+
      xlab("Company Score")+
      geom_smooth(method="lm")+
      theme_minimal()+
      theme(axis.text = element_text(size=14))
  })
  
  output$prodPlots <- renderUI({
    plotOuputList <- lapply(productionScores$prodSeason, function(prod){
      print(paste0("Calling ", prod))
      plotname <- prod
      gaugeOutput(plotname)
    })
    
    do.call(tagList, plotOuputList)
  })
  
  for(prod in factor(productionScores$prodSeason)) {
    local({
      plotname <- prod
      
      output[[plotname]] <- renderGauge({
        score <- productionScores %>% 
          filter(prodSeason == prod) %>% 
          mutate(npsScore = round(npsScore))
        
        gauge(value = score$npsScore, min = -100, max = 100, symbol = '', gaugeSectors(
          success = c(50, 100),
          warning = c(0, 50),
          danger = c(-100, 0)
        ))
      })
    })
  }
  
})
